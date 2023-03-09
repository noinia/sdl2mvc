{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  SDL2MVC.SDLApp
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- An SDLApp,
--
--------------------------------------------------------------------------------
module SDL2MVC.SDLApp
  ( SDLApp
  , AppConfig(..)

  , runApp

  , runApp', initializeSDLApp

  , Action
  , AppAction(.., AppAction')
  , SDLAction(..)
  ) where

import           Control.Concurrent.Async (mapConcurrently_)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as Queue
import           Control.Lens
import           Control.Monad (when)
import           Data.Bifunctor
import           GHC.Natural
import           SDL
import           SDL2MVC.Effect
-- import           SDL2MVC.PublicModel (PublicModel, theModel, title)
-- import qualified SDL2MVC.PublicModel as PublicModel
import qualified SDL2MVC.UIState as UIState
import           SDL2MVC.View
import           Data.Text (Text)

--------------------------------------------------------------------------------

-- 'action' will always be the user specific action type.

-- | The main App actions, including quitting and doing nothing.
data AppAction action = Skip
                      | Quit
                      | AppAction action
                      deriving (Eq,Functor,Foldable,Traversable)

-- | SDL Actions, including "internal" ones
data SDLAction action model =
    UIStateAction (UIState.Action (Action action model))
  | WaitSDLEvent
  | AppSpecificAction action
  deriving (Eq)

pattern AppAction'   :: action -> AppAction (SDLAction action model)
pattern AppAction' a = AppAction (AppSpecificAction a)


-- instance Bifunctor SDLAction where
--   bimap f g = \case
--     PublicModelAction a -> PublicModelAction <$> bimap f g a
--     AppSpecificAction a -> AppSpecificAction <$> f a

-- instance Bitraversable SDLAction where
--   bitraverse f g = \case
--     PublicModelAction a -> PublicModelAction <$> bitraverse f g a
--     AppSpecificAction a -> AppSpecificAction <$> f a


-- | The actual Action type we expose.
type Action action model = AppAction (SDLAction action model)

-- -- | Our public model
-- type PublicModel' action model = PublicModel (Action action model) model

--------------------------------------------------------------------------------


data AppConfig action model =
  AppConfig { _update             :: model -> action -> Effect (Action action model) model
                                  -- ^ the update function
            , _render             :: model -> View (Action action model)
                                  -- ^ the view function
            , _startupAction      :: Action action model
                                  -- ^ action to run on startup
            , _interpretSDLEvent  :: SDL.Event -> action
                                  -- ^ How to interpet SDL events
            , _initialWindowTitle :: Text
            }
makeLenses ''AppConfig
-- we pass the initial model separately, otherwise this may leak space


--------------------------------------------------------------------------------
-- * Model

maxQueueSize :: Natural
maxQueueSize = 1000


-- | The complete SDL App, storing the app model, as well as
-- "internal" data that the framework itself needs.
data SDLApp action model =
  SDLApp { _model             :: model
                              -- ^ the public part of the model,
         , _appConfig         :: AppConfig action model
                             -- ^ the app configuration
         , _uiState           :: UIState.UIState (Action action model)
                              -- ^ internal state of the SDL App
         , _eventQueue        :: Queue.TBQueue (Action action model)
                              -- ^ the event queue we are using
         }
makeLenses ''SDLApp


-- | Initialize the app
initializeSDLApp          :: model -- ^ initial model
                          -> AppConfig action model
                          -> IO (SDLApp action model)
initializeSDLApp m appCfg = do
    initializeAll
    ui     <- UIState.initializeUIState $ appCfg^.initialWindowTitle
    let initialActions = [ appCfg^.startupAction
                         , AppAction WaitSDLEvent
                         , AppAction . UIStateAction . UIState.Redraw $ (appCfg^.render) m
                         ]
    queue  <- atomically $ do q <- Queue.newTBQueue maxQueueSize
                              mapM_ (Queue.writeTBQueue q) initialActions
                              pure q

    pure $ SDLApp { _model      = m
                  , _appConfig  = appCfg
                  , _uiState    = ui
                  , _eventQueue = queue
                  }


-- | Helper type that we use when running te events
data SDLAppModel action model =
  SDLAppModel { _theUIState :: UIState.UIState (Action action model)
              , _theModel   :: model
              } deriving stock (Eq)
makeLenses  ''SDLAppModel

--------------------------------------------------------------------------------
-- * Controller

-- | Initializes and runs the app
runApp           :: (Eq model, Eq action)
                 => model
                 -- ^ Initial model
                 -> AppConfig action model
                 -- ^ App config
                 -> IO ()
runApp m0 appCfg = initializeSDLApp m0 appCfg >>= runApp'

-- | Runs the app
runApp'     :: forall action model. (Eq model, Eq action)
            => SDLApp action model -> IO ()
runApp' app = go $ SDLAppModel (app^.uiState) (app^.model)
  where
    queue = app^.eventQueue

    -- | The main app loop; we read the next event from the queue, and
    -- then handle it. In turn, this may again trigger further event
    -- handling.
    go    :: SDLAppModel action model -> IO ()
    go m0 = do e <- atomically $ Queue.readTBQueue queue
               handle m0 e

    -- | How to handle a particular event.
    handle   :: SDLAppModel action model -> Action action model -> IO ()
    handle m = \case
      Skip                -> go m -- just continue handling events.
      Quit                -> pure ()
        -- Stop handling events; the rest of the events in the queue are not handled.
      AppAction a         -> do let Effect m' acts = update' m a
                                -- when the model has changed we may need to redraw things
                                when (m' /= m) (scheduleAll [redrawAct $ m'^.theModel ])
                                -- also schedule all other actions
                                runAll acts
                                go m' -- continue handling events.


    -- | schedules a bunch of actions
    scheduleAll :: [Action action model] -> IO ()
    scheduleAll = mapConcurrently_ (atomically . Queue.writeTBQueue queue)

    -- | Runs a bunch of IO actions that may schedule more actions
    runAll :: [IO (Action action model)] -> IO ()
    runAll = mapConcurrently_ (>>= atomically . Queue.writeTBQueue queue)

    -- | The update function
    update'   :: SDLAppModel action model
              -> SDLAction action model
              -> Effect (Action action model) (SDLAppModel action model)
    update' m = \case
      UIStateAction a     -> first (maybe Skip (AppAction . UIStateAction))
                           $ m&theUIState %%~ flip UIState.update a
      -- PublicModelAction a  -> first (const Skip) $ PublicModel.update m a
      WaitSDLEvent         -> m <# do e <- waitEvent
                                      scheduleAll [interpret e]
                                      pure $ AppAction WaitSDLEvent
      AppSpecificAction a  -> m&theModel %%~ flip (app^.appConfig.update) a

    -- | Interprets an sdl event
    interpret = AppAction . AppSpecificAction . (app^.appConfig.interpretSDLEvent)

    redrawAct :: model -> Action action model
    redrawAct =
      AppAction . UIStateAction . UIState.Redraw . (app^.appConfig.render)

----------------------------------------


--------------------------------------------------------------------------------
-- * View


--------------------------------------------------------------------------------
-- * Main



--------------------------------------------------------------------------------
