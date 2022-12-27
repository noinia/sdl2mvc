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
  , AppAction(..)
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
import           SDL2MVC.PublicModel (PublicModel, theModel, title)
import qualified SDL2MVC.PublicModel as PublicModel
import           SDL2MVC.View

--------------------------------------------------------------------------------




-- 'action' will always be the user specific action type.

-- | The main App actions, including quitting and doing nothing.
data AppAction action = Skip
                      | Quit
                      | AppAction action
                      deriving (Functor,Foldable,Traversable)

-- | SDL Actions, including "internal" ones
data SDLAction action model =
    PublicModelAction (PublicModel.Action (Action action model) model)
  | WaitSDLEvent
  | AppSpecificAction action

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

-- | Our public model
type PublicModel' action model = PublicModel (Action action model) model

--------------------------------------------------------------------------------


data AppConfig action model =
  AppConfig { _update            :: model -> action -> Effect (Action action model) model
                                 -- ^ the update function
            , _render            :: model -> View (Action action model)
                                 -- ^ the view function
            , _startupAction     :: action
                                 -- ^ action to run on startup
            , _interpretSDLEvent :: SDL.Event -> action
                                 -- ^ How to interpet SDL events
            }
makeLenses ''AppConfig
-- we pass the initial model separately, otherwise this may leak space


--------------------------------------------------------------------------------
-- * Model

maxQueueSize :: Natural
maxQueueSize = 1000

-- | Internal state of the SDL app
data InternalState action' =
  InternalState { _renderer   :: Renderer
                                 -- ^ Renderer to use to actually draw things
                , _eventQueue :: Queue.TBQueue action'
                                 -- ^ event queue we are using
                }
makeLenses ''InternalState

-- | initialze the internal state
initializeState           :: Window
                          -> [action'] -- ^ initial actions
                          -> IO (InternalState action')
initializeState window as = do
  renderer' <- createRenderer window (-1) defaultRenderer
  queue     <- atomically $ do q <- Queue.newTBQueue maxQueueSize
                               mapM_ (Queue.writeTBQueue q) as
                               pure q
  pure $ InternalState { _renderer   = renderer'
                       , _eventQueue = queue
                       }


-- | The complete SDL App, storing the app model, as well as
-- "internal" data that the framework itself needs.
data SDLApp action model =
  SDLApp { _appModel          :: PublicModel' action model
                              -- ^ the public part of the model,
         , _internalState     :: InternalState (Action action model)
                              -- ^ internal state of the SDL App
         , _appConfig        :: AppConfig action model
                             -- ^ the app configuration
         }
makeLenses ''SDLApp

-- | Initialize the app
initializeSDLApp :: model -- ^ initial model
                 -> AppConfig action model
                 -> IO (SDLApp action model)
initializeSDLApp m appCfg = do
    initializeAll
    window <- createWindow (pm^.title) defaultWindow
    state  <- initializeState window [ AppAction $ AppSpecificAction (appCfg^.startupAction)
                                     , AppAction WaitSDLEvent
                                     ]
    pure $ SDLApp { _appModel      = pm
                  , _internalState = state
                  , _appConfig     = appCfg
                  }
  where
    pm = PublicModel.createModel m

--------------------------------------------------------------------------------
-- * Controller

-- | Initializes and runs the app
runApp           :: Eq model
                 => model
                 -- ^ Initial model
                 -> AppConfig action model
                 -- ^ App config
                 -> IO ()
runApp m0 appCfg = initializeSDLApp m0 appCfg >>= runApp'

-- | Runs the app
runApp'     :: forall action model. Eq model
            => SDLApp action model -> IO ()
runApp' app = go (app^.appModel)
  where
    queue = app^.internalState.eventQueue

    -- | The main app loop; we read the next event from the queue, and
    -- then handle it. In turn, this may again trigger further event
    -- handling.
    go    :: PublicModel' action model -> IO ()
    go m0 = do e <- atomically $ Queue.readTBQueue queue
               handle m0 e

    -- | How to handle a particular event.
    handle   :: PublicModel' action model -> Action action model -> IO ()
    handle m = \case
      Skip                -> go m -- just continue handling events.
      Quit                -> pure ()
        -- Stop handling events; the rest of the events in the queue are not handled.
      AppAction a         -> do let Effect m' acts = update' m a
                                -- when the model has changed we may need to redraw things
                                when (m' /= m) (scheduleAll [redrawAct])
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
    update'   :: PublicModel' action model -> SDLAction action model
              -> Effect (Action action model) (PublicModel' action model)
    update' m = \case
      PublicModelAction a  -> first (const Skip) $ PublicModel.update m a
      WaitSDLEvent         -> m <# do e <- waitEvent
                                      scheduleAll [interpret e]
                                      pure $ AppAction WaitSDLEvent
      -- PollSDLEvents        -> m <# do events <- pollEvents
      --                                 scheduleAll (interpret <$> events)
      --                                 pure $ AppAction PollSDLEvents
      AppSpecificAction a  -> m&theModel %%~ flip (app^.appConfig.update) a

    -- | Interprets an sdl event
    interpret = AppAction . AppSpecificAction . (app^.appConfig.interpretSDLEvent)

    redrawAct :: Action action model
    redrawAct = AppAction . PublicModelAction
              $ PublicModel.Redraw (app^.internalState.renderer)
                                   (app^.appConfig.render)


----------------------------------------


--------------------------------------------------------------------------------
-- * View


--------------------------------------------------------------------------------
-- * Main



--------------------------------------------------------------------------------
