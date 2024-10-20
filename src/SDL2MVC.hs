{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module SDL2MVC
  ( main
  ) where

import           Control.Concurrent.Async (mapConcurrently_)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as Queue
import           Control.Lens
import           Data.Semigroup (Any(..))
import           Data.Text (Text)
import           Diagrams
import           Diagrams.Backend.Cairo
import           Effectful
import qualified SDL

--------------------------------------------------------------------------------





type View action = QDiagram Cairo V2 Double Any

data Reaction effs model action = Reaction model [Eff effs action]

data AppConfig effs model action =
     AppConfig { _render             :: model -> View action
                                     -- ^ the view function
               , _appUpdate          :: model -> action -> Reaction effs model action
               , _startupAction      :: action
                                     -- ^ action to run on startup
               , _interpretSDLEvent  :: SDL.Event -> action
                                     -- ^ How to interpet SDL events
               }

makeLenses ''AppConfig

data App model action =
     App { _appData         :: model
         , _window          :: SDL.Window
         , _renderer        :: SDL.Renderer
         -- , _texture         :: SDL.Texture
         , _eventQueue      :: Queue.TBQueue action
          -- ^ the event queue we are using
         }

makeLenses ''App


data AppModel effs model action =
     AppModel { _initialWindowTitle :: Text
              , _theModel           :: model
              , _appConfig          :: AppConfig effs model action
              }

-- makeClassy ''AppModel


data AppAction action = Skip
                      -- | Quit
                      -- | Render
                      | SDLEvent SDL.Event
                      | Custom action
                      deriving (Eq, Functor, Foldable, Traversable)

-- handleAppAction       :: model -> AppAction action -> Reaction effs model action
-- handleAppAction model = \case
--   Skip -> Reaction model [] -- do nothing
--   Quit -> Reaction model [ do SDL.destroyWindow (model^.window) ]




--------------------------------------------------------------------------------


-- | Initialize the app
initializeSDLApp       :: model -- ^ initial model
                       -> IO (App model)
initializeSDLApp model = do
  SDL.initializeAll
  let windowCfg = SDL.defaultWindow
  window'   <- SDL.createWindow "foo" windowCfg
  renderer' <- SDL.createRenderer window' (-1) SDL.defaultRenderer
  -- texture'  <- SDL.Cairo.createCairoTexture' renderer' window'

  -- initialize the event queue
  queue  <- atomically $ do q <- Queue.newTBQueue maxQueueSize
                            mapM_ (Queue.writeTBQueue q) initialActions
                            pure q

  pure $ App { _appData    = model
             , _window     = window'
             , _renderer   = renderer'
             , _eventQueue = queue'
             }


defaultAppConfig :: AppConfig effs model (AppAction action)
defaultAppConfig = AppConfig { _render            = renderView
                             , _appUpdate         = defaultUpdate
                             , _startupAction     = Skip
                             , _interpretSDLEvent = SDLEvent
                             }


defaultUpdate       :: model -> AppAction action -> Reaction effs model (AppAction action)
defaultUpdate model = undefined


data LoopAction action = Shutdown
                       | Continue action



-- | Runs the app
runApp     :: App model -> IO ()
runApp app = go (app^.model)
  where
    queue = app^.eventQueue

    -- | The main app loop; we read the next event from the queue, and
    -- then handle it. In turn, this may again trigger further event
    -- handling.
    go     :: model -> IO ()
    go m = do e <- atomically $ Queue.readTBQueue queue
              handle m e

    handle       :: model -> LoopAction action -> IO ()
    handle model = \case
      Shutdown     -> destroyWindow (model^.window) -- destory the window, and then quit
      Continue act -> let Reaction model' effs = (app^.appUpdate) act
                      in do runAll effs -- runs the effects, which may produce more actions
                            go model'  -- continue to thandle then ext event.

    -- | Runs a bunch of IO actions that may schedule more actions
    runAll :: [Eff

      IO (Action action model)] -> IO ()
    runAll = mapConcurrently_ (>>= atomically . Queue.writeTBQueue queue)






  do
  SDL.showWindow (app^.window)
  print "woei"







main :: IO ()
main = do
  app <- initializeSDLApp $ AppModel { _initialWindowTitle = "SDL2MVC App "
                                     , _theModel           = ()
                                     , _appConfig          = defaultAppConfig
                                     }
  runApp app




--------------------------------------------------------------------------------

renderView :: model -> View (AppAction action)
renderView = undefined
