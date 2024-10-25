{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module SDL2MVC
  ( main
  ) where

import           Control.Concurrent.Async (mapConcurrently_, concurrently, withAsync, uninterruptibleCancel)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as Queue
import           Control.Lens
import           Data.Semigroup (Any(..))
import           Data.Text (Text)
import           Diagrams
import           Diagrams.Backend.Cairo
import           Effectful
import           GHC.Natural
import qualified SDL

--------------------------------------------------------------------------------

data Reaction m model action = Reaction model [m action]


data LoopAction action = Shutdown
                       | Continue action

type Handler m model action = model -> action -> Reaction m model (LoopAction action)


noEff       :: model -> Reaction m model action
noEff model = Reaction model []


data AppConfig m model action =
  AppConfig { _appModel      :: model
            , _handler       :: Handler m model action
            , _startupAction :: action
            , _liftSDLEvent  :: SDL.Event -> LoopAction action
            }

makeLenses ''AppConfig

data App m model action =
     App { _config          :: AppConfig m model action
         , _windowRef       :: SDL.Window
         , _rendererRef     :: SDL.Renderer
         -- , _texture         :: SDL.Texture
         , _eventQueue      :: Queue.TBQueue (LoopAction action)
          -- ^ the event queue we are using
         }

makeLenses ''App

--------------------------------------------------------------------------------


maxQueueSize :: Natural
maxQueueSize = 1000


--------------------------------------------------------------------------------



-- | Initialize the app
initializeSDLApp        :: AppConfig m model action -> IO (App m model action)
initializeSDLApp appCfg = do
  SDL.initializeAll
  let windowCfg = SDL.defaultWindow
  window'   <- SDL.createWindow "foo" windowCfg
  renderer' <- SDL.createRenderer window' (-1) SDL.defaultRenderer
  -- texture'  <- SDL.Cairo.createCairoTexture' renderer' window'


  -- initialize the event queue
  let initialActions = [ Continue $ appCfg^.startupAction
                       -- , AppAction WaitSDLEvent
                       -- , AppAction . UIStateAction . UIState.Redraw $ (appCfg^.render) m
                       ]
  queue  <- atomically $ do q <- Queue.newTBQueue maxQueueSize
                            mapM_ (Queue.writeTBQueue q) initialActions
                            pure q

  pure $ App { _config      = appCfg
             , _windowRef   = window'
             , _rendererRef = renderer'
             , _eventQueue  = queue
             }


-- | Handles some of the default events, in particular closing the window and quitting
withDefaultSDLEvents :: (SDL.Event -> action) -> SDL.Event -> LoopAction action
withDefaultSDLEvents handle e = case SDL.eventPayload e of
  SDL.WindowClosedEvent _ -> Shutdown
  SDL.QuitEvent           -> Shutdown
  _                       -> Continue $ handle e



-- | Runs the app
runApp     :: forall model action. App IO model action -> IO ()
runApp app = startup (app^.config.appModel)
  where
    queue = app^.eventQueue

    -- start listening for sdl events (pushing them into our own event queue), and
    -- start our handler
    startup model = withAsync awaitSDLEvent $ \sdlWaiter -> do
                         go model
                         uninterruptibleCancel sdlWaiter

    -- | indefinitely wait for SDL events, pushing them into our own event Queue
    awaitSDLEvent = do e <- SDL.waitEvent
                       atomically $ Queue.writeTBQueue queue (app^.config.liftSDLEvent $ e)
                       awaitSDLEvent

    -- | The main app loop; we read the next event from the queue, and
    -- then handle it. In turn, this may again trigger further event
    -- handling.
    go     :: model -> IO ()
    go m = do e <- atomically $ Queue.readTBQueue queue
              handle m e

    handle       :: model -> LoopAction action -> IO ()
    handle model = \case
      Shutdown     -> SDL.destroyWindow (app^.windowRef) -- destroy the window, and then quit
      Continue act -> let Reaction model' effs = (app^.config.handler) model act
                      in do runAll effs -- runs the effects, which may produce more actions
                            go model'  -- continue to thandle then ext event.

    runAll :: [IO (LoopAction action)] -> IO ()
    runAll = mapConcurrently_ (>>= atomically . Queue.writeTBQueue queue)


main :: IO ()
main = do
  app <- initializeSDLApp $ AppConfig { _appModel      = ()
                                      , _handler       = \model _ -> noEff model
                                      , _startupAction = Nothing
                                      , _liftSDLEvent  = withDefaultSDLEvents Just
                                      }
  runApp app
