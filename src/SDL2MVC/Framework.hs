{-# LANGUAGE OverloadedStrings #-}
module SDL2MVC.Framework
  ( runApp
  , withSDLApp
  , runApp'
  , withDefaultSDLEvents
  ) where


import           Control.Concurrent.Async (mapConcurrently_, withAsync, uninterruptibleCancel)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as Queue
import           Control.Exception (bracket)
import           Control.Lens
import           Control.Monad.Managed
import           GHC.Natural
import qualified SDL
import           SDL2MVC.App
import           SDL2MVC.Cairo
import           SDL2MVC.Reaction

import           Debug.Trace
--------------------------------------------------------------------------------

-- | maximum queue size
maxQueueSize :: Natural
maxQueueSize = 1000

--------------------------------------------------------------------------------

-- | Main entrypoint. Runs an SDL2MV app given by the appConfig
runApp :: Show action => AppConfig IO model action -> IO ()
runApp = flip withSDLApp runApp'

--------------------------------------------------------------------------------

-- | Initialize the app
withSDLApp          :: AppConfig m model action
                    -- ^ The configuration describing how to set up our application
                    -> (App m model action -> IO ())
                    -- ^ the continuation; i.e. the actual application
                    -> IO ()
withSDLApp appCfg k = do
  SDL.initializeAll
  let windowCfg   = SDL.defaultWindow
      rendererCfg = SDL.defaultRenderer { SDL.rendererType = SDL.SoftwareRenderer
                                        }
                    -- to use the texture we need to use the softwarerenderer
  -- Initialize the window, renderer, and texture that we draw on
  runManaged $ do
    window'   <- managed $ bracket (SDL.createWindow (appCfg^.windowTitle) windowCfg)
                                   SDL.destroyWindow
    renderer' <- managed $ bracket (SDL.createRenderer window' (-1) rendererCfg)
                                   SDL.destroyRenderer
    texture'  <- managed $ bracket (createCairoTexture' renderer' window')
                                   SDL.destroyTexture
    -- initialize the event queue
    let initialActions = [ Continue $ appCfg^.startupAction
                         ]
    queue  <- liftIO . atomically $ do q <- Queue.newTBQueue maxQueueSize
                                       mapM_ (Queue.writeTBQueue q) initialActions
                                       pure q

    -- run the actuall application
    liftIO . k $ App { _config      = appCfg
                     , _windowRef   = window'
                     , _rendererRef = renderer'
                     , _textureRef  = texture'
                     , _eventQueue  = queue
                     }


-- | Handles some of the default events, in particular closing the window and quitting
withDefaultSDLEvents :: (SDL.Event -> action) -> SDL.Event -> LoopAction action
withDefaultSDLEvents handle e = case SDL.eventPayload e of
  SDL.WindowClosedEvent _ -> Shutdown
  SDL.QuitEvent           -> Shutdown
  _                       -> Continue $ handle e


-- | Interleave the two lists
interleaved       :: [a] -> [a] -> [a]
interleaved xs ys = case xs of
  []    -> ys
  x:xs' -> case ys of
    []    -> xs
    y:ys' -> x:y:interleaved xs' ys'


-- | Runs the app
runApp'     :: forall model action.
               Show action =>
              App IO model action -> IO ()
runApp' app = go (app^.config.appModel)
  where
    queue        = app^.eventQueue
    handleAction = app^.config.handler

    -- | The main app loop; we essentially get the current sdl events and the current
    -- app events, and handle them in an interlaving manner. When we've handled
    -- all events, we continue the loop.
    --
    -- we interleave the events to prevent starvation.
    go   :: model -> IO ()
    go m = do sdlEvents <- fmap (app^.config.liftSDLEvent) <$> SDL.pollEvents
              appEvents <- atomically $ Queue.flushTBQueue queue
              handleAll m $ interleaved sdlEvents appEvents
    -- | handleAll does the actual event handling, whereas the go function collects the
    -- events to run.
    handleAll model = \case
      []     -> go model
      e:evts -> case e of
        Shutdown     -> pure ()
        Continue act -> case handleAction app model act of
          Reaction model' effs -> do scheduleAll effs  -- schedules additional actions
                                     handleAll model' evts

    scheduleAll :: [IO (LoopAction action)] -> IO ()
    scheduleAll = mapConcurrently_ (>>= atomically . Queue.writeTBQueue queue)
