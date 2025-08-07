{-# LANGUAGE OverloadedStrings #-}
module SDL2MVC.Framework
  ( runApp
  , withSDLApp
  , runApp'
  , withDefaultSDLEvents
  ) where


import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TBQueue as Queue
import           Control.Exception (bracket)
import           Control.Lens
import           Control.Monad.Managed
import           Data.Maybe (maybeToList)
import           Effectful
import           Effectful.Concurrent
import           Effectful.Concurrent.Async (mapConcurrently_)
import           Effectful.Concurrent.STM
import           Effectful.Resource
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
runApp :: IOE :> es
       => AppConfig (Resource : Concurrent : es) model msg -> Eff es ()
runApp = flip withSDLApp runApp'

--------------------------------------------------------------------------------

-- | Initialize the app
withSDLApp          :: ( IOE :> es
                       )
                    => AppConfig (Resource : Concurrent : es) model msg
                    -- ^ The configuration describing how to set up our application
                    -> (App (Resource : Concurrent : es) model msg ->
                         Eff (Resource : Concurrent  : es) ())
                    -- ^ the continuation; i.e. the actual application
                    -> Eff es ()
withSDLApp appCfg withApp = do
  liftIO $ SDL.initializeAll
  let rendererCfg = SDL.defaultRenderer { SDL.rendererType = SDL.SoftwareRenderer
                                        }
                    -- to use the texture we need to use the softwarerenderer
  -- Initialize the window, renderer, and texture that we draw on
  runConcurrent . runResource $ do
    window'   <- manage (SDL.createWindow (appCfg^.windowTitle) (appCfg^.windowConfig))
                        SDL.destroyWindow
    renderer' <- manage (SDL.createRenderer window' (-1) rendererCfg)
                        SDL.destroyRenderer
    texture'  <- manage (createCairoTexture' renderer' window')
                        SDL.destroyTexture
    -- initialize the event queue
    let initialActions = maybeToList (Continue <$> appCfg^.startupAction)
    queue  <- atomically $ do q <- newTBQueue maxQueueSize
                              mapM_ (writeTBQueue q) initialActions
                              pure q

    -- run the actuall application
    withApp $ App { _config      = appCfg
                  , _windowRef   = window'
                  , _rendererRef = renderer'
                  , _textureRef  = texture'
                  , _eventQueue  = queue
                  }


-- | Handles some of the default events, in particular closing the window and quitting
withDefaultSDLEvents :: (SDL.Event -> msg) -> SDL.Event -> LoopAction msg
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
runApp'     :: forall os model msg.
               (IOE :> os, Concurrent :> os)
            => App os model msg -> Eff os ()
runApp' app = go (app^.config.appModel)
  where
    queue        = app^.eventQueue
    handleAction = app^.config.handler

    -- | The main app loop; we essentially get the current sdl events and the current
    -- app events, and handle them in an interlaving manner. When we've handled
    -- all events, we continue the loop.
    --
    -- we interleave the events to prevent starvation.
    go   :: model -> Eff os ()
    go m = do sdlEvents <- fmap (app^.config.liftSDLEvent) <$> liftIO SDL.pollEvents
              appEvents <- atomically $ flushTBQueue queue
              handleAll m $ interleaved sdlEvents appEvents
    -- | handleAll does the actual event handling, whereas the go function collects the
    -- events to run.
    handleAll       :: model -> [LoopAction msg] -> Eff os ()
    handleAll model = \case
      []     -> go model
      e:evts -> case e of
        Shutdown     -> pure ()
        Continue act -> case handleAction app model act of
          Reaction model' effs -> do scheduleAll effs  -- schedules additional msgs
                                     handleAll model' evts

    scheduleAll :: [Eff os (LoopAction msg)] -> Eff os ()
    scheduleAll = mapConcurrently_ (\act -> do msg <- act
                                               atomically . writeTBQueue queue $ msg
                                   )


data WithRenderAction msg = RenderAction Render
                          | Act msg
                          deriving (Show,Eq)


-- withReRendering     :: Handler m model msg
-- withReRendering app = \case
--   Unchanged ->

-- withRerendering :: App m model msg -> App m (Updated model) msg
-- withRerendering app = app
