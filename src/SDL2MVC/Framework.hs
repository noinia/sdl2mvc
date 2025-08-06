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
import           GHC.Natural
import qualified SDL
import           SDL2MVC.App
import           SDL2MVC.Cairo
import           SDL2MVC.Reaction

import           Debug.Trace
--------------------------------------------------------------------------------

maxQueueSize :: Natural
maxQueueSize = 1000

--------------------------------------------------------------------------------

-- | Main entrypoint. Runs an SDL2MV app given by the appConfig
runApp :: Show action => AppConfig IO model action -> IO ()
runApp = flip withSDLApp runApp'

--------------------------------------------------------------------------------

-- | Initialize the app
withSDLApp          :: AppConfig m model action
                    -> (App m model action -> IO res)
                    -- ^ the continuation; i.e. some computation to perform with the actual App.
                    -> IO res
withSDLApp appCfg k = do
  SDL.initializeAll
  let windowCfg   = SDL.defaultWindow
      rendererCfg = SDL.defaultRenderer { SDL.rendererType = SDL.SoftwareRenderer
                                        }
  bracket (SDL.createWindow "SDL2MVC App" windowCfg)
          SDL.destroyWindow                            $ \window'   -> do
    bracket (SDL.createRenderer window' (-1) rendererCfg)
            SDL.destroyRenderer                        $ \renderer' -> do
      bracket (createCairoTexture' renderer' window')
              SDL.destroyTexture                       $ \texture'  -> do
        -- initialize the event queue
        let initialActions = [ Continue . (appCfg^.liftRenderEvent) $ Render
                             , Continue $ appCfg^.startupAction
                             -- , AppAction WaitSDLEvent
                             -- , AppAction . UIStateAction . UIState.Redraw $ (appCfg^.render) m
                             ]
        queue  <- atomically $ do q <- Queue.newTBQueue maxQueueSize
                                  mapM_ (Queue.writeTBQueue q) initialActions
                                  pure q

        -- run the actuall application
        k $ App { _config      = appCfg
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

    -- -- start listening for sdl events (pushing them into our own event queue), and
    -- -- start our handler
    -- startup model = do
    --                   (app^.config.liftSDLEvent $ e)
    --   pollEvents




    --   withAsync awaitSDLEvent $ \sdlWaiter -> do
    --                      !_ <- go model
    --                      uninterruptibleCancel sdlWaiter

    -- -- | indefinitely wait for SDL events, pushing them into our own event Queue
    -- awaitSDLEvent = do e <- SDL.waitEvent
    --                    atomically $ Queue.writeTBQueue queue (app^.config.liftSDLEvent $ e)
    --                    awaitSDLEvent

    -- | The main app loop; we read the next event from the queue, and
    -- then handle it. In turn, this may again trigger further event
    -- handling.
    go   :: model -> IO ()
    go m = do sdlEvents <- fmap (app^.config.liftSDLEvent) <$> SDL.pollEvents
              appEvents <- atomically $ Queue.flushTBQueue queue
              handleAll m $ interleaved sdlEvents appEvents

    handleAll model = \case
      []     -> go model
      e:evts -> case e of
        Shutdown     -> putStrLn "shutting down"
        Continue act -> case traceShow ("handling",act) $ handleAction app model act of
          Reaction model' effs -> do scheduleAll effs  -- schedules additional actions
                                     handleAll model' evts

    --           -- e    <- atomically $ do
    --           --           l <- Queue.lengthTBQueue queue
    --           --           let (toSchedule,rest) = List.genericSplitAt (maxQueueSize - l) evts
    --           --           for_ toSchedule $ Queue.writeTBQueue queue
    --           --           e <- Queue.readTBQueue queue
    --           --           case rest of
    --           --             []                -> pure ()
    --           --             (f:droppedEvents) -> do Queue.writeTBQueue queue f
    --           --                                     when (not $ null droppedEvents) $
    --           --                                       putStrLn "WARNING: Dropped Eevnts!"
    --           --           pure e
    --           -- handle m e

    -- handle        :: model -> LoopAction action -> IO model
    -- handle !model = \case
    --   Shutdown     -> do putStrLn "shutting down"
    --                      pure model
    --   Continue act ->

    scheduleAll :: [IO (LoopAction action)] -> IO ()
    scheduleAll = mapConcurrently_ (>>= atomically . Queue.writeTBQueue queue)
