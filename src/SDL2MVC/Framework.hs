{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
import           Control.Monad (when)
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
import           SDL2MVC.Render
import           SDL2MVC.Send
import           SDL2MVC.Updated
import qualified Vary
import           Vary ((:|))
import qualified Vary.Utils as Vary

import           Debug.Trace
--------------------------------------------------------------------------------

-- | maximum queue size
maxQueueSize :: Natural
maxQueueSize = 1000

--------------------------------------------------------------------------------

-- | Main entrypoint. Runs an SDL2MV app given by the appConfig
runApp :: forall es os msgs inMsgs msgs' model.
          ( es ~ Send msgs : Concurrent : os
          , IOE :> os

          , msgs   ~ (Shutdown : inMsgs)
          , inMsgs ~ (Render : SDL.Event : msgs')
          )
       => AppConfig es model msgs inMsgs
       -> Eff os ()
runApp = flip withSDLApp runApp'
  -- where
  --   runApp'' :: App es model msgs inMsgs -> Eff (Concurrent : os) ()
  --   runApp'' = runApp


--------------------------------------------------------------------------------

-- | Initialize the app
withSDLApp          :: forall es msg os model inMsgs.
                       ( es ~ Send msg : Concurrent : os
                       , IOE :> os
                       )
                    => AppConfig es model msg inMsgs
                    -- ^ The configuration describing how to set up our application
                    -> (App es model msg inMsgs -> Eff (Concurrent : os) ())
                    -- ^ the continuation; i.e. the actual application
                    -> Eff os ()
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
    queue  <- atomically $ do q <- newTBQueue maxQueueSize
                              mapM_ (writeTBQueue q) (appCfg^.initialMessages)
                              pure q


    let withApp' :: App es model msg inMsgs -> Eff (Resource : Concurrent : os) ()
        withApp' = inject . withApp

    -- run the actuall application
    withApp' $ App { _config      = appCfg
                   , _windowRef   = window'
                   , _rendererRef = renderer'
                   , _textureRef  = texture'
                   , _eventQueue  = queue
                   }


-- | Handles some of the default events, in particular closing the window and quitting
withDefaultSDLEvents          :: forall msgs inMsgs es model.
                                 ( Send msgs :> es
                                 , Shutdown :| msgs
                                 , SDL.Event :| inMsgs
                                 )
                              => Handler es model msgs inMsgs
                              -> Handler es model msgs inMsgs
withDefaultSDLEvents handler' = \model msg -> case Vary.into @SDL.Event msg of
    Just e  -> case SDL.eventPayload e of
                 SDL.WindowClosedEvent _ -> Unchanged <$ sendMsg @msgs Shutdown
                 SDL.QuitEvent           -> Unchanged <$ sendMsg @msgs Shutdown
                 _                       -> handler' model msg
    Nothing -> handler' model msg

-- | Interleave the two lists
interleaved       :: [a] -> [a] -> [a]
interleaved xs ys = case xs of
  []    -> ys
  x:xs' -> case ys of
    []    -> xs
    y:ys' -> x:y:interleaved xs' ys'


-- | Runs the app
runApp'     :: forall es os model msgs inMsgs msgs'.
               ( es ~ Send msgs : Concurrent : os
                 -- Send msgs :> es
               -- , Subset os es

               , IOE        :> os
               -- , Concurrent :> os

               , msgs   ~ (Shutdown : inMsgs)
               , inMsgs ~ (Render : SDL.Event : msgs')
               )
            => App es model msgs inMsgs
            -> Eff (Concurrent : os) ()
runApp' app = runSendWith queue $ go (app^.config.appModel)
  where
    queue        = app^.eventQueue


    -- not the type msgs', sicne we've already handled shutdown
    handleAction           :: model -> Vary.Vary inMsgs -> Eff es (Updated model)
    handleAction model msg = (app^.config.handler) app model msg

    -- | The main app loop; we essentially get the current sdl events and the current
    -- app events, and handle them in an interlaving manner. When we've handled
    -- all events, we continue the loop.
    --
    -- we interleave the events to prevent starvation.
    go   :: model -> Eff es ()
    go m = do sdlEvents' <- liftIO SDL.pollEvents
              when (not $ null sdlEvents') $ liftIO (print sdlEvents')
              let sdlEvents = fmap Vary.from $ sdlEvents'
              appEvents <- atomically $ flushTBQueue queue
              handleAll m $ interleaved sdlEvents appEvents
    -- | handleAll does the actual event handling, whereas the go function collects the
    -- events to run.
    handleAll       :: model -> [Vary.Vary msgs] -> Eff es ()
    handleAll model = \case
      []     -> go model
      e:evts -> case Vary.pop e of
        Right Shutdown -> pure ()
        Left msg       -> handleAction model msg >>= \case
          Unchanged      -> handleAll model  evts
          Changed model' -> do sendMsg @msgs Render
                               handleAll model' evts

-- type Send msg = Send (LoopAction msg) -- (RenderAction msg))

-- data RenderAction msg = Render
--                       | Act msg
--                       deriving (Show,Eq)


-- withReRendering     :: Handler m model msg
-- withReRendering app = \case
--   Unchanged ->

-- withRerendering :: App m model msg -> App m (Updated model) msg
-- withRerendering app = app
