{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module SDL2MVC.Framework
  ( runApp
  , withSDLApp
  , runApp'
  , withDefaultHandlers
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
runApp :: forall es os msgs msgs' model.
          ( es ~ Send' model msgs : Concurrent : os
          , IOE :> os
          , msgs ~ (SDL.Event : msgs')
          )
       => AppConfig es model msgs
       -> Eff os ()
runApp = flip withSDLApp runApp'
  -- where
  --   runApp'' :: App es model msgs inMsgs -> Eff (Concurrent : os) ()
  --   runApp'' = runApp


--------------------------------------------------------------------------------

-- | Initialize the app
withSDLApp          :: forall es msgs os model.
                       ( es ~ Send' model msgs : Concurrent : os
                       , IOE :> os
                       )
                    => AppConfig es model msgs
                    -- ^ The configuration describing how to set up our application
                    -> (App es model msgs -> Eff es ())
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


    let withApp' :: App es model msgs -> Eff (Send' model msgs : Resource : Concurrent : os) ()
        withApp' = inject . withApp
        withAppX = runSendWith queue . withApp'

    -- run the actuall application
    withAppX $ App { _config      = appCfg
                   , _windowRef   = window'
                   , _rendererRef = renderer'
                   , _textureRef  = texture'
                   , _eventQueue  = queue
                   }

-- | Runs the app
runApp'     :: forall es model msgs msgs'.
               ( Send' model msgs :> es
               , IOE :> es
               , Concurrent :> es

               --   es ~ Send' model msgs : os
               -- , IOE        :> os
               -- , Concurrent :> os
               , msgs ~ (SDL.Event : msgs')
               )
            => App es model msgs
            -> Eff es ()
runApp' app = go (app^.config.appModel)
  where
    queue        = app^.eventQueue

    -- note that we have already handled shutdown messages, so just Render : msgs is what is left
    handleAction           :: model -> Vary.Vary (Render : Animate model : msgs)
                           -> Eff es (Updated model)
    handleAction model msg = withDefaultHandlers ((app^.config.handler) app) app model msg

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
    handleAll       :: model -> [Vary.Vary (All model msgs)] -> Eff es ()
    handleAll model = \case
      []     -> go model
      e:evts -> case Vary.pop e of
        Right Shutdown -> pure ()
        Left msg       -> handleAction model msg >>= \case
          Unchanged      -> handleAll model  evts
          Changed model' -> do sendMsg @(All model msgs) Render
                               handleAll model' evts



--------------------------------------------------------------------------------

-- | Interleave the two lists
interleaved       :: [a] -> [a] -> [a]
interleaved xs ys = case xs of
  []    -> ys
  x:xs' -> case ys of
    []    -> xs
    y:ys' -> x:y:interleaved xs' ys'


-- type Send msg = Send (LoopAction msg) -- (RenderAction msg))

-- data RenderAction msg = Render
--                       | Act msg
--                       deriving (Show,Eq)


-- withReRendering     :: Handler m model msg
-- withReRendering app = \case
--   Unchanged ->

-- withRerendering :: App m model msg -> App m (Updated model) msg
-- withRerendering app = app

--------------------------------------------------------------------------------

-- | Handlers some default events already
withDefaultHandlers             :: forall msgs msgs' es model.
                                   ( Send' model msgs :> es
                                   , IOE        :> es
                                   , Concurrent :> es
                                   , msgs ~ (SDL.Event : msgs')
                                   )
                                   => Handler es model (All model msgs) msgs
                                   -> App  es model msgs
                                   -> Handler es model (All model msgs)
                                                       (Render : Animate model : msgs)
withDefaultHandlers controller app = handleRender rendererData
                                   $ handleAnimate
                                   $ withDefaultSDLEvents @(All model msgs) controller
  where
    rendererData = RendererData (app^.rendererRef) (app^.textureRef) (app^.config.appRender)


-- | Handles some of the default events, in particular closing the window and quitting
withDefaultSDLEvents          :: forall allMsgs inMsgs es model.
                                 ( Send allMsgs :> es
                                 , Shutdown :| allMsgs
                                 , SDL.Event :| inMsgs
                                 )
                              => Handler es model allMsgs inMsgs
                              -> Handler es model allMsgs inMsgs
withDefaultSDLEvents handler' = \model msg -> case Vary.into @SDL.Event msg of
    Just e  -> case SDL.eventPayload e of
                 SDL.WindowClosedEvent _ -> Unchanged <$ sendMsg @allMsgs Shutdown
                 SDL.QuitEvent           -> Unchanged <$ sendMsg @allMsgs Shutdown
                 _                       -> handler' model msg
    Nothing -> handler' model msg
