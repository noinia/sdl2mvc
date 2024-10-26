{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module SDL2MVC
  ( main
  ) where

import           Control.Concurrent.Async (mapConcurrently_, concurrently, withAsync, uninterruptibleCancel)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as Queue
import           Control.Lens
import           Data.Colour.Names
import           Data.Semigroup (Any(..))
import           Data.Text (Text)
import           Diagrams hiding (Render)
import           Diagrams.Backend.Cairo
import           Effectful
import           GHC.Natural
import           Linear
import qualified SDL
import           SDL2MVC.Cairo

import           Debug.Trace

--------------------------------------------------------------------------------

data Reaction m model action = Reaction model [m action]

noEff       :: model -> Reaction m model action
noEff model = Reaction model []

infix <#

model <# act = Reaction model [act]



data LoopAction action = Shutdown
                       | Continue action

data Render = Render

type Handler m model action = model -> action -> Reaction m model (LoopAction action)




type View m action = SDL.Texture -> m action


data AppConfig m model action =
  AppConfig { _appModel        :: model
            , _handler         :: App m model action -> Handler m model action
            , _startupAction   :: action
            , _liftSDLEvent    :: SDL.Event -> LoopAction action
            , _liftRenderEvent :: Render -> action
            , _appRender       :: model -> View m action
            }


data App m model action =
     App { _config          :: AppConfig m model action
         , _windowRef       :: SDL.Window
         , _rendererRef     :: SDL.Renderer
         , _textureRef      :: SDL.Texture
         , _eventQueue      :: Queue.TBQueue (LoopAction action)
          -- ^ the event queue we are using
         }


makeLenses ''AppConfig


makeLenses ''App

--------------------------------------------------------------------------------


maxQueueSize :: Natural
maxQueueSize = 1000


--------------------------------------------------------------------------------



-- | Initialize the app
initializeSDLApp        :: AppConfig m model action -> IO (App m model action)
initializeSDLApp appCfg = do
  SDL.initializeAll
  let windowCfg   = SDL.defaultWindow
      rendererCfg = SDL.defaultRenderer { SDL.rendererType = SDL.SoftwareRenderer
                                        }
  window'   <- SDL.createWindow "foo" windowCfg
  renderer' <- SDL.createRenderer window' (-1) rendererCfg
  print "go"
  texture'  <- createCairoTexture' renderer' window'


  -- initialize the event queue
  let initialActions = [ Continue . (appCfg^.liftRenderEvent) $ Render
                       , Continue $ appCfg^.startupAction
                       -- , AppAction WaitSDLEvent
                       -- , AppAction . UIStateAction . UIState.Redraw $ (appCfg^.render) m
                       ]
  queue  <- atomically $ do q <- Queue.newTBQueue maxQueueSize
                            mapM_ (Queue.writeTBQueue q) initialActions
                            pure q

  pure $ App { _config      = appCfg
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
      Continue act -> let Reaction model' effs = (app^.config.handler) app model act
                      in do runAll effs -- runs the effects, which may produce more actions
                            go model'  -- continue to thandle then ext event.

    runAll :: [IO (LoopAction action)] -> IO ()
    runAll = mapConcurrently_ (>>= atomically . Queue.writeTBQueue queue)

--------------------------------------------------------------------------------

-- | Handles a render action
handleRender           :: MonadIO m
                       => App m model action -> model -> Render
                       -> Reaction m model (LoopAction action)
handleRender app model = \case
    Render -> model <# do let renderer = app^.rendererRef
                              texture  = app^.textureRef
                          -- sets color to white
                          SDL.clear renderer
                          -- now render
                          act <- (app^.config.appRender) model texture
                          SDL.copy renderer texture Nothing Nothing
                          SDL.present renderer
                          pure $ Continue act

--------------------------------------------------------------------------------


diagramDraw        :: model -> Diagram Cairo
diagramDraw _model = circle 1 & fc blue


-- -- | draw on SDL texture with Render monad from Cairo
-- withCairoTexture     :: SDL.Texture -> Render () -> IO ()
-- withCairoTexture t m = withCairoTexture' t (\s -> renderWith s m)


--------------------------------------------------------------------------------

-- cairoDraw _ _ =



--------------------------------------------------------------------------------

myDraw               :: model -> View IO MyAction
myDraw model texture = do renderDiagramTo texture $ diagramDraw model
                          pure Skip

  -- do SDL.rendererDrawColor renderer SDL.$= V4 0 0 255 255
  --                      SDL.drawRect renderer (Just $ SDL.Rectangle origin (V2 200 100))
  --                      pure Skip


-- withRenderer :: Handler m model action'
--              -> App m model action -> model -> Either Render action -> Handler m model action'
-- withRenderer handler

-- withDefaultRender :: Handler m model action -> App m model action -> Handler m model action
-- withDefaultRender handler app model =


--------------------------------------------------------------------------------

data MyAction = RenderAction Render
              | SDLEvent SDL.Event
              | Skip

myHandler app model = \case
  RenderAction renderAct -> handleRender app model renderAct
  SDLEvent e                   -> case SDL.eventPayload e of
    SDL.MouseMotionEvent mouseData -> model <# do print $ SDL.mouseMotionEventPos mouseData
                                                  pure $ Continue Skip
    _                              -> noEff model
  Skip                   -> noEff model

--------------------------------------------------------------------------------

main :: IO ()
main = do
  app <- initializeSDLApp $ AppConfig { _appModel        = ()
                                      , _handler         = myHandler
                                      , _startupAction   = Skip
                                      , _liftSDLEvent    = withDefaultSDLEvents SDLEvent
                                      , _liftRenderEvent = RenderAction
                                      , _appRender       = myDraw
                                      }
  runApp app
