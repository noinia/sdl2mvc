{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module SDL2MVC
  ( main
  ) where

import           Control.Concurrent.Async (mapConcurrently_, withAsync, uninterruptibleCancel)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as Queue
import           Control.Lens
import           Debug.Trace
import           Effectful
import           GHC.Natural
import           Linear
import           Linear.Affine
import qualified SDL
import           SDL2MVC.App
import           SDL2MVC.Cairo
import           SDL2MVC.Framework
import           SDL2MVC.Reaction
import           SDL2MVC.Render


import qualified GI.Cairo.Render as Cairo

--------------------------------------------------------------------------------
-- * Model


data MyModel = MyModel { _mousePosition :: !(Maybe (Point V2 Int))
                       }
             deriving (Show,Eq)

makeLenses ''MyModel

defaultModel :: MyModel
defaultModel = MyModel { _mousePosition = Nothing
                       }


--------------------------------------------------------------------------------
-- * Controller


data MyAction = RenderAction Render
              | SDLEvent SDL.Event
              | Skip
              deriving (Show,Eq)


----------------------------------------


myHandler app model = \case
  RenderAction renderAct -> handleRender app model renderAct
  SDLEvent e                   -> case SDL.eventPayload e of
    SDL.MouseMotionEvent mouseData -> let p = fromIntegral <$> SDL.mouseMotionEventPos mouseData
                                      in (model&mousePosition ?~ p)
                                         <# do pure $ Continue (RenderAction Render)
    _                              -> noEff model
    -- SDL.WindowShownEvent _         -> model <# (pure $ Continue (RenderAction Render))
    -- SDL.WindowExposedEvent _       -> model <# (pure $ Continue (RenderAction Render))
    -- SDL.WindowGainedKeyboardFocusEvent _  -> model <# (pure $ Continue (RenderAction Render))

  Skip                   -> noEff model



--------------------------------------------------------------------------------
-- * View


headerHeight = 10
footerHeight = 20

paneWidth    = 100
toolBarWidth = 16




-- -- | draw on SDL texture with Render monad from Cairo
-- withCairoTexture     :: SDL.Texture -> Render () -> IO ()
-- withCairoTexture t m = withCairoTexture' t (\s -> renderWith s m)


--------------------------------------------------------------------------------

-- cairoDraw _ _ =



--------------------------------------------------------------------------------


  -- do SDL.rendererDrawColor renderer SDL.$= V4 0 0 255 255
  --                      SDL.drawRect renderer (Just $ SDL.Rectangle origin (V2 200 100))
  --                      pure Skip


-- withRenderer :: Handler m model action'
--              -> App m model action -> model -> Either Render action -> Handler m model action'
-- withRenderer handler

-- withDefaultRender :: Handler m model action -> App m model action -> Handler m model action
-- withDefaultRender handler app model =


--------------------------------------------------------------------------------

--------------------------------------------------------------------------------


myDraw               :: MyModel -> View IO MyAction
myDraw model texture =
  -- do
  --   renderDiagramTo texture $ diagramDraw model
  --   pure Skip
  Skip <$ case fmap fromIntegral <$> model^.mousePosition of
    Nothing -> print "cursor outside screen"
    Just (P (V2 x y)) -> do SDL.TextureInfo _ _ w h <- SDL.queryTexture texture
                            withCairoTexture texture $ do
                                    Cairo.setSourceRGB 1 1 1
                                    Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
                                    Cairo.fill
                                    Cairo.setSourceRGB 1 0.5 0
                                    Cairo.arc x y 2 0 (2*pi)
                                    -- Cairo.rectangle x y 20 20
                                    Cairo.fill


--------------------------------------------------------------------------------

main :: IO ()
main = runApp $
       AppConfig
         { _appModel        = defaultModel
         , _handler         = myHandler
         , _startupAction   = Nothing
         , _liftSDLEvent    = withDefaultSDLEvents SDLEvent
         , _liftRenderEvent = RenderAction
         , _appRender       = myDraw
         , _windowTitle     = "My SDL2MVC App"
         }
