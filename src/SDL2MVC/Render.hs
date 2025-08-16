{-# LANGUAGE AllowAmbiguousTypes #-}
module SDL2MVC.Render
  ( handleRender
  , RendererData(..)

  , handleAnimate
  ) where

import           Control.Lens
import           Effectful
import           Effectful.Concurrent (Concurrent, threadDelay)
import qualified GI.Cairo.Render as Cairo
import           HGeometry.Box
import           HGeometry.Point
import           HGeometry.Transformation
import           HGeometry.Vector
import           HGeometry.Viewport
import qualified SDL
import           SDL2MVC.App
import           SDL2MVC.Cairo
import           SDL2MVC.Reaction
import           SDL2MVC.Send
import           SDL2MVC.Updated
import qualified Vary

--------------------------------------------------------------------------------

data RendererData model = RendererData SDL.Renderer
                                       SDL.Texture
                                       (model -> RenderTarget -> Cairo.Render ())

-- | Handles a render action
handleRender                     :: IOE :> es
                                 => RendererData model
                                 -> Handler es model msgs inMsgs
                                 -> Handler es model msgs (Render : inMsgs)
handleRender renderData handler' = \model msg -> case Vary.pop msg of
    Right Render -> Unchanged <$ runRenderWith renderData model
    Left msg'    -> handler' model msg'



-- | Runs the rendering action
runRenderWith                                              :: IOE :> es
                                                           => RendererData model
                                                           -> model
                                                           -> Eff es ()
runRenderWith (RendererData renderer texture render) model = do
    SDL.TextureInfo _ _ w h <- liftIO $ SDL.queryTexture texture
    let screen = Rectangle origin (Point2 (realToFrac w) (realToFrac h))
        vp     = Viewport screen identity
        renderTarget = RenderTarget texture vp
    liftIO $ withCairoTexture texture $ do
      render model renderTarget
    -- display the drawing
    liftIO $ do
      SDL.copy renderer texture Nothing Nothing
      SDL.present renderer

--------------------------------------------------------------------------------



frameRate = 60

-- | Computes the right time to wait in microseconds for a given framerate
fromFrameRate      :: Int -> Int
fromFrameRate rate = 1_000_000 `div` rate

-- | Handles a render action
--
--
handleAnimate          :: forall msgs es model.
                          ( Concurrent   :> es
                          , Send' model msgs :> es
                          , IOE :> es
                          )
                       => Handler es model (All model msgs) msgs
                       -> Handler es model (All model msgs) (Animate model : msgs)
handleAnimate handler' = \model msg -> case Vary.pop msg of
    Right (Animate shouldContinue) -> Unchanged <$
                                      do sendMsg @(All model msgs) Render
                                         case shouldContinue model of
                                           Stop     -> pure ()
                                           Continue -> do threadDelay (fromFrameRate frameRate)
                                                          liftIO $ putStrLn "go"
                                                          sendMsg @(All model msgs) $
                                                            Animate shouldContinue
    Left msg'    -> handler' model msg'
