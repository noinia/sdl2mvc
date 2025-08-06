module SDL2MVC.Cairo
  ( createCairoTexture
  , createCairoTexture'
  , withCairoTexture

  -- , renderDiagramTo
  ) where

import           Control.Exception (bracket)
-- import qualified Diagrams.Backend.Cairo as Diagrams
-- import           Diagrams.Backend.Cairo.Internal (Options(..))
-- import qualified Diagrams.Core as Diagrams
-- import qualified Diagrams.Prelude as Diagrams
import           Foreign.C.Types (CInt)
import           Foreign.Ptr (castPtr)
import qualified GI.Cairo.Render as Cairo
import           Linear (V2(..))
import qualified SDL

--------------------------------------------------------------------------------

-- ported from sdl2-cairo

-- | create new texture for Cairo with given size
createCairoTexture    :: SDL.Renderer -> V2 CInt -> IO SDL.Texture
createCairoTexture r = SDL.createTexture r SDL.ARGB8888 SDL.TextureAccessStreaming

-- | create new texture for Cairo with the size of the given window
createCairoTexture'     :: SDL.Renderer -> SDL.Window -> IO SDL.Texture
createCairoTexture' r w = do
                            surf <- SDL.getWindowSurface w
                            sz   <- SDL.surfaceDimensions surf
                            createCairoTexture r sz

-- | draw on SDL texture with Render monad from Cairo
withCairoTexture     :: SDL.Texture -> Cairo.Render () -> IO ()
withCairoTexture t m = withCairoTexture' t (\s -> Cairo.renderWith s m)

-- | lock and unwrap SDL texture, get a Cairo surface, pass it to some function
withCairoTexture' :: SDL.Texture -> (Cairo.Surface -> IO a) -> IO a
withCairoTexture' t m = do
    SDL.TextureInfo f _ w h <- SDL.queryTexture t
    case mapFormat f of
      Nothing -> error "ERROR: Invalid pixel format for cairo use!"
      Just f' -> do
        (pixels, pitch) <- SDL.lockTexture t Nothing
        ret <- Cairo.withImageSurfaceForData (castPtr pixels) f'
                 (fromIntegral w) (fromIntegral h) (fromIntegral pitch) m
        SDL.unlockTexture t
        pure ret
  where
    mapFormat SDL.ARGB8888 = Just Cairo.FormatARGB32
    mapFormat SDL.RGB888   = Just Cairo.FormatRGB24
    mapFormat _            = Nothing

{-
-- | Given a function that takes the dimensions of the canvas and produces a diagram,
-- renders the diagram to the given texture.
renderDiagramTo                 :: SDL.Texture
                                -> (V2 Int -> Diagrams.Diagram Diagrams.Cairo)
                                -> IO ()
renderDiagramTo texture diagram = do
    SDL.TextureInfo _ _ w h <- SDL.queryTexture texture
    let dims    = fromIntegral <$> V2 w h
        options = CairoOptions
          { _cairoSizeSpec     = fromIntegral <$> Diagrams.dims2D w h
          , _cairoOutputType   = Diagrams.RenderOnly
          , _cairoBypassAdjust = False
          , _cairoFileName     = ""
          }
        (_, render) = Diagrams.renderDia Diagrams.Cairo options (diagram dims)
    withCairoTexture texture render
-}
