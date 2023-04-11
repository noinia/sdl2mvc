module SDL2MVC.Cairo.Stateless
  ( renderOn
  , renderAsSvg
  , renderAsPdf

  , test
  ) where

import           Control.Lens
import           Control.Monad.IO.Class
-- import           Control.Monad.Reader.Class
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.State.Class
import           Control.Monad.State.Strict (StateT(..), evalStateT)
import qualified Data.Colour as Colour
import           Data.Colour.SRGB (RGB(..), toSRGB)
import           Data.Default.Class
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word
import           Foreign.C.Types (CInt)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.Matrix as Matrix
import           HGeometry.Box
import           HGeometry.Interval
import           HGeometry.Matrix
import           HGeometry.Point
import           HGeometry.Transformation
import           HGeometry.Vector
import qualified SDL
import qualified SDL.Cairo
-- import           SDL2MVC.Attribute

import           SDL2MVC.Canvas


import Data.Colour.Names(red,blue)

--------------------------------------------------------------------------------

test = do renderAsSvg "/tmp/out.svg" (Vector2 800 600) myCanvas
          renderAsPdf "/tmp/out.pdf" (Vector2 800 600) myCanvas
myCanvas :: Canvas
myCanvas = drawAll [ draw (def&pathColor .~ StrokeOnly blue) $ Rectangle (Point2 10 10) (Point2 100 (100 :: R))
                   , draw (def&pathColor .~ StrokeAndFill red blue) $
                          Rectangle (Point2 10 210) (Point2 220 (220 :: R))
                   , draw def $ Point2 300 (400 :: R)
                   , drawAt (Point2 320 320) def $ Text.pack "woei"
                   -- , drawAt (Point2 320 320) def $ Point2 0 0
                   ]


--------------------------------------------------------------------------------

-- | Render the canvas on the given surface
renderOn        :: MonadIO m => Cairo.Surface -> Canvas -> m ()
renderOn surf c = do w <- Cairo.imageSurfaceGetWidth  surf
                     h <- Cairo.imageSurfaceGetHeight surf
                     let canvasInfo   = CanvasInfo (Vector2 w h)
                         initialState = RenderState zero zero
                     Cairo.renderWith surf $ runRender canvasInfo initialState c

-- | Render the canvas to the given svg file.
renderAsSvg                    :: FilePath
                               -> Vector 2 R
                               -- ^ size of the surface in points (1 point == 1/72.0 inch)
                               -> Canvas
                               -> IO ()
renderAsSvg fp (Vector2 w h) c = Cairo.withSVGSurface fp w h $ \surf ->
                                   renderOn surf c


-- | Render the canvas to the given pdf file.
renderAsPdf                    :: FilePath
                               -> Vector 2 R
                               -- ^ size of the surface in points (1 point == 1/72.0 inch)
                               -> Canvas
                               -> IO ()
renderAsPdf fp (Vector2 w h) c = Cairo.withPDFSurface fp w h $ \surf ->
                                   renderOn surf c

--------------------------------------------------------------------------------
-- * RenderM def

newtype RenderM m a =
  RenderM { rawRunRenderM :: ReaderT CanvasInfo (StateT RenderState m) a
          }
  deriving newtype ( Functor,Applicative,Monad
                   , MonadState RenderState
                   , MonadReader CanvasInfo
                   -- , MonadIO
                   )

type Render = RenderM Cairo.Render

instance MonadTrans RenderM where
  lift = RenderM . lift @(ReaderT CanvasInfo) . lift @(StateT RenderState)

runRenderM      :: Monad m => CanvasInfo -> RenderState -> RenderM m a -> m a
runRenderM ci s = flip evalStateT s . flip runReaderT ci . rawRunRenderM

runRender        :: CanvasInfo -> RenderState -> Canvas -> Cairo.Render ()
runRender ci s c = runRenderM ci s $ render c

newtype CanvasInfo = CanvasInfo { _size :: (Vector 2 Int)
                                -- ^ size of the canvas in pixels
                                }
                     deriving stock (Show,Eq)

data RenderState = RenderState { _strokeColor :: !(Vector 4 Word8)
                               , _fillColor   :: !(Vector 4 Word8)
                               }
                   deriving stock (Show,Eq)

strokeColor :: Lens' RenderState (Vector 4 Word8)
strokeColor = lens _strokeColor (\s c -> s { _strokeColor = c })

fillColor :: Lens' RenderState (Vector 4 Word8)
fillColor = lens _fillColor (\s c -> s { _fillColor = c })

--------------------------------------------------------------------------------
-- * Implementation

render   :: Canvas -> Render ()
render c = do lift $ Cairo.save
              interpret $ c^.canvasTransformation
              mapMOf_ (content.traverse) runRender' c
              lift $ Cairo.restore

runRender' :: CanvasObject -> Render ()
runRender' = \case
  PathObj ats p -> withPathAts ats $ renderPath p
  TextObj ats t -> withTextAts ats $ lift $ do Cairo.newPath
                                               Cairo.showText t
  CanvasObj c   -> render c


with                     :: (MonadState s (t m), MonadTrans t, Monad m)
                         => (a -> m ())
                         -> Lens' s a
                         -> a
                         -> t m b
                         -> t m b
with setterF field x act = do x0 <- use field
                              field .= x
                              lift $ setterF x
                              b <- act
                              field .= x0
                              pure b

withPathAts ats act = do act
                         lift $ case ats^.pathColor of
                           StrokeOnly c        -> stroke c
                           FillOnly c          -> fill c
                           StrokeAndFill sc fc -> do fill fc
                                                     stroke sc
  where
    stroke c = do let (RGB r g b) = toSRGB c
                  Cairo.setSourceRGB r g b
                  Cairo.strokePreserve
    fill c = do let (RGB r g b) = toSRGB c
                Cairo.setSourceRGB r g b
                Cairo.fillPreserve



withTextAts ats r = do r
  -- TODO

renderPath         :: PathSegment -> Render ()
renderPath pathSeg = do lift $ Cairo.newPath
                        render' pathSeg
  where
    render' = \case
      PolyPath vs     -> renderPoly vs
      ClosedPath vs   -> do renderPoly vs
                            lift $ Cairo.closePath
      ArcPath c r s e -> lift $ do Cairo.arc (c^.xCoord) (c^.yCoord) r s e

    renderPoly (p:|vs) = lift $
                         do Cairo.moveTo (p^.xCoord) (p^.yCoord)
                            mapM_ (\q -> Cairo.lineTo (q^.xCoord) (q^.yCoord)) vs


-- | interpret the matrix
interpret   :: Transformation 2 R -> Render ()
interpret t = lift $ Cairo.transform m
  where
    m = Matrix.Matrix a d b e c f
    Vector3 a b c = fromMaybe err $ row 0 (t^.transformationMatrix)
    Vector3 d e f = fromMaybe err $ row 1 (t^.transformationMatrix)
    err = error "SDL.view: interpet matrix does not have 2 rows!? "
