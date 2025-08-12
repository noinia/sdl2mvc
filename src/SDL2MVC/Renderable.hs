{-# LANGUAGE PatternSynonyms #-}
module SDL2MVC.Renderable
  ( Renderable(..)
  , Box(..,Rect)
  ) where

import           Control.Lens hiding (elements)
import           Data.Colour
import qualified Data.Colour as Colour
import           Data.Colour.Names (red,blue,white,green,orange)
import           Data.Colour.SRGB (RGB(..), toSRGB)
import           Data.Foldable
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Matrix as CairoM
import           HGeometry.Ball
import           HGeometry.Box
import           HGeometry.Ellipse
import           HGeometry.Ext
import           HGeometry.Matrix
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.Polygon
import           HGeometry.Properties
import           HGeometry.Transformation
import           HGeometry.Triangle
import           HGeometry.Vector
import           HGeometry.Viewport
import           SDL2MVC.Drawing.Color
import           SDL2MVC.Drawing.Path
import           SDL2MVC.Drawing.Text
import qualified Vary

--------------------------------------------------------------------------------

class Renderable t where
  -- | Given the dimensions of the screen, and something that can be rendered, render the
  -- object.
  render :: Vector 2 Double -> t -> Cairo.Render ()


instance Renderable t => Renderable (Seq.Seq t) where
  render vp = traverse_ (render vp)
instance Renderable t => Renderable [t] where
  render vp = traverse_ (render vp)

instance Renderable (Vary.Vary '[]) where
  render _ _ = pure ()

instance (Renderable g, Renderable (Vary.Vary gs)) => Renderable (Vary.Vary (g:gs)) where
  render vp v = case Vary.pop v of
                  Right g -> render vp g
                  Left v' -> render vp v'

instance Renderable (TextLabel :+ TextAttributes) where
  render _ (TextLabel t p :+ ats) = do applyTextAttributes ats
                                       renderTextAt p t

-- | Sets the text attributes
applyTextAttributes     :: TextAttributes -> Cairo.Render ()
applyTextAttributes ats = do setColor (ats^.textColor)
                             Cairo.setFontSize (ats^.textSize)

instance Renderable (Rectangle (Point 2 Double) :+ PathAttributes) where
  render _ (g :+ ats) = rectangle ats g

instance Renderable (SimplePolygon (Point 2 Double) :+ PathAttributes) where
  render _ (g :+ ats) = polygon ats g

instance Renderable (Triangle (Point 2 Double) :+ PathAttributes) where
  render _ (g :+ ats) = triangle ats g

instance Renderable (Ellipse Double :+ PathAttributes) where
  render _ (g :+ ats) = ellipse ats g

instance Renderable (Disk (Point 2 Double) :+ PathAttributes) where
  render _ (g :+ ats) = disk ats g



--------------------------------------------------------------------------------
-- TODO: move to HGeometry

-- | Given x y w h construct the rectangle with bottom left corner (x,y), width w, and
-- hegith h.
pattern Rect :: Num r => r -> r -> r -> r -> Rectangle (Point 2 r)
pattern Rect x y w h <- (rectXYWH -> (x,y,w,h))
  where
    Rect x y w h = Rectangle (Point2 x y) (Point2 (x+w) (y+h))

-- | Extract the x,y of the lower left corner, and width and height of the rectangle.
rectXYWH      :: Num r => Rectangle (Point 2 r) -> (r,r,r,r)
rectXYWH rect = let Vector2 w h = size rect
                    Point2 x y  = rect^.minPoint
                in (x,y,w,h)

--------------------------------------------------------------------------------
-- * Raw cairo renderers

-- | Renders some text label at a given location
renderTextAt                  :: Real r => Point 2 r -> Text -> Cairo.Render ()
renderTextAt (Point2 x y) txt = do Cairo.save
                                   Cairo.translate (realToFrac x) (realToFrac y)
                                   Cairo.showText txt
                                   Cairo.restore

-- | Convert a matrix into Cairo form.
toCairoMatrix   :: Real r => Matrix 3 3 r -> Cairo.Matrix
toCairoMatrix m = case m&elements %~ realToFrac of
  Matrix (Vector3
          (Vector3 a b c)
          (Vector3 d e f)
          _     -- (Vector3 0 0 1) -- should be
         ) -> CairoM.Matrix a d b e c f
  _        -> error "toCairoMatrix: Don't know how to convert this matrix!"

-- | Render stuff within the given viewport
render'           :: Real r => Viewport r -> Cairo.Render () -> Cairo.Render ()
render' vp render = do Cairo.save
                       Cairo.transform (vp^.worldToHost.transformationMatrix.to toCairoMatrix)
                       render
                       Cairo.restore

-- | Set the stroke
setStroke s = do Cairo.setLineWidth $ s^.strokeWidth
                 setColor $ s^.strokeColor

-- | Sets the stroke and fill approprately
withStrokeAndFill                 :: StrokeAndFillSpec -> Cairo.Render () -> Cairo.Render ()
withStrokeAndFill spec renderPath = case spec of
  StrokeOnly s      -> do setStroke s
                          renderPath
                          Cairo.stroke
  FillOnly f        -> do setColor f
                          renderPath
                          Cairo.fill
  StrokeAndFill s f -> do setColor f
                          renderPath
                          Cairo.fillPreserve
                          setStroke s
                          renderPath
                          Cairo.stroke

-- | Renders a disk
disk          :: (Point_ point 2 r, Real r)
              => PathAttributes -> Disk point -> Cairo.Render ()
disk ats disk = withStrokeAndFill (ats^.pathColor) $ do
                   let Point2 x y = disk^.center.asPoint
                                    & coordinates %~ realToFrac
                       r          = realToFrac $ disk^.squaredRadius
                   Cairo.arc x y (sqrt r) 0 (2*pi)

-- | Render an ellipse
ellipse          :: Real r
                 => PathAttributes -> Ellipse r -> Cairo.Render ()
ellipse ats e = withStrokeAndFill (ats^.pathColor) $ do
                   Cairo.save
                   Cairo.transform (e^.ellipseMatrix.to toCairoMatrix)
                   Cairo.arc 0 0 1 0 (2*pi)
                   Cairo.restore

-- | Extract an alphacolor into a tuple
toRGBA     :: Color -> (RGB Double,Double)
toRGBA col = ( toSRGB $ col `Data.Colour.over` black
             , alphaChannel col
             )

setColor col = let (RGB r g b, a) = toRGBA col
               in Cairo.setSourceRGBA r g b a

rectangle          :: (Point_ point 2 r, Real r)
                   => PathAttributes -> Rectangle point -> Cairo.Render ()
rectangle ats rect = withStrokeAndFill (ats^.pathColor) $ do
                        let Point2 x y  = rect^.minPoint.asPoint
                                          & coordinates %~ realToFrac
                            Vector2 w h = realToFrac <$> size rect
                        Cairo.rectangle x y w h

-- | Render a triangle
triangle         :: (Point_ point 2 r, Real r)
                 => PathAttributes -> Triangle point -> Cairo.Render ()
triangle ats tri = polygon' ats (toNonEmpty tri)

-- | Render a simple polygon
polygon        :: (SimplePolygon_ simplePolygon point r, Point_ point 2 r, Real r)
               => PathAttributes -> simplePolygon -> Cairo.Render ()
polygon ats pg = polygon' ats (toNonEmptyOf vertices pg)


polygon' ats vs = withStrokeAndFill (ats^.pathColor) $
                  case toPoints vs of
                    (u :| vs) -> do Cairo.moveTo (u^.xCoord) (u^.yCoord)
                                    for_ vs $ \v ->
                                      Cairo.lineTo (v^.xCoord) (v^.yCoord)
                                    Cairo.lineTo (u^.xCoord) (u^.yCoord)
                                    Cairo.closePath

-- | Render a polyLine
polyLine        :: ( HasVertices polyLine polyLine
                   , Vertex polyLine ~ point, Point_ point 2 r, Real r)
                => PathAttributes -> polyLine -> Cairo.Render ()
polyLine ats pg = withStrokeAndFill (ats^.pathColor) $
                    case toPoints $ toNonEmptyOf vertices pg of
                      (u :| vs) -> do Cairo.moveTo (u^.xCoord) (u^.yCoord)
                                      for_ vs $ \v ->
                                        Cairo.lineTo (v^.xCoord) (v^.yCoord)


toPoints :: (Functor f, Point_ point 2 r, Real r) => f point -> f (Point 2 Double)
toPoints = fmap (\p -> p^.asPoint &coordinates %~ realToFrac)
