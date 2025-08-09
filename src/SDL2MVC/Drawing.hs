{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module SDL2MVC.Drawing
  ( Drawing(..)
  , drawIn
  , drawInViewport
  , DrawingElements

  , Blank(..)

  , renderDrawingIn
  , Drawable(..)
  , Renderable(..)


  , Color

  , module SDL2MVC.Drawing.Path
  , module SDL2MVC.Drawing.Text
  ) where

import           Control.Lens hiding (elements)
import           Data.Colour
import qualified Data.Colour as Colour
import           Data.Colour.Names (red,blue,white,green,orange)
import           Data.Colour.SRGB (RGB(..), toSRGB)
import           Data.Default.Class
import           Data.Foldable
import           Data.Kind (Type,Constraint)
import qualified Data.List as List
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
import           HGeometry.Properties
import           HGeometry.Transformation
import           HGeometry.Triangle
import           HGeometry.Vector
import           HGeometry.Viewport
import           SDL2MVC.Drawing.Color
import           SDL2MVC.Drawing.Path
import           SDL2MVC.Drawing.Text
import qualified Vary


import           Data.Typeable
import           Debug.Trace

--------------------------------------------------------------------------------
-- * Drawing

-- | The Main Drawing type
newtype Drawing = Drawing (Seq.Seq (Vary.Vary DrawingElements))
  deriving newtype (Show,Eq,Semigroup,Monoid,Renderable,IsTransformable)

type instance Dimension Drawing = 2
type instance NumType   Drawing = Double



newtype Blank = Blank Color
  deriving (Show,Eq)


type instance Dimension Blank = 2
type instance NumType   Blank = Double

instance IsTransformable Blank where
  transformBy _ = id

type instance Dimension (Vary.Vary (t:ts)) = Dimension t
type instance NumType   (Vary.Vary (t:ts)) = NumType t


instance (IsTransformable g, IsTransformableHelper (g:gs) (Dimension g) (NumType g)
         ) => IsTransformable (Vary.Vary (g:gs)) where
  transformBy = transformBy'


class IsTransformableHelper ts d r where
  transformBy' :: Transformation d r -> Vary.Vary ts -> Vary.Vary ts


instance IsTransformableHelper '[] d r where
  transformBy' _ = id

-- instance IsTransformable g => IsTransformable (Vary.Vary '[g]) where
instance (IsTransformable g, IsTransformableHelper gs d r
         , Dimension g ~ d
         , NumType   g ~ r
         ) => IsTransformableHelper (g:gs) d r where
  transformBy' t v = case Vary.pop v of
                      Right g -> Vary.from     $ transformBy t g
                      Left v' -> Vary.pushTail $ transformBy' t v'

type instance Dimension (Seq.Seq g) = Dimension g
type instance NumType   (Seq.Seq g) = NumType g

instance IsTransformable g => IsTransformable (Seq.Seq g) where
  transformBy t = fmap (transformBy t)


type DrawingElements = [ Rectangle (Point 2 Double) :+ PathAttributes
                       , Ellipse Double             :+ PathAttributes
                       , Triangle  (Point 2 Double) :+ PathAttributes
                       , Blank
                       , TextLabel :+ TextAttributes
                       ]

draw' :: g Vary.:| DrawingElements => g -> Drawing
draw' = Drawing . Seq.singleton . Vary.from

--------------------------------------------------------------------------------

-- drawIn vp = (<> drawViewport vp) . drawInViewport vp . draw

drawViewport    :: Viewport Double -> Drawing
drawViewport vp = draw $ (vp^.viewPort) :+ (def @PathAttributes)

-- | Given a drawing, draws it in the given viewport ; i.e. applies the appropriate
-- transform
--
drawInViewport :: Viewport Double -> Drawing -> Drawing
drawInViewport = toHostFrom


--------------------------------------------------------------------------------

class Drawable t where
  -- | Given an object, construct a drawing out of it.
  draw :: t -> Drawing

  -- | Given a viewport, and some some geometric object, specified in world coordinates,
    -- constructs a drawing of the object in terms of *host* coordinates.
  drawIn    :: Viewport Double -> t -> Drawing
  drawIn vp = drawInViewport vp . draw



instance Drawable t => Drawable [t] where
  draw = foldMap draw
instance Drawable t => Drawable (Seq.Seq t) where
  draw = foldMap draw

instance Drawable (Rectangle (Point 2 Double) :+ PathAttributes) where
  draw = draw'
instance Drawable (Triangle (Point 2 Double) :+ PathAttributes) where
  draw = draw'
instance Drawable (Ellipse Double :+ PathAttributes) where
  draw = draw'
instance Drawable (Disk (Point 2 Double) :+ PathAttributes) where
  draw (d :+ ats) = draw $ (d^._DiskCircle.re _EllipseCircle) :+ ats

instance Drawable (TextLabel :+ TextAttributes) where
  draw = draw'

-- instance Drawable (Disk (Point 2 Double) :+ PathAttributes) where
--   drawIn _ = draw



-- instance Drawable (Vary.Vary '[]) where
--   drawIn _ _ = mempty

-- instance (Drawable g, Drawable (Vary.Vary gs)) => Drawable (Vary.Vary (g:gs)) where
--   draw v = case Vary.pop v of
--                   Right g -> renderIn vp g
--                     Left v' -> renderIn vp v'


instance Drawable Blank where
  draw = draw'
  drawIn vp (Blank c) = draw' $ (vp^.viewPort) :+ (def&pathColor .~ FillOnly c)

instance Drawable Drawing where
  draw = id -- toHostIn vp

-- (vp^.viewPort) apply the transform

renderDrawingIn      :: Drawable t => Viewport Double -> t -> Cairo.Render ()
renderDrawingIn vp g = renderIn (vp^.viewPort) $ drawIn vp g

-- instance Rectangle (Point 2 Double) :+ PathAttributes where
--   draw (r :+ ats) = rectangle ats r

--------------------------------------------------------------------------------
-- * Raw cairo renderers

renderTextAt                  :: Real r => Point 2 r -> Text -> Cairo.Render ()
renderTextAt (Point2 x y) txt = do Cairo.save
                                   Cairo.translate (realToFrac x) (realToFrac y)
                                   Cairo.showText txt
                                   Cairo.restore

toCairoMatrix   :: Real r => Matrix 3 3 r -> Cairo.Matrix
toCairoMatrix m = case m&elements %~ realToFrac of
  Matrix (Vector3
          (Vector3 a b c)
          (Vector3 d e f)
          _
         ) -> CairoM.Matrix a d b e c f

renderIn'           :: Real r => Viewport r -> Cairo.Render () -> Cairo.Render ()
renderIn' vp render = do
                        Cairo.save
                        Cairo.transform (vp^.worldToHost.transformationMatrix.to toCairoMatrix)
                        render
                        Cairo.restore


setStroke s = do Cairo.setLineWidth $ s^.strokeWidth
                 setColor $ s^.strokeColor


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

disk          :: (Point_ point 2 r, Real r)
              => PathAttributes -> Disk point -> Cairo.Render ()
disk ats disk = withStrokeAndFill (ats^.pathColor) $ do
                   let Point2 x y = disk^.center.asPoint
                                    & coordinates %~ realToFrac
                       r          = realToFrac $ disk^.squaredRadius
                   Cairo.arc x y (sqrt r) 0 (2*pi)


ellipse          :: Real r
                 => PathAttributes -> Ellipse r -> Cairo.Render ()
ellipse ats e = withStrokeAndFill (ats^.pathColor) $ do
                   Cairo.save
                   Cairo.transform (e^.ellipseMatrix.to toCairoMatrix)
                   Cairo.arc 0 0 1 0 (2*pi)
                   Cairo.restore




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

triangle         :: (Point_ point 2 r, Real r)
                 => PathAttributes -> Triangle point -> Cairo.Render ()
triangle ats tri = withStrokeAndFill (ats^.pathColor) $ do
                      let Triangle a b c = toPoints tri
                      Cairo.moveTo (a^.xCoord) (a^.yCoord)
                      Cairo.lineTo (b^.xCoord) (b^.yCoord)
                      Cairo.lineTo (c^.xCoord) (c^.yCoord)
                      Cairo.closePath

toPoints :: (Functor f, Point_ point 2 r, Real r) => f point -> f (Point 2 Double)
toPoints = fmap (\p -> p^.asPoint &coordinates %~ realToFrac)


--------------------------------------------------------------------------------
-- * Renderable instances

class Renderable t where
  renderIn :: Rectangle (Point 2 Double) -> t -> Cairo.Render ()

instance Renderable t => Renderable (Seq.Seq t) where
  renderIn vp = traverse_ (renderIn vp)
instance Renderable t => Renderable [t] where
  renderIn vp = traverse_ (renderIn vp)


instance Renderable (Vary.Vary '[]) where
  renderIn _ _ = pure ()

instance (Renderable g, Renderable (Vary.Vary gs)) => Renderable (Vary.Vary (g:gs)) where
  renderIn vp v = case Vary.pop v of
                    Right g -> renderIn vp g
                    Left v' -> renderIn vp v'

instance Renderable Blank where
  renderIn vp (Blank c) = rectangle (def&pathColor .~ FillOnly c)
                                    vp

instance Renderable (TextLabel :+ TextAttributes) where
  renderIn _ (TextLabel t p :+ ats) = do applyTextAttributes ats
                                         renderTextAt p t

applyTextAttributes     :: TextAttributes -> Cairo.Render ()
applyTextAttributes ats = do setColor (ats^.textColor)
                             Cairo.setFontSize (ats^.textSize)

instance Renderable (Rectangle (Point 2 Double) :+ PathAttributes) where
  renderIn _ (g :+ ats) = rectangle ats g

instance Renderable (Triangle (Point 2 Double) :+ PathAttributes) where
  renderIn _ (g :+ ats) = triangle ats g

instance Renderable (Ellipse Double :+ PathAttributes) where
  renderIn _ (g :+ ats) = ellipse ats g

instance Renderable (Disk (Point 2 Double) :+ PathAttributes) where
  renderIn _ (g :+ ats) = disk ats g
