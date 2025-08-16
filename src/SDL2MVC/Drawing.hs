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
import           HGeometry.Ball
import           HGeometry.Box
import           HGeometry.Ellipse
import           HGeometry.Ext
import           HGeometry.Matrix
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.LineSegment
import           HGeometry.Polygon
import           HGeometry.Properties
import           HGeometry.Transformation
import           HGeometry.Triangle
import           HGeometry.Vector
import           HGeometry.Viewport
import           SDL2MVC.Drawing.Color
import           SDL2MVC.Drawing.Path
import           SDL2MVC.Drawing.Text
import           SDL2MVC.Renderable
import qualified Vary


import           Data.Typeable
import           Debug.Trace

--------------------------------------------------------------------------------
-- * Drawing

-- | The Main Drawing type
newtype Drawing = Drawing (Seq.Seq (Vary.Vary DrawingElements))
  deriving newtype (Show,Eq,Semigroup,Monoid,Renderable,Layoutable,IsTransformable)

type instance Dimension Drawing = 2
type instance NumType   Drawing = Double

newtype Blank = Blank Color
  deriving (Show,Eq)


type instance Dimension Blank = 2
type instance NumType   Blank = Double

instance IsTransformable Blank where
  transformBy _ = id

instance Renderable Blank where
  render dims@(Vector2 w h) (Blank c) = render dims $
                                          Rect 0 0 w h :+ (def&pathColor .~ FillOnly c)

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


type DrawingElements = [ Rectangle      (Point 2 Double) :+ PathAttributes
                       , Ellipse Double                  :+ PathAttributes
                       , Triangle       (Point 2 Double) :+ PathAttributes
                       , SimplePolygon  (Point 2 Double) :+ PathAttributes
                       , PolyLine       (Point 2 Double) :+ PathAttributes
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
drawInViewport      :: ( Drawable t, IsTransformable t
                       , NumType t ~ Double, Dimension t ~ 2

                       , Show t
                       ) => Viewport Double -> t -> Drawing
drawInViewport vp = draw . toHostFrom vp
-- (<> drawViewport vp) .

-- TODO we may want to do some manual clipping here


--------------------------------------------------------------------------------

-- | Class of things that can be turned into an Drawing
class Drawable t where
  -- | Given an object, construct a drawing out of it.
  draw :: t -> Drawing

instance Drawable Drawing where
  draw = id
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
instance Drawable (SimplePolygon (Point 2 Double) :+ PathAttributes) where
  draw = draw'
instance Drawable (Disk (Point 2 Double) :+ PathAttributes) where
  draw (d :+ ats) = draw $ (d^._DiskCircle.re _EllipseCircle) :+ ats
instance Drawable (TextLabel :+ TextAttributes) where
  draw = draw'
instance Drawable (PolyLine (Point 2 Double) :+ PathAttributes) where
  draw = draw'
instance Drawable (ClosedLineSegment (Point 2 Double) :+ PathAttributes) where
  draw (seg :+ ats) = draw $ ((seg^.re _PolyLineLineSegment) :: PolyLine (Point 2 Double)) :+ ats
instance Drawable Blank where
  draw = draw'


  -- drawIn vp (Blank c) = draw' $ (vp^.viewPort) :+ (def&pathColor .~ FillOnly c)


-- instance Drawable (Disk (Point 2 Double) :+ PathAttributes) where
--   drawIn _ = draw



-- instance Drawable (Vary.Vary '[]) where
--   drawIn _ _ = mempty

-- instance (Drawable g, Drawable (Vary.Vary gs)) => Drawable (Vary.Vary (g:gs)) where
--   draw v = case Vary.pop v of
--                   Right g -> render vp g
--                     Left v' -> render vp v'













--------------------------------------------------------------------------------

class Layoutable t where
  -- | constructs a drawing of the object in terms of *host* coordinates.
  --
  --
  drawIn    :: Viewport Double -> t -> Drawing
  -- drawIn vp = drawIn vp . draw


instance Layoutable t => Layoutable [t] where
  drawIn vp = foldMap (drawIn vp)

instance Layoutable t => Layoutable (Seq.Seq t) where
  drawIn vp = foldMap (drawIn vp)

instance Layoutable (Vary.Vary '[]) where
  drawIn _ = mempty

instance (Layoutable t, Layoutable (Vary.Vary ts)) => Layoutable (Vary.Vary (t:ts)) where
  drawIn vp v = case Vary.pop v of
                  Right g -> drawIn vp g
                  Left v' -> drawIn vp v'

instance Layoutable (Rectangle (Point 2 Double) :+ PathAttributes) where
  drawIn = drawInViewport

  -- vp = draw . toHostFrom vp


instance Layoutable (Triangle (Point 2 Double) :+ PathAttributes) where
  drawIn = drawInViewport
  -- drawIn vp = draw'

instance Layoutable (Ellipse Double :+ PathAttributes) where
  drawIn = drawInViewport
  -- drawIn vp = draw'
instance Layoutable (SimplePolygon (Point 2 Double) :+ PathAttributes) where
  drawIn = drawInViewport
instance Layoutable (PolyLine (Point 2 Double) :+ PathAttributes) where
  drawIn = drawInViewport
instance Layoutable (TextLabel :+ TextAttributes) where
  drawIn = drawInViewport
  -- drawIn vp = draw'
instance Layoutable Blank where
  drawIn vp (Blank c) = draw' $ (vp^.viewPort) :+ (def&pathColor .~ FillOnly c)



-- instance Layoutable  where
--   drawIn









-- layoutDrawingIn :: Viewport Double -> Drawing -> Drawing
-- layoutDrawingIn = undefined



-- mostly a helper class for the various pieces of a









--------------------------------------------------------------------------------

-- | renders an object in the given viewport
--
--- pre : the viewport size is the screen size
renderDrawingIn      :: Layoutable t => Viewport Double -> t -> Cairo.Render ()
renderDrawingIn vp g = render (vp^.viewPort.to size) $ drawIn vp g

--------------------------------------------------------------------------------
-- * Renderable instances

-- | Given a drawing, we
