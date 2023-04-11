{-# LANGUAGE UndecidableInstances #-}
module SDL2MVC.Canvas
  ( Canvas(Canvas)
  , canvasTransformation, content
  -- * Construct a canvas
  , blank, group, drawAll

  , CanvasObject(..)
  , Path
  , PathSegment(..)

  , PathAttributes(PathAttributes)
  , pathColor
  , TextAttributes(TextAttributes)
  , PathColor(..)
  , Color


  , Drawable(..)
  , R
  ) where

import           Control.Lens
import qualified Data.Colour as Colour
import           Data.Default.Class
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe
import           Data.Semigroup.Foldable
import           Data.Text (Text)
import qualified Data.Text as Text
import           HGeometry.Ball
import           HGeometry.Box
import           HGeometry.Cyclic
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.Polygon.Simple
import           HGeometry.Properties
import           HGeometry.Transformation

--------------------------------------------------------------------------------

type R = Double

-- | A canvas
data Canvas = Canvas { _canvasTransformation :: !(Transformation 2 R)
                     -- ^ apply this transofmration to all objects
                     , _content              :: [CanvasObject]
                     -- ^ objects to draw on te canvas, drawn in this order
                     }
  deriving stock (Show,Eq)

-- | The transformation to apply to all obejcts on the canvas
canvasTransformation :: Lens' Canvas (Transformation 2 R)
canvasTransformation = lens _canvasTransformation (\c t -> c { _canvasTransformation = t })
{-# INLINE canvasTransformation #-}

-- | Content on the canvas
content :: Lens' Canvas [CanvasObject]
content = lens _content (\c xs -> c { _content = xs })
{-# INLINE content #-}

type instance NumType   Canvas = R
type instance Dimension Canvas = 2

-- | Creates a blank canvas
blank :: Canvas
blank = Canvas identity []

-- | Creates a canvas from a group of Drawable canvas objects
group :: [CanvasObject] -> Canvas
group = Canvas identity

-- | Draws a bunch of drawable things
drawAll :: [Canvas] -> Canvas
drawAll = group . map CanvasObj

instance IsTransformable Canvas where
  transformBy t' (Canvas t xs) = Canvas (t' |.| t) xs

-- | Objects in a canvas
data CanvasObject = PathObj   !PathAttributes !Path
                  | TextObj   !TextAttributes !Text
                  | CanvasObj Canvas
                  deriving stock (Show,Eq)

-- | for now just do paths consisting of a single segment
type Path = PathSegment

-- | Paths
data PathSegment = PolyPath   (NonEmpty (Point 2 R))
                   -- ^ vertices of a polygonal path
                 | ClosedPath (NonEmpty (Point 2 R))
                 -- ^ vertices of a polygonal closed path
                 | ArcPath    (Point 2 R)
                              R
                              R
                              R
                 -- ^ center, radius, starting angle, cw ending angle in radians.
                 deriving stock (Show,Eq)

--------------------------------------------------------------------------------
-- * Attributes

--------------------------------------------------------------------------------

data PathAttributes = PathAttributes { _pathColor :: {-# UNPACK #-} !PathColor
                                     }
  deriving stock (Show,Eq)

pathColor :: Lens' PathAttributes PathColor
pathColor = lens _pathColor (\ats c -> ats { _pathColor = c} )

instance Default PathAttributes where
  def = PathAttributes def

instance Semigroup PathAttributes where
  -- ^ Later options may overwrite former ones.
  (PathAttributes c) <> (PathAttributes c') = PathAttributes c'

instance Monoid PathAttributes where
  mempty = def


type Color = Colour.Colour Double

-- | Paths must have stroke, fill, or both
data PathColor =
    StrokeOnly  {-# UNPACK #-}!Color
  | FillOnly    {-# UNPACK #-}!Color
  | StrokeAndFill  {-# UNPACK #-}!Color -- ^ stroke color
                   {-# UNPACK #-}!Color -- ^ fill color
    deriving stock (Show,Eq)

getStrokeColor :: PathColor -> Maybe Color
getStrokeColor = \case
  StrokeOnly c      -> Just c
  StrokeAndFill c _ -> Just c
  FillOnly _        -> Nothing

getFillColor :: PathColor -> Maybe Color
getFillColor = \case
  StrokeOnly _      -> Nothing
  StrokeAndFill _ c -> Just c
  FillOnly c        -> Just c

instance Default PathColor where
  def = StrokeOnly Colour.black

----------------------------------------
-- * Text Attributes

data TextAttributes = TextAttributes { _textColor :: {-#UNPACK#-}!Color
                                     }
  deriving stock (Show,Eq)

textColor :: Lens' TextAttributes Color
textColor = lens _textColor (\ats c -> ats { _textColor = c } )

instance Default TextAttributes where
  def = TextAttributes Colour.black

instance Semigroup TextAttributes where
  -- ^ Later options may overwrite former ones.
  (TextAttributes c) <> (TextAttributes c') = TextAttributes c'

instance Monoid TextAttributes where
  mempty = def

--------------------------------------------------------------------------------

class Drawable geom where
  type DrawableAttributes geom
  draw :: DrawableAttributes geom -> geom -> Canvas

  -- | Draw a point at a particular location
  drawAt         :: (Point_ point 2 r, Real r)
                 => point -> DrawableAttributes geom -> geom -> Canvas
  drawAt p ats g = transformBy (translation $ (toPoint p)^.vector) $ draw ats g

instance Drawable Text where
  type DrawableAttributes Text = TextAttributes
  draw ats t = group [TextObj ats t]

instance Drawable PathSegment where
  type DrawableAttributes PathSegment = PathAttributes
  draw ats p = group [PathObj ats p]

instance Drawable String where
  type DrawableAttributes String = TextAttributes
  draw ats s = draw ats (Text.pack s)

-- instance Drawable g => Drawable [g] where
--   type DrawableAttributes [g] = ()
--   draw _ = group . map draw def

instance (Point_ point 2 r, Real r) => Drawable (Rectangle point) where
  type DrawableAttributes (Rectangle point) = PathAttributes
  draw ats r = draw ats $ ClosedPath $ toPoint <$> toNonEmpty (corners r)

instance (Point_ point 2 r, Real r) => Drawable (Disk point) where
  type DrawableAttributes (Disk point) = PathAttributes
  draw ats (Disk c rr) = let ats' = def&pathColor .~ StrokeAndFill Colour.black Colour.black
                         in draw (ats' <> ats) (toArc c rr)

instance (Point_ point 2 r, Real r) => Drawable (Circle point) where
  type DrawableAttributes (Circle point) = PathAttributes
  draw ats (Circle c rr) = let ats' = def&pathColor .~ StrokeOnly Colour.black
                           in draw (ats' <> ats) (toArc c rr)

-- | Renders a full circle arc segment
toArc      :: (Point_ point 2 r, Real r, Real r')
           => point -> r' -> PathSegment
toArc c rr = ArcPath (toPoint c) (sqrt $ realToFrac rr) 0 (2*pi)

-- | default radius of a point
defaultSqRadius :: R
defaultSqRadius = 2

instance Real r => Drawable (Point 2 r) where
  type DrawableAttributes (Point 2 r) = PathAttributes
  draw ats p = draw ats (Disk (toPoint p) defaultSqRadius)

toPoint   :: (Point_ point 2 r, Real r) => point -> Point 2 R
toPoint c = over coordinates realToFrac $ c^.asPoint

instance (Real r, Point_ point 2 r) => Drawable (PolyLine point) where
  type DrawableAttributes (PolyLine point) = PathAttributes
  draw ats pl = draw ats $ PolyPath $ toPoint <$> toNonEmptyOf allPoints pl

instance ( Real r, Point_ point 2 r
         , TraversableWithIndex Int f
         , Traversable1 f
         , IxValue (f point) ~ point
         , Index   (f point) ~ Int
         , Ixed    (f point)
         , HasDirectedTraversals f
         ) => Drawable (SimplePolygonF f point) where
  type DrawableAttributes (SimplePolygonF f point) = PathAttributes
  draw ats pg = draw ats $ ClosedPath $ toPoint <$> toNonEmptyOf allPoints pg

--------------------------------------------------------------------------------




-- -- | Canvas
-- data Drawing =
--     Blank
--   | Transform (Transformation 2 R)


--     (View action)
--   | Group [View action]

--   | Geom Geom (Attributes action)
--   deriving stock (Eq,Show,Functor,Foldable,Traversable)



-- newtype Drawing = Drawing [Geom]

-- -- | Geometric Objects that we can draw
-- data Geom =
--     PointGeom    (Point 2 R)
--   | RectGeom     (Rectangle (Point 2 R))
--   | PathGeom     (NonEmpty (Point 2 R))
--   | PolygonGeom  (NonEmpty (Point 2 R))
--   | TextGeom     (Point 2 R) Text
--   | ArcGeom      (Point 2 R) R R R
--     -- ^ center, radius, starting angle, cw ending angle in radians.
--   deriving stock (Show,Eq,Ord)
