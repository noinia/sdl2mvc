{-# LANGUAGE TemplateHaskell #-}
module SDL2MVC.Drawing
  ( Drawing(..)
  , draw
  , DrawingElements

  , Blank(..)
  , TextLabel(..)
  , TextAttributes(TextAttributes), textColor, textSize, location

  , renderDrawingIn
  , Drawable(..)
  , Renderable(..)


  , Color
  , Stroke(Stroke), strokeWidth, strokeColor
  , StrokeAndFillSpec(..)
  , _Stroke, _Fill

  , PathAttributes(PathAttributes)
  , pathColor
  ) where

import           Control.Lens hiding (elements)
import           Data.Colour
import qualified Data.Colour as Colour
import           Data.Colour.Names (red,blue,white,green,orange)
import           Data.Colour.SRGB (RGB(..), toSRGB)
import           Data.Default.Class
import           Data.Foldable
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Matrix as CairoM
import           HGeometry.Ball
import           HGeometry.Box
import           HGeometry.Ext
import           HGeometry.Matrix
import           HGeometry.Point
import           HGeometry.Transformation
import           HGeometry.Triangle
import           HGeometry.Vector
import           HGeometry.Viewport
import qualified Vary

import Debug.Trace
--------------------------------------------------------------------------------


type Color = Colour.AlphaColour Double

data Stroke = Stroke { _strokeWidth :: {-#UNPACK#-}!Double
                     , _strokeColor :: !Color
                     }
            deriving (Show,Eq)

makeLenses ''Stroke

instance Default Stroke where
  def = Stroke 1 (opaque Colour.black)

-- | Paths must have stroke, fill, or both
data StrokeAndFillSpec =
    StrokeOnly  {-# UNPACK #-}!Stroke
  | FillOnly    {-# UNPACK #-}!Color
  | StrokeAndFill  {-# UNPACK #-}!Stroke -- ^ stroke spec
                   {-# UNPACK #-}!Color -- ^ fill color
    deriving stock (Show,Eq)

instance Default StrokeAndFillSpec where
  def = StrokeOnly def

-- | Lens to access the Stroke of a StrokeAndFill. This may set the stroke.
_Stroke :: Lens' StrokeAndFillSpec (Maybe Stroke)
_Stroke = lens (\case
                   StrokeOnly s      -> Just s
                   StrokeAndFill s _ -> Just s
                   _                 -> Nothing
               )
               (\spec -> \case
                   Nothing -> case spec of
                                StrokeAndFill _ f -> FillOnly f
                                _                 -> spec -- impossible to delete stroke
                   Just s  -> case spec of
                                StrokeOnly _      -> StrokeOnly s
                                StrokeAndFill _ f -> StrokeAndFill s f
                                FillOnly f        -> StrokeAndFill s f
               )

-- | Lens to access the fill color of a StrokeAndFill. This may set the stroke and fill
_Fill :: Lens' StrokeAndFillSpec (Maybe Color)
_Fill = lens (\case
                 FillOnly f        -> Just f
                 StrokeAndFill _ f -> Just f
                 _                 -> Nothing
             )
             (\spec -> \case
                 Nothing -> case spec of
                              StrokeAndFill s _ -> StrokeOnly s
                              _                 -> spec -- impossible to delete fill
                 Just f  -> case spec of
                              StrokeOnly s      -> StrokeAndFill s f
                              FillOnly _        -> FillOnly f
                              StrokeAndFill s _ -> StrokeAndFill s f
             )



data PathAttributes = PathAttributes { _strokeAndFill :: {-# UNPACK #-} !StrokeAndFillSpec
                                     }
  deriving stock (Show,Eq)

pathColor :: Lens' PathAttributes StrokeAndFillSpec
pathColor = lens _strokeAndFill (\ats c -> ats { _strokeAndFill = c} )

instance Default PathAttributes where
  def = PathAttributes def

instance Semigroup PathAttributes where
  -- ^ Later options may overwrite former ones.
  (PathAttributes c) <> (PathAttributes c') = PathAttributes c'

instance Monoid PathAttributes where
  mempty = def


--------------------------------------------------------------------------------

data TextAttributes = TextAttributes { _textColor :: Color
                                     , _textSize  :: Double
                                     , _location  :: Point 2 Double
                                     }
                      deriving (Show,Eq)

makeLenses 'TextAttributes


instance Default TextAttributes where
  def = TextAttributes (opaque black) 20 origin

--------------------------------------------------------------------------------
-- * Drawing

-- | The Main Drawing type
newtype Drawing = Drawing (Seq.Seq (Vary.Vary DrawingElements))
  deriving newtype (Show,Eq,Semigroup,Monoid,Renderable)

newtype Blank = Blank Color
  deriving (Show,Eq)

newtype TextLabel = TextLabel Text
  deriving (Show,Eq)


type DrawingElements = [ Rectangle (Point 2 Double) :+ PathAttributes
                       , Disk      (Point 2 Double) :+ PathAttributes
                       , Triangle  (Point 2 Double) :+ PathAttributes
                       , Blank
                       , TextLabel :+ TextAttributes
                       ]

draw :: g Vary.:| DrawingElements => g -> Drawing
draw = Drawing . Seq.singleton . Vary.from

--------------------------------------------------------------------------------

class Drawable t where
  drawIn :: Viewport Double -> t -> Drawing

instance Drawable t => Drawable [t] where
  drawIn vp = foldMap (drawIn vp)
instance Drawable t => Drawable (Seq.Seq t) where
  drawIn vp = foldMap (drawIn vp)


-- instance Drawable (Vary.Vary '[]) where
--   drawIn _ _ = mempty

-- instance (Drawable g, Drawable (Vary.Vary gs)) => Drawable (Vary.Vary (g:gs)) where
--   drawIn vp v = case Vary.pop v of
--                   Right g -> renderIn vp g
--                     Left v' -> renderIn vp v'


-- instance Drawable (Vary xs) where
--   drawIn vp =


instance Drawable Drawing where
  drawIn _ = id


renderDrawingIn      :: Drawable t => Viewport Double -> t -> Cairo.Render ()
renderDrawingIn vp g = renderIn vp $ drawIn vp g

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

toRGBA col = ( toSRGB $ col `Data.Colour.over` black
             , alphaChannel col
             )

setColor col = let (RGB r g b, a) = toRGBA col
               in Cairo.setSourceRGBA r g b a

rectangle          :: (Point_ point 2 r, Real r, Show point)
                   => PathAttributes -> Rectangle point -> Cairo.Render ()
rectangle ats rect | traceShow ("rectangle",rect) False = undefined
                   | otherwise =
                     withStrokeAndFill (ats^.pathColor) $ do
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
  renderIn :: Viewport Double -> t -> Cairo.Render ()

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
                                    (vp^.viewPort)

instance Renderable (TextLabel :+ TextAttributes) where
  renderIn _ (TextLabel t :+ ats) = do applyTextAttributes ats
                                       renderTextAt (ats^.location) t

applyTextAttributes     :: TextAttributes -> Cairo.Render ()
applyTextAttributes ats = do setColor (ats^.textColor)
                             Cairo.setFontSize (ats^.textSize)

instance Renderable (Rectangle (Point 2 Double) :+ PathAttributes) where
  renderIn _ (g :+ ats) = rectangle ats g

instance Renderable (Triangle (Point 2 Double) :+ PathAttributes) where
  renderIn _ (g :+ ats) = triangle ats g

instance Renderable (Disk (Point 2 Double) :+ PathAttributes) where
  renderIn _ (g :+ ats) = disk ats g
