{-# LANGUAGE TemplateHaskell #-}
module SDL2MVC.Drawing.Path
  ( PathAttributes(PathAttributes)
  , pathColor

  , Stroke(Stroke), strokeWidth, strokeColor
  , StrokeAndFillSpec(..)
  , _Stroke, _Fill
  ) where

import           Control.Lens
import           Data.Colour
import qualified Data.Colour as Colour
import           Data.Default.Class
import           SDL2MVC.Drawing.Color


--------------------------------------------------------------------------------


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
  (PathAttributes _) <> (PathAttributes c') = PathAttributes c'

instance Monoid PathAttributes where
  mempty = def
