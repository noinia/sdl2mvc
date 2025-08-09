{-# LANGUAGE TemplateHaskell #-}
module SDL2MVC.Drawing.Text
  ( TextLabel(..)
  , TextAttributes(TextAttributes), textColor, textSize, location
  ) where

import Control.Lens
import Data.Colour
import Data.Default.Class
import Data.Text (Text)
import HGeometry.Point
import HGeometry.Transformation
import HGeometry.Properties
import SDL2MVC.Drawing.Color

--------------------------------------------------------------------------------

data TextLabel = TextLabel { _labelText :: Text
                           , _location  :: Point 2 Double
                           }
               deriving (Show,Eq)

makeLenses 'TextLabel

type instance Dimension TextLabel = 2
type instance NumType   TextLabel = Double

instance IsTransformable TextLabel where
  transformBy t lbl = lbl&location %~ transformBy t


--------------------------------------------------------------------------------

data TextAttributes = TextAttributes { _textColor :: Color
                                     , _textSize  :: Double
                                     }
                      deriving (Show,Eq)

makeLenses 'TextAttributes


instance Default TextAttributes where
  def = TextAttributes (opaque black) 20
