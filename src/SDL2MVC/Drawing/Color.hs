module SDL2MVC.Drawing.Color
  ( Color
  ) where

import           Data.Colour
import qualified Data.Colour as Colour
import           Data.Colour.Names (red,blue,white,green,orange)
import           Data.Colour.SRGB (RGB(..), toSRGB)


--------------------------------------------------------------------------------

type Color = Colour.AlphaColour Double
