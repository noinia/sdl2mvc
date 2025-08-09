{-# LANGUAGE TemplateHaskell #-}
module SDL2MVC.Layer
  ( Visible(..), toggle
  , Layer(Layer), layerName, visibility, layerDrawing
  , LayerName
  , drawLayer
  ) where

import Control.Lens
import Data.Text (Text)
import SDL2MVC.Drawing

--------------------------------------------------------------------------------

type LayerName = Text

data Visible = InVisible | Visible
  deriving (Show,Eq,Enum,Bounded)

toggle = \case
  InVisible -> Visible
  Visible   -> InVisible

data Layer = Layer { _layerName    :: !LayerName
                   , _visibility   :: {-#UNPACK#-}!Visible
                   , _layerDrawing :: Drawing
                   }
             deriving (Show,Eq)

makeLenses 'Layer

-- | Draw the layer (if it should be visible)
drawLayer                   :: Layer -> Drawing
drawLayer l = case l^.visibility of
    InVisible -> mempty
    Visible   -> l^.layerDrawing
