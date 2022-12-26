{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  SDL2MVC.PublicModel
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- The model part of an SDL Model, i.e. all public information in an
-- SDL app.
--
--------------------------------------------------------------------------------
module SDL2MVC.PublicModel
  ( PublicModel(PublicModel)
  , drawing, title
  ) where

import Control.Lens
import Control.Monad
import Data.Text (Text)
import Data.Word
import SDL
import SDL2MVC.Drawing
import SDL2MVC.Effect

--------------------------------------------------------------------------------
-- * Model

-- | The public part of the model of an SDL App; stores the main
-- application info like what we should draw, window titles etc.
data PublicModel action = PublicModel { _drawing :: Drawing action
                                      , _title   :: Text
                                      }
                        deriving (Show)
makeLenses ''PublicModel


--------------------------------------------------------------------------------
-- * Update

data Action action = UpdateTitle Text


update   :: PublicModel -> Action action -> Effect PublicModel action
update m = \case
  UpdateTitle t -> noEff $ m&title .~ t


--------------------------------------------------------------------------------
-- * View

view   :: PublicModel -> View action
view m = m^.drawing
