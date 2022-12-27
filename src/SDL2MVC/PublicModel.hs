{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
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
  ( -- * Model
    PublicModel(PublicModel)
  , drawing, title, theModel
  , createModel
  -- * Controller
  , Action(..)
  , update
  -- * View
  , rerender
  ) where

import           Control.Lens
import           Data.Text (Text)
import           SDL (Renderer, present, clear)
import           SDL2MVC.Drawing
import           SDL2MVC.Effect


--------------------------------------------------------------------------------
-- * Model

-- | The public part of the model of an SDL App; stores the main
-- application info like what we should draw, window titles etc.
data PublicModel action model = PublicModel { _drawing  :: Drawing action
                                            , _title    :: Text
                                            , _theModel :: model
                                            }
                              deriving stock (Eq)
makeLenses ''PublicModel

-- | Creates a model
createModel   :: model -> PublicModel action model
createModel m = PublicModel { _drawing  = Blank
                            , _title    = "SDLApp"
                            , _theModel = m
                            }

instance Bifunctor PublicModel where
  bimap f g m = m&drawing  %~ fmap f
                 &theModel %~ g


--------------------------------------------------------------------------------
-- * Controller

-- | Actions affecting the SDL Application that we support by default.
data Action action model = Redraw Renderer (model -> View action)
                         | UpdateTitle Text

-- instance Bifunctor Action where
--   bimap f g =


-- | The update function
update   :: PublicModel action model
         -> Action action model
         -> Effect () (PublicModel action model)
update m = \case
  Redraw renderer renderF -> let View drawing' = renderF (m^.theModel)
                             in (m&drawing .~ drawing') <# do runRender renderer drawing'
                                                              clear renderer
                                                              present renderer
  UpdateTitle t           -> noEff $ m&title .~ t

--------------------------------------------------------------------------------
-- * View

-- | Renders the public model, (by simply taking the drawing that is
-- stored and rendering it)
rerender   :: PublicModel action model -> View action
rerender m = View $ m^.drawing

-- -- | Rerenders the current view based on the existing drawing
-- redraw            :: Renderer -> Drawing action -> IO ()
-- redraw renderer d =
