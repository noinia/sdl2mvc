{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  SDL2MVC.SDLApp
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- An SDLApp,
--
--------------------------------------------------------------------------------
module SDL2MVC.SDLApp
  ( SDLApp

  ) where

import Control.Lens
import Control.Monad
import Data.Text (Text)
import Data.Word
import SDL
import SDL2MVC.PublicModel

--------------------------------------------------------------------------------
-- * Model

-- | The complete SDL App, storing the app model, as well as
-- "internal" data that the framework itself needs.
data SDLApp = SDLApp { _appModel :: PublicModel
                     , _renderer :: Renderer
                     }
makeLenses ''SDLApp

--------------------------------------------------------------------------------
-- * Update


--------------------------------------------------------------------------------
-- * View
