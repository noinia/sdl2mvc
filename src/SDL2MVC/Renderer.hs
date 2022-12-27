{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  SDL2MVC.Renderer
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- MVC view on teh SDL Renderer
--
--------------------------------------------------------------------------------
module SDL2MVC.Renderer
  ( Renderer
  ) where

import           Control.Lens
import           Data.Text (Text)
import           SDL (present, clear)
import qualified SDL
import           SDL2MVC.View
import           SDL2MVC.Effect


--------------------------------------------------------------------------------

data Renderer action = Renderer { _drawing  :: View action
                                , _renderer :: SDL.Renderer
                                , _texture  :: SDL.Texture
                                }

createCairoTexture'
