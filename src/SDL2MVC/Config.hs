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
module SDL2MVC.Config
  ( Settings(..)
  )

import Data.Text

data Settings = Settings { _windowTitle :: Text
                         }
