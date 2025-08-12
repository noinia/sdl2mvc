{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens hiding (elements)
import           Data.Bifunctor
import           Data.Colour
import           Effectful
import qualified SDL
import           SDL2MVC
import qualified Vary

--------------------------------------------------------------------------------
-- * Model

--------------------------------------------------------------------------------
-- * Controller

--------------------------------------------------------------------------------
-- * View

--------------------------------------------------------------------------------

main :: IO ()
main = runEff $ SDL2MVC.runApp $
       AppConfig
         { _appModel        = defaultModel
         , _handler         = myHandler'
         , _initialMessages = []
         , _appRender       = myDraw
         , _settings        = def&windowTitle .~ "Demo"
         }
