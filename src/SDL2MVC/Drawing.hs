{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  SDL2MVC.Drawing
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data that we can render
--
--------------------------------------------------------------------------------
module SDL2MVC.Drawing
  ( Drawing(..)
  , View(..)
  ) where

import           Control.Lens
import           Control.Monad
import qualified Data.Map as Map
import           Data.Text (Text)
import           Data.Word
import           SDL

--------------------------------------------------------------------------------

data Event = OnClick
           | OnEnter
           | OnExit
           deriving (Show,Eq)

newtype Attributes action = Attributes (Map.Map Event action)
  deriving (Show,Eq)


type Rectangle = () -- placeholder

-- | A drawing is someting that we can render. Elements may contain
-- actions that we can trigger.
data Drawing action = Blank
                    | Filled (V4 Word8) (Drawing action)
                    | Rect Rectangle (Attributes action)
                    deriving (Eq,Show)


-- | The view, i.e. the thing we end up producing
newtype View action = View (Drawing action)

--------------------------------------------------------------------------------

-- | Given a renderer and a drawing that we wish to draw; render the drawing
render          :: Renderer -> Drawing action -> IO ()
render renderer = go
  where
    go = \case
      Blank      -> pure ()
      Filled c d -> do rendererDrawColor renderer $= c
                       go d
      Rect r ats -> pure () -- TODO
