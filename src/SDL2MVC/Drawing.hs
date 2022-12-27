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
  , runRender
  ) where

import           Control.Lens
import qualified Data.Map as Map
import           Data.Word
import           SDL (Renderer, V4(..), ($=), rendererDrawColor)
import qualified SDL2MVC.Event as Event
--------------------------------------------------------------------------------


newtype Attributes action = Attributes (Map.Map Event.Event action)
  deriving stock (Show,Functor,Foldable,Traversable)

instance Eq (Attributes action) where
  -- | Tets only whether the keys are the same.
  (Attributes m) == (Attributes m') = Map.keys m == Map.keys m'
  -- TODO: this is not the right thing to do in the end ...

type Rectangle = () -- placeholder

-- | A drawing is someting that we can render. Elements may contain
-- actions that we can trigger.
data Drawing action = Blank
                    | Filled (V4 Word8) (Drawing action)
                    | Rect Rectangle (Attributes action)
                    deriving stock (Eq,Show,Functor,Foldable,Traversable)


-- | The view, i.e. the thing we end up producing
newtype View action = View (Drawing action)

--------------------------------------------------------------------------------

-- | Given a renderer and a drawing that we wish to draw; render the drawing
runRender          :: Renderer -> Drawing action -> IO ()
runRender renderer = go
  where
    go = \case
      Blank      -> pure ()
      Filled c d -> do rendererDrawColor renderer $= c
                       go d
      Rect r ats -> pure () -- TODO
