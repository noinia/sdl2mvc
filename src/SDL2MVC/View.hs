{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  SDL2MVC.View
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data that we can render
--
--------------------------------------------------------------------------------
module SDL2MVC.View
  ( View(..)
  , runRender
  ) where

import           Control.Lens
import qualified Data.Map as Map
import           Data.Word
import qualified SDL
import           SDL (Renderer, V4(..), ($=), rendererDrawColor, V2(..))
import qualified SDL2MVC.Event as Event

--------------------------------------------------------------------------------


data Attribute = Fill
               | Stroke
               | OnEvent Event.Event
               deriving (Show,Eq)

data AttributeAssignment action =
  AttrAssignment {-# UNPACK#-} !Attribute


newtype Attributes action = Attributes (Map.Map Event.Event action)
  deriving stock (Show,Functor,Foldable,Traversable)

instance Eq (Attributes action) where
  -- | Tets only whether the keys are the same.
  (Attributes m) == (Attributes m') = Map.keys m == Map.keys m'
  -- TODO: this is not the right thing to do in the end ...

type Rectangle = SDL.Rectangle Int

type R = Double

data Transformation r = Translate (V2 r)
                      | Scale (V2 r)
                      | Rotate r
                      deriving stock (Eq,Show)

newtype Poly = Poly [V2 R]
  deriving (Show,Eq)


-- | A drawing is someting that we can render. Elements may contain
-- actions that we can trigger.
data View action =
    Blank

  | Colored (V4 Word8)   (View action)
  | Transform (Transformation R) (View action)
  | Group [View action]

  | Point (SDL.Point V2 R)  (Attributes action)
  | Rect  Rectangle      (Attributes action)
  | Polygon Poly
  deriving stock (Eq,Show,Functor,Foldable,Traversable)


--------------------------------------------------------------------------------

-- | Given a renderer and a drawing that we wish to draw; render the drawing
runRender          :: Renderer -> View action -> IO ()
runRender renderer = go
  where
    go = \case
      Blank         -> pure ()
      Colored c d   -> do rendererDrawColor renderer $= c
                          go d
      Transform t d -> do go d
      Group ds      -> mapM_ go ds

      Point p ats   -> pure ()
      Rect r ats    -> pure ()
      Polygon pg    -> pure ()
