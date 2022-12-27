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
import qualified Graphics.Rendering.Cairo as Cairo
import qualified SDL
import           SDL (V4(..), ($=), rendererDrawColor, V2(..))
import qualified SDL.Cairo
import qualified SDL2MVC.Event as Event

import Debug.Trace

--------------------------------------------------------------------------------


data Attribute = Fill
               | Stroke
               | OnEvent Event.Event
               deriving (Show,Ord,Eq)

data AttributeAssignment action =
  AttrAssignment {-# UNPACK#-} !Attribute


newtype Attributes action = Attributes (Map.Map Event.Event action)
  deriving stock (Show,Functor,Foldable,Traversable)
  deriving newtype (Semigroup,Monoid)

instance Eq (Attributes action) where
  -- | Tets only whether the keys are the same.
  (Attributes m) == (Attributes m') = Map.keys m == Map.keys m'
  -- TODO: this is not the right thing to do in the end ...





type Rectangle = SDL.Rectangle R

type R = Double

data Transformation r = Translate (V2 r)
                      | Scale (V2 r)
                      | Rotate r
                      deriving stock (Eq,Show)

interpret :: Transformation R -> Cairo.Render ()
interpret = \case
  Translate (SDL.V2 x y) -> Cairo.translate x y
  Scale     (SDL.V2 x y) -> Cairo.scale x y
  Rotate alpha           -> Cairo.rotate alpha


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
runRender         :: SDL.Texture -> View action -> IO ()
runRender texture = SDL.Cairo.withCairoTexture texture . go
  where
    go :: View action -> Cairo.Render ()
    go = \case
      Blank         -> pure ()
      Colored c d   -> do let (SDL.V4 r g b a) = (\x -> realToFrac x / 255.0) <$> traceShowId c
                          Cairo.setSourceRGBA r g b a
                          go d
                          Cairo.fill
        -- rendererDrawColor renderer $= c
        --                   go d
      Transform t d -> do Cairo.save
                          interpret t
                          go d
                          Cairo.restore

      Group ds      -> mapM_ go ds

      Point p ats   -> do let (SDL.P (SDL.V2 px py)) = p
                          Cairo.arc px py 2 0 (2*pi)
      Rect r ats    -> do let SDL.Rectangle (SDL.P (SDL.V2 px py)) (SDL.V2 w h) = r
                          Cairo.rectangle px py w h
      Polygon pg    -> pure ()
