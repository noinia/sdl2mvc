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
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmtpy
import qualified Data.Map as Map
import           Data.Word
import           Foreign.C.Types (CInt)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified SDL
import           SDL (V4(..), V2(..), Point(..), Rectangle(..), _x, _y)
import qualified SDL.Cairo
import           SDL2MVC.Attribute
import qualified SDL2MVC.Event as Event

--------------------------------------------------------------------------------

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

-- | Geometric Objects that we can draw
data Geom =
    PointGeom    (Point V2 R)
  | RectGeom     (Rectangle R)
  | PathGeom     (NonEmpty (Point V2 R))
  | PolygonGeom  (NonEmpty (Point V2 R))
  deriving stock (Show,Eq,Ord)

-- | A drawing is someting that we can render. Elements may contain
-- actions that we can trigger.
data View action =
    Blank

  -- | Colored (V4 Word8)   (View action)
  | Transform (Transformation R) (View action)
  | Group [View action]

  | Geom Geom (Attributes action)
  deriving stock (Eq,Show,Functor,Foldable,Traversable)

--------------------------------------------------------------------------------

-- | Given a renderer and a drawing that we wish to draw; render the drawing
runRender            :: SDL.Texture
                     -> CInt -- ^ Height of the texture we are rendering to
                     -> View action -> IO ()
runRender texture h = SDL.Cairo.withCairoTexture texture . withMathCoords h . go
  where
    go :: View action -> Cairo.Render ()
    go = \case
      Blank         -> pure ()
      Transform t d -> do Cairo.save
                          interpret t
                          go d
                          Cairo.restore

      Group ds      -> mapM_ go ds

      Geom g attrs  -> withAttrs attrs $ renderGeom g


    renderGeom = \case
      PointGeom p    -> do let (SDL.P (SDL.V2 px py)) = p
                           Cairo.arc px py 2 0 (2*pi)
      RectGeom r     -> do let Rectangle (P (V2 px py)) (V2 w h) = r
                           Cairo.rectangle px py w h
      PathGeom vs    -> renderPoly vs
      PolygonGeom vs -> do renderPoly vs
                           Cairo.closePath

    renderPoly (p:|vs) = do Cairo.newPath
                            Cairo.moveTo (p^._x) (p^._y)
                            mapM_ (\q -> Cairo.lineTo (q^._x) (q^._y)) vs

    withAttrs ats r = foldrAttrs (flip withAttr) r ats

    withAttr   :: Cairo.Render () -> AttrAssignment action -> Cairo.Render ()
    withAttr k = \case
      Fill      :=> c -> do let (V4 r g b a) = toRGBA c
                            Cairo.setSourceRGBA r g b a
                            k
                            Cairo.fill
      Stroke    :=> c -> do let (V4 r g b a) = toRGBA c
                            Cairo.setSourceRGBA r g b a
                            k
      OnEvent _ :=> _ -> pure ()

      -- Colored c d   -> do let (SDL.V4 r g b a) = (\x -> realToFrac x / 255.0) <$> c
      --                     Cairo.setSourceRGBA r g b a
      --                     go d
      --                     Cairo.fill
      --   -- rendererDrawColor renderer $= c
      --   --                   go d



-- | Flip the plane so that the origin is in the bottom left.
withMathCoords       :: CInt -> Cairo.Render () -> Cairo.Render ()
withMathCoords h act = do Cairo.save
                          Cairo.scale 1 (-1)
                          Cairo.translate 0 (negate $ realToFrac h)
                          act
                          Cairo.restore
