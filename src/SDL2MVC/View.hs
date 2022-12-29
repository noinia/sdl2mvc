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


  , Transformation(..)
  , Geom(..)
  ) where

import           Control.Lens
import           Data.Colour.SRGB (RGB(..), toSRGB)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text (Text)
import           Foreign.C.Types (CInt)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.Matrix as Matrix
import qualified SDL
import           SDL (V4(..), V2(..), Point(..), Rectangle(..), _x, _y)
import qualified SDL.Cairo
import           SDL2MVC.Attribute

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
  | TextGeom     (Point V2 R) Text
  | ArcGeom      (Point V2 R) R R R -- center, radius, starting angle, cw ending angle in radians.
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
runRender texture h = SDL.Cairo.withCairoTexture texture
                    . withMathCoords h
                    . runRender'

runRender' :: View action -> Cairo.Render ()
runRender' = \case
    Blank         -> pure ()
    Transform t d -> do Cairo.save
                        interpret t
                        runRender' d
                        Cairo.restore

    Group ds      -> mapM_ runRender' ds

    Geom g attrs  -> do Cairo.newPath
                        withAttrs attrs $ renderGeom g

renderGeom :: Geom -> Cairo.Render ()
renderGeom = \case
    PointGeom p     -> do let (SDL.P (SDL.V2 px py)) = p
                          Cairo.arc px py 2 0 (2*pi)
    RectGeom r      -> do let Rectangle (P (V2 px py)) (V2 w h) = r
                          Cairo.rectangle px py w h
    PathGeom vs     -> renderPoly vs
    PolygonGeom vs  -> do renderPoly vs
                          Cairo.closePath
    TextGeom p t    -> do Cairo.moveTo (p^._x) (p^._y)
                          Cairo.textPath t
    ArcGeom c r s t -> do let (SDL.P (SDL.V2 cx cy)) = c
                          Cairo.arc cx cy r s t
  where
    renderPoly (p:|vs) = do Cairo.newPath
                            Cairo.moveTo (p^._x) (p^._y)
                            mapM_ (\q -> Cairo.lineTo (q^._x) (q^._y)) vs

withAttrs       :: Attributes action -> Cairo.Render () -> Cairo.Render ()
withAttrs ats r = foldrAttrs (flip withAttr) r ats

withAttr   :: Cairo.Render () -> AttrAssignment action -> Cairo.Render ()
withAttr k = \case
  Fill      :=> c -> do k
                        let (RGB r g b) = toSRGB c
                        Cairo.setSourceRGB r g b
                        Cairo.fillPreserve
  Stroke    :=> c -> do k
                        let (RGB r g b) = toSRGB c
                        Cairo.setSourceRGB r g b
                        Cairo.strokePreserve
  OnEvent _ :=> _ -> pure () -- TODO
  Opacity   :=> o -> pure () -- TODO


-- | Flip the plane so that the origin is in the bottom left.
withMathCoords       :: CInt -> Cairo.Render () -> Cairo.Render ()
withMathCoords h act = do Cairo.save
                          Cairo.scale 1 (-1)
                          Cairo.translate 0 (negate $ realToFrac h)
                          -- Cairo.setFontMatrix (Matrix.translate 0 (negate $ realToFrac h)
                          --                     (Matrix.scale 1 (-1) Matrix.identity))
                          act
                          Cairo.restore



--------------------------------------------------------------------------------

-- renderToSvg :: FilePath
--             -> V2 Double
--             -> View action
--             -> IO ()
-- renderToSvg fp (V2 w h) = withSVGSurface fp w h $ \surface ->
--                             do t <- SDL.createTextureFromSurface surface
