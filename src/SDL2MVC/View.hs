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
import           Data.Maybe
import           Data.Text (Text)
import           Foreign.C.Types (CInt)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.Matrix as Matrix
import           HGeometry.Box
import           HGeometry.Matrix
import           HGeometry.Point
import           HGeometry.Interval
import           HGeometry.Transformation
import           HGeometry.Vector
import qualified SDL
import qualified SDL.Cairo
import           SDL2MVC.Attribute

--------------------------------------------------------------------------------

type R = Double

interpret   :: Transformation 2 R -> Cairo.Render ()
interpret t = Cairo.transform m
  where
    m = Matrix.Matrix a d b e c f
    Vector3 a b c = fromMaybe err $ row 0 (t^.transformationMatrix)
    Vector3 d e f = fromMaybe err $ row 1 (t^.transformationMatrix)
    err = error "SDL.view: interpet matrix does not have 2 rows!? "

-- | Geometric Objects that we can draw
data Geom =
    PointGeom    (Point 2 R)
  | RectGeom     (Rectangle (Point 2 R))
  | PathGeom     (NonEmpty (Point 2 R))
  | PolygonGeom  (NonEmpty (Point 2 R))
  | TextGeom     (Point 2 R) Text
  | ArcGeom      (Point 2 R) R R R
    -- ^ center, radius, starting angle, cw ending angle in radians.
  deriving stock (Show,Eq,Ord)

-- | A drawing is someting that we can render. Elements may contain
-- actions that we can trigger.
data View action =
    Blank

  -- | Colored (V4 Word8)   (View action)
  | Transform (Transformation 2 R) (View action)
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
    PointGeom p     -> do Cairo.arc (p^.xCoord) (p^.yCoord) 2 0 (2*pi)
    RectGeom r      -> do let (Point2 px py) = r^.minPoint
                              (Vector2 w h)  = size r
                          Cairo.rectangle px py w h
    PathGeom vs     -> renderPoly vs
    PolygonGeom vs  -> do renderPoly vs
                          Cairo.closePath
    TextGeom p t    -> do Cairo.moveTo (p^.xCoord) (p^.yCoord)
                          Cairo.textPath t
    ArcGeom c r s t -> do Cairo.arc (c^.xCoord) (c^.yCoord) r s t
  where
    renderPoly (p:|vs) = do Cairo.newPath
                            Cairo.moveTo (p^.xCoord) (p^.yCoord)
                            mapM_ (\q -> Cairo.lineTo (q^.xCoord) (q^.yCoord)) vs

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
