--------------------------------------------------------------------------------
-- |
-- Module      :  SDL2MVC.Render
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Rendering geometric objects
--
--------------------------------------------------------------------------------
module SDL2MVC.Render
  ( geom_
  , rectangle_
  ) where

import qualified SDL
import           SDL2MVC.Attribute
import           SDL2MVC.View

--------------------------------------------------------------------------------

-- | Renders a geom
geom_      :: [AttrAssignment action] -> Geom -> View action
geom_ ats g = Geom g (fromAttrList ats)

-- | Renders a rectangle
rectangle_       :: Real r => [AttrAssignment action] -> SDL.Rectangle r -> View action
rectangle_ ats r = geom_ ats (RectGeom $ realToFrac <$> r)
