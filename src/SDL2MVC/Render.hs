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
  , group_
  , text_
  , point_, point'_
  , circle_
  , arc_
  ) where

import Control.Lens
import Control.Subcategory.Functor
import Data.Colour.Names (black)
import Data.Text (Text)
import HGeometry.Box
import HGeometry.Point
import HGeometry.Vector
import SDL2MVC.Attribute
import SDL2MVC.View

--------------------------------------------------------------------------------

-- | Renders a geom
geom_      :: [AttrAssignment action] -> Geom -> View action
geom_ ats g = Geom g (fromAttrList ats)

-- | Renders a rectangle
rectangle_       :: ( Real r
                    , OptCVector_ 2 r, OptMetric_ 2 r
                    , HasComponents (Vector 2 r) (Vector 2 Double)
                    ) => [AttrAssignment action] -> Rectangle (Point 2 r) -> View action
rectangle_ ats r = geom_ ats (RectGeom $ over coordinates realToFrac <$:> r)

-- | Constructs a group
group_ :: [View action] -> View action
group_ = Group

-- | Renders text at a given position
text_         :: ( Real r
                 , OptCVector_ 2 r, OptMetric_ 2 r
                 , HasComponents (Vector 2 r) (Vector 2 Double)
                 ) => [AttrAssignment action] -> Point 2 r -> Text -> View action
text_ ats p t = geom_ ats (TextGeom (p&coordinates %~ realToFrac) t)

-- | Renders a filled point
point_     :: ( Real r
              , OptCVector_ 2 r, OptMetric_ 2 r
              , HasComponents (Vector 2 r) (Vector 2 Double)
              ) => [AttrAssignment action] -> Point 2 r -> View action
point_ ats = point'_ (ats <> [Fill :=> black])

-- | Raw point, without any attributes
point'_       :: ( Real r
                 , OptCVector_ 2 r, OptMetric_ 2 r
                 , HasComponents (Vector 2 r) (Vector 2 Double)
                 ) => [AttrAssignment action] -> Point 2 r -> View action
point'_ ats p = geom_ ats (PointGeom (p&coordinates %~ realToFrac))


-- | Draws an arc
arc_              :: ( Real r
                     , OptCVector_ 2 r, OptMetric_ 2 r
                     , HasComponents (Vector 2 r) (Vector 2 Double)
                     )
                  => [AttrAssignment action]
                  -> Point 2 r -- ^ center
                  -> r        -- ^ radius
                  -> r -- ^ starting angle in radians
                  -> r -- ^ stoppin angle in radians
                  -> View action
arc_ ats c r s t  =  geom_ ats
                  $ ArcGeom (c&coordinates %~ realToFrac)
                            (realToFrac r) (realToFrac s) (realToFrac t)

-- | Draws a circle with the given center and given radius
circle_          :: ( Real r, Floating r
                    , OptCVector_ 2 r, OptMetric_ 2 r
                    , HasComponents (Vector 2 r) (Vector 2 Double)
                    ) => [AttrAssignment action] -> Point 2 r -> r -> View action
circle_ ats c r = arc_ ats c r 0 (2*pi)
