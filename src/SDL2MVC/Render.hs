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

import           Data.Colour.Names (black)
import           Data.Text (Text)
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

-- | Constructs a group
group_ :: [View action] -> View action
group_ = Group

text_         :: Real r => [AttrAssignment action] -> SDL.Point SDL.V2 r -> Text -> View action
text_ ats p t = geom_ ats (TextGeom (realToFrac <$> p) t)

-- | Renders a filled point
point_     :: Real r => [AttrAssignment action] -> SDL.Point SDL.V2 r -> View action
point_ ats = point'_ (ats <> [Fill :=> black])

-- | Raw point, without any attributes
point'_       :: Real r => [AttrAssignment action] -> SDL.Point SDL.V2 r -> View action
point'_ ats p = geom_ ats (PointGeom $ realToFrac <$> p)


arc_              :: Real r
                  => [AttrAssignment action]
                  -> SDL.Point SDL.V2 r
                  -> r
                  -> r
                  -> r
                  -> View action
arc_ ats c r s t  =  geom_ ats
                  $ ArcGeom(realToFrac <$> c) (realToFrac r) (realToFrac s) (realToFrac t)

circle_          :: (Real r, Floating r)
                 => [AttrAssignment action] -> SDL.Point SDL.V2 r -> r -> View action
circle_ ats c r = arc_ ats c r 0 (2*pi)
