--------------------------------------------------------------------------------
-- |
-- Module      :  SDL2MVC.Event
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Events that we support in SDL Apps
--
--------------------------------------------------------------------------------
module SDL2MVC.Event
  ( Event(..)
  ) where

-- | Events that we support
data Event = OnClick
           | OnEnter
           | OnExit
           deriving (Show,Eq,Ord)
