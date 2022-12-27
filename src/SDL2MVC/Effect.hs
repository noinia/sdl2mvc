--------------------------------------------------------------------------------
-- |
-- Module      :  SDL2MVC.Effect
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- An Effect in an SDLApp, i.e. the return type for the controller
--
--------------------------------------------------------------------------------
module SDL2MVC.Effect
  ( Effect(..)
  , noEff
  , (<#)
  ) where

import Data.Bifunctor

--------------------------------------------------------------------------------

-- | The return type to use for te update function of the controller;
-- an effect is essentially a pair of te new model, and a bunch of
-- actions to run.
--
--
data Effect action model = Effect model [IO action]
                         deriving (Functor,Foldable,Traversable)

instance Bifunctor Effect where
  bimap f g (Effect m as) = Effect (g m) (fmap f <$> as)


-- | Return the model as is; don't schedule any actions.
noEff       :: model -> Effect action model
noEff model = Effect model []

-- | Schedule a single action
(<#) :: model -> IO action -> Effect action model
model <# act = Effect model [act]
