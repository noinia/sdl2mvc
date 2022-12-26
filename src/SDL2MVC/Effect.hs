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

--------------------------------------------------------------------------------

-- | The return type to use for te update function of the controller;
-- an effect is essentially a pair of te new model, and a bunch of
-- actions to run.
--
--
data Effect model action = Effect model [IO action]

-- | Return the model as is; don't schedule any actions.
noEff       :: model -> Effect model action
noEff model = Effect model []

-- | Schedule a single action
(<#) :: model -> IO action -> Effect model action
model <# act = Effect model [act]
