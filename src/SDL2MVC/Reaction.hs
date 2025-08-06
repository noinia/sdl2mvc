module SDL2MVC.Reaction
  ( Reaction(..)
  , noEff
  , (<#)

  , Handler

  , LoopAction(..)
  , Render(..)
  ) where
--------------------------------------------------------------------------------


import           Control.Concurrent.Async (mapConcurrently_, withAsync, uninterruptibleCancel)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as Queue
import           Control.Lens
import           Effectful
import           GHC.Natural
import           Linear
import qualified SDL
import           SDL2MVC.Cairo

--------------------------------------------------------------------------------

data Reaction m model action = Reaction model [m action]

noEff       :: model -> Reaction m model action
noEff model = Reaction model []

infix <#

(<#)         :: model -> m action -> Reaction m model action
model <# act = Reaction model [act]


type Handler m model action = model -> action -> Reaction m model (LoopAction action)


--------------------------------------------------------------------------------
-- * Actions used in SDLApp

data LoopAction action = Shutdown
                       | Continue action
                       deriving (Show,Eq)

data Render = Render
  deriving (Show,Eq)
