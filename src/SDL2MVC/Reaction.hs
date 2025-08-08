module SDL2MVC.Reaction
  ( Handler
  , Shutdown(..)
  , Render(..)
  ) where
--------------------------------------------------------------------------------


import           Control.Concurrent.Async (mapConcurrently_, withAsync, uninterruptibleCancel)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as Queue
import           Control.Lens
import           Data.Kind (Type)
import           Effectful
import           GHC.Natural
import           Linear
import qualified SDL
import           SDL2MVC.Cairo
import           SDL2MVC.Updated
import qualified Vary

--------------------------------------------------------------------------------

type Handler es model (outMsgs :: [Type]) inMsgs =
  model -> Vary.Vary inMsgs -> Eff es (Updated model)

--------------------------------------------------------------------------------
-- * Actions used in SDLApp

data Shutdown = Shutdown

data Render = Render
  deriving (Show,Eq)
