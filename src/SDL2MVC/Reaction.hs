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

-- | The handler type
--
-- es     :: the effects the handler may perform
-- model  :: the model parameter
-- msgs   :: all messages that our app is supposed to handle
-- inMsgs :: the messages we can receive/handle
type Handler es model (msgs :: [Type]) inMsgs =
  model -> Vary.Vary inMsgs -> Eff es (Updated model)

--------------------------------------------------------------------------------
-- * Actions used in SDLApp

data Shutdown = Shutdown

data Render = Render
  deriving (Show,Eq)
