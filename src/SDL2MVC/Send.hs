{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module SDL2MVC.Send
  ( Send, Send'
  , sendMsg
  , SendMessage
  , sendMessage
  , runSendWith
  ) where

import           Data.Kind (Type)
import           Effectful
import           Effectful.Concurrent.STM
import           Effectful.Dispatch.Dynamic
import qualified Effectful.Dispatch.Dynamic as Eff
import           SDL2MVC.Reaction (All)
import qualified Vary
--------------------------------------------------------------------------------

type Send' model (msgs :: [Type]) = Send (All model msgs)

type Send (msgs :: [Type]) = SendMessage (Vary.Vary msgs)

data SendMessage msg :: Effect where
  SendMessage :: msg -> SendMessage msg m ()

type instance DispatchOf (SendMessage msg) = Dynamic

-- | Send a message
sendMsg :: forall msgs msg es. (msg Vary.:| msgs, Send msgs :> es, HasCallStack) => msg -> Eff es ()
sendMsg = sendMessage . Vary.from @msg @msgs

-- | Send message
sendMessage :: (SendMessage msg :> es, HasCallStack) => msg -> Eff es ()
sendMessage = Eff.send . SendMessage

-- | A way of implementing send
runSendWith       :: Concurrent :> es
                  => TBQueue msg -> Eff (SendMessage msg : es) a -> Eff es a
runSendWith queue = interpret $ \_ -> \case
    SendMessage msg -> atomically $ writeTBQueue queue msg
-- I want this to be pretty mcuh the only way of implemething send?

-- evalSend


-- data Send msg :: Effect

-- type instance DispatchOf (Send msg)  = Static WithSideEffects
-- newtype instance StaticRep (Send msg) = SendDispatch (TBQueue msg)

-- sendMessage     :: (Send msg :> es, Concurrent es) => msg -> Eff es ()
-- sendMessage msg = do SendDispatch queue <- getStaticRep
--                      writeTBQueue es
