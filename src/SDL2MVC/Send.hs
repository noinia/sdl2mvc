{-# LANGUAGE TypeFamilyDependencies #-}
module SDL2MVC.Send
  ( Send
  , sendMessage
  , runSendWith
  ) where

import           Effectful
import           Effectful.Concurrent.STM
import           Effectful.Dispatch.Dynamic
import qualified Effectful.Dispatch.Dynamic as Eff

--------------------------------------------------------------------------------

data Send msg :: Effect where
  SendMessage :: msg -> Send msg m ()

type instance DispatchOf (Send msg) = Dynamic

sendMessage :: (Send msg :> es, HasCallStack) => msg -> Eff es ()
sendMessage = Eff.send . SendMessage

-- | A way of implementing send
runSendWith       :: Concurrent :> es
                  => TBQueue msg -> Eff (Send msg : es) a -> Eff es a
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
