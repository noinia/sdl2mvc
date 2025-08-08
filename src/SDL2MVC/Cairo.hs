module SDL2MVC.Cairo
  ( module SDL2MVC.Cairo.Raw
  ) where

import SDL2MVC.Cairo.Raw


-- import Effectful
-- import           Effectful.Dispatch.Dynamic
-- import qualified Effectful.Dispatch.Dynamic as Eff
-- import qualified SDL

-- --------------------------------------------------------------------------------

-- data CairoRender :: Effect where
--   CairoRender :: Cairo.Render a -> Cairo m a

-- type instance DispatchOf CairoRender = Dynamic

-- -- | Send message
-- cairo :: (CairoRender :> es, HasCallStack) => Cairo.Render a -> Eff es a
-- cairo = Eff.send . CairoRender

-- -- | A way of implementing send
-- runCairoRenderWith         :: IOE :> es
--                            => SDL.Texture -> Eff (CairoRender : es) a -> Eff es a
-- runCairoRenderWith texture = interpret $ \_ -> \case
--   CairoRender r -> liftIO $ Raw.withCairoTexture

--   SendMessage msg -> atomically $ writeTBQueue queue msg
-- -- I want this to be pretty mcuh the only way of implemething send?
