module SDL2MVC.Render
  ( handleRender
  ) where

import           Control.Lens
import           Effectful
import qualified SDL
import           SDL2MVC.App
import           SDL2MVC.Reaction

--------------------------------------------------------------------------------

-- withRender




-- | Handles a render action
handleRender           :: IOE :> es
                       => App es model action -> model
                       -> Render
                       -> Eff es ()
handleRender app model = \case
    Render -> do let renderer = app^.rendererRef
                     texture  = app^.textureRef
                 -- clear previous rendering
                 -- SDL.clear renderer
                 -- now render
                 (app^.config.appRender) model texture
                 -- display the drawing
                 liftIO $ SDL.copy renderer texture Nothing Nothing
                 liftIO $ SDL.present renderer


-- runRender app model = do let renderer = app^.rendererRef
--                              texture  = app^.textureRef
--                          -- clear previous rendering
--                          -- SDL.clear renderer
--                          -- now render
--                          (app^.config.appRender) model texture
--                          -- display the drawing
--                          liftIO $ SDL.copy renderer texture Nothing Nothing
--                          liftIO $ SDL.present renderer


-- white :: SDL.V4 Word8
-- white = pure maxBound
