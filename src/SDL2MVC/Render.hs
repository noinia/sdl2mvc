module SDL2MVC.Render
  ( handleRender
  ) where

import           Control.Lens
import           Effectful
import qualified SDL
import           SDL2MVC.App
import           SDL2MVC.Reaction

--------------------------------------------------------------------------------


-- | Handles a render action
handleRender           :: IOE :> es
                       => App es model action -> model -> Render
                       -> Reaction (Eff es) model (LoopAction action)
handleRender app model = \case
    Render -> model <# do let renderer = app^.rendererRef
                              texture  = app^.textureRef
                          -- clear previous rendering
                          -- SDL.clear renderer
                          -- now render
                          act <- (app^.config.appRender) model texture
                          -- display the drawing
                          liftIO $ SDL.copy renderer texture Nothing Nothing
                          liftIO $ SDL.present renderer
                          pure $ Continue act


-- white :: SDL.V4 Word8
-- white = pure maxBound
