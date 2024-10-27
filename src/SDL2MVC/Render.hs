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
handleRender           :: MonadIO m
                       => App m model action -> model -> Render
                       -> Reaction m model (LoopAction action)
handleRender app model = \case
    Render -> model <# do let renderer = app^.rendererRef
                              texture  = app^.textureRef
                          -- clear previous rendering
                          SDL.clear renderer
                          -- now render
                          liftIO $ print "rendering"
                          act <- (app^.config.appRender) model texture
                          liftIO $ print "done rendering"
                          -- display the drawing
                          SDL.copy renderer texture Nothing Nothing
                          SDL.present renderer
                          pure $ Continue act
