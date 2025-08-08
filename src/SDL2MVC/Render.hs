module SDL2MVC.Render
  ( handleRender
  ) where

import           Control.Lens
import           Effectful
import qualified SDL
import           SDL2MVC.App
import           SDL2MVC.Reaction
import           SDL2MVC.Updated
import qualified Vary

--------------------------------------------------------------------------------

-- | Handles a render action
handleRender             :: IOE :> es
                         => App es model msgs inMsgs
                         -> Handler es model inMsgs'
                         -> Handler es model (Render : inMsgs')
handleRender app handler = \model msg -> case Vary.pop msg of
    Right Render -> Unchanged <$ runRender app model
    Left msg'    -> handler model msg'

runRender            :: IOE :> es
                     => App es model msgs inMsgs
                     -> model
                     -> Eff es ()
runRender app model = do let renderer = app^.rendererRef
                             texture  = app^.textureRef
                         -- clear previous rendering
                         -- now render
                         (app^.config.appRender) model texture
                         -- display the drawing
                         liftIO $ SDL.copy renderer texture Nothing Nothing
                         liftIO $ SDL.present renderer
