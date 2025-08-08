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
import           HGeometry.Point
import           HGeometry.Box
import           HGeometry.Vector
import           HGeometry.Transformation
import           HGeometry.Viewport
import           SDL2MVC.Cairo

--------------------------------------------------------------------------------

-- | Handles a render action
handleRender             :: IOE :> es
                         => App es model msgs inMsgs
                         -> Handler es model outMsgs inMsgs'
                         -> Handler es model outMsgs (Render : inMsgs')
handleRender app handler = \model msg -> case Vary.pop msg of
    Right Render -> Unchanged <$ runRender app model
    Left msg'    -> handler model msg'

runRender            :: IOE :> es
                     => App es model msgs inMsgs
                     -> model
                     -> Eff es ()
runRender app model = do let renderer = app^.rendererRef
                             texture  = app^.textureRef
                         SDL.TextureInfo _ _ w h <- SDL.queryTexture texture
                         let screen = Rectangle origin (Point2 (realToFrac w) (realToFrac h))
                             vp     = mkViewport screen identity
                             renderTarget = RenderTarget texture vp
                         liftIO $ withCairoTexture texture $ do
                           (app^.config.appRender) model renderTarget
                         -- display the drawing
                         liftIO $ do
                           SDL.copy renderer texture Nothing Nothing
                           SDL.present renderer
