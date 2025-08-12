{-# LANGUAGE AllowAmbiguousTypes #-}
module SDL2MVC.Render
  ( handleRender


  , Animate(..)
  , Continue(..)
  , handleAnimate
  ) where

import           Control.Lens
import           Effectful
import           Effectful.Concurrent (Concurrent, threadDelay)
import           HGeometry.Box
import           HGeometry.Point
import           HGeometry.Transformation
import           HGeometry.Vector
import           HGeometry.Viewport
import qualified SDL
import           SDL2MVC.App
import           SDL2MVC.Cairo
import           SDL2MVC.Reaction
import           SDL2MVC.Updated
import           SDL2MVC.Send
import qualified Vary

--------------------------------------------------------------------------------

-- | Handles a render action
handleRender              :: IOE :> es
                          => App es model msgs inMsgs
                          -> Handler es model outMsgs inMsgs'
                          -> Handler es model outMsgs (Render : inMsgs')
handleRender app handler' = \model msg -> case Vary.pop msg of
    Right Render -> Unchanged <$ runRender app model
    Left msg'    -> handler' model msg'

runRender            :: IOE :> es
                     => App es model msgs inMsgs
                     -> model
                     -> Eff es ()
runRender app model = do let renderer = app^.rendererRef
                             texture  = app^.textureRef
                         SDL.TextureInfo _ _ w h <- SDL.queryTexture texture
                         let screen = Rectangle origin (Point2 (realToFrac w) (realToFrac h))
                             vp     = Viewport screen identity
                             renderTarget = RenderTarget texture vp
                         liftIO $ withCairoTexture texture $ do
                           (app^.config.appRender) model renderTarget
                         -- display the drawing
                         liftIO $ do
                           SDL.copy renderer texture Nothing Nothing
                           SDL.present renderer




--------------------------------------------------------------------------------


data Continue = Stop | Continue
  deriving (Show,Eq,Bounded)

newtype Animate model = Animate (model -> Continue)


frameRate = 60

-- | Computes the right time to wait in microseconds for a given framerate
fromFrameRate      :: Int -> Int
fromFrameRate rate = 1_000_000 `div` rate

-- | Handles a render action
--
--
handleAnimate              :: forall outMsgs es model msgs inMsgs inMsgs'.
                              ( Concurrent   :> es
                              , Send outMsgs :> es
                              , Render Vary.:| outMsgs
                              , Animate model Vary.:| outMsgs
                              )
                           => App es model msgs inMsgs
                           -> Handler es model outMsgs inMsgs'
                           -> Handler es model outMsgs (Animate model : inMsgs')
handleAnimate app handler' = \model msg -> case Vary.pop msg of
    Right (Animate shouldContinue) -> Unchanged <$
                                      do sendMsg @outMsgs Render
                                         case shouldContinue model of
                                           Stop     -> pure ()
                                           Continue -> do threadDelay (fromFrameRate frameRate)
                                                          sendMsg @outMsgs $ Animate shouldContinue
    Left msg'    -> handler' model msg'
