{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module SDL2MVC
  ( main
  ) where

import           Control.Concurrent.Async (mapConcurrently_, withAsync, uninterruptibleCancel)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as Queue
import           Control.Lens hiding (elements)
import           Data.Foldable (for_)
import           Debug.Trace
import           Effectful
import           GHC.Natural
import           Linear
import qualified Linear.Affine as Linear
import qualified SDL
import           SDL2MVC.App
import           SDL2MVC.Cairo
import           SDL2MVC.Framework
import           SDL2MVC.Reaction
import           SDL2MVC.Render


import           Data.Colour
import           Data.Colour.Names (red,blue,white,green,orange)
import           Data.Colour.SRGB (RGB(..), toSRGB)
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Matrix as Cairo
import           HGeometry.Ball
import           HGeometry.Box
import           HGeometry.Ext
import           HGeometry.Matrix
import           HGeometry.Point
import           HGeometry.Transformation
import           HGeometry.Triangle
import           HGeometry.Vector
import           HGeometry.Viewport

--------------------------------------------------------------------------------
-- * Model


data MyModel = MyModel { _mousePosition :: !(Maybe (Linear.Point V2 Int))
                       }
             deriving (Show,Eq)

makeLenses ''MyModel

defaultModel :: MyModel
defaultModel = MyModel { _mousePosition = Nothing
                       }


--------------------------------------------------------------------------------
-- * Controller


data MyAction = RenderAction Render
              | SDLEvent SDL.Event
              | Skip
              deriving (Show,Eq)


----------------------------------------


myHandler app model = \case
  RenderAction renderAct -> handleRender app model renderAct
  SDLEvent e                   -> case SDL.eventPayload e of
    SDL.MouseMotionEvent mouseData -> let p = fromIntegral <$> SDL.mouseMotionEventPos mouseData
                                      in (model&mousePosition ?~ p)
                                         <# do pure $ Continue (RenderAction Render)
    _                              -> noEff model
    -- SDL.WindowShownEvent _         -> model <# (pure $ Continue (RenderAction Render))
    -- SDL.WindowExposedEvent _       -> model <# (pure $ Continue (RenderAction Render))
    -- SDL.WindowGainedKeyboardFocusEvent _  -> model <# (pure $ Continue (RenderAction Render))

  Skip                   -> noEff model



--------------------------------------------------------------------------------
-- * View


headerHeight = 10
footerHeight = 20

paneWidth    = 100
toolBarWidth = 16




-- -- | draw on SDL texture with Render monad from Cairo
-- withCairoTexture     :: SDL.Texture -> Render () -> IO ()
-- withCairoTexture t m = withCairoTexture' t (\s -> renderWith s m)





type Attributes = AlphaColour Double
fillColor = id


toCairoMatrix   :: Real r => Matrix 3 3 r -> Cairo.Matrix
toCairoMatrix m = case m&elements %~ realToFrac of
  Matrix (Vector3
          (Vector3 a b c)
          (Vector3 d e f)
          _
         ) -> Cairo.Matrix a d b e c f

renderIn           :: Real r => Viewport r -> Cairo.Render () -> Cairo.Render ()
renderIn vp render = do
                        Cairo.save
                        Cairo.transform (vp^.worldToHost.transformationMatrix.to toCairoMatrix)
                        render
                        Cairo.restore


disk          :: (Point_ point 2 r, Real r)
              => Attributes -> Disk point -> Cairo.Render ()
disk ats disk = do setColor (fillColor ats)
                   let Point2 x y = disk^.center.asPoint
                                    & coordinates %~ realToFrac
                       r          = realToFrac $ disk^.squaredRadius
                   Cairo.arc x y (sqrt r) 0 (2*pi)
                   Cairo.fill

toRGBA col = ( toSRGB $ col `Data.Colour.over` black
             , alphaChannel col
             )

setColor col = let (RGB r g b, a) = toRGBA col
               in Cairo.setSourceRGBA r g b a

rectangle          :: (Point_ point 2 r, Real r)
                   => Attributes -> Rectangle point -> Cairo.Render ()
rectangle ats rect = do let Point2 x y  = rect^.minPoint.asPoint
                                          & coordinates %~ realToFrac
                            Vector2 w h = realToFrac <$> size rect
                        setColor $ fillColor ats
                        Cairo.rectangle x y w h
                        Cairo.fill

triangle         :: (Point_ point 2 r, Real r)
                 => Attributes -> Triangle point -> Cairo.Render ()
triangle ats tri = do let Triangle a b c = toPoints tri
                      setColor $ fillColor ats
                      Cairo.moveTo (a^.xCoord) (a^.yCoord)
                      Cairo.lineTo (b^.xCoord) (b^.yCoord)
                      Cairo.lineTo (c^.xCoord) (c^.yCoord)
                      Cairo.closePath
                      Cairo.fill

toPoints :: (Functor f, Point_ point 2 r, Real r) => f point -> f (Point 2 Double)
toPoints = fmap (\p -> p^.asPoint &coordinates %~ realToFrac)


myRectangles = [ Rectangle (Point2 10 10)   (Point2 200 100) :+ opaque red
               , Rectangle (Point2 100 100) (Point2 250 250) :+ opaque blue
               ]

myTriangles = [ Triangle origin (Point2 400 10) (Point2 430 30) :+ opaque orange
              ]

-- textLabel                    :: Attributes
--                              -> Point 2 Double
--                              -> Text
--                              -> Cairo.Render ()
-- textLabel ats (Point2 x y) t =


-- triangle :: Attributes -> Triangle (Point 2 Double) -> Cairo.Render ()
-- triangle


--------------------------------------------------------------------------------

-- cairoDraw _ _ =



--------------------------------------------------------------------------------


  -- do SDL.rendererDrawColor renderer SDL.$= V4 0 0 255 255
  --                      SDL.drawRect renderer (Just $ SDL.Rectangle origin (V2 200 100))
  --                      pure Skip


-- withRenderer :: Handler m model action'
--              -> App m model action -> model -> Either Render action -> Handler m model action'
-- withRenderer handler

-- withDefaultRender :: Handler m model action -> App m model action -> Handler m model action
-- withDefaultRender handler app model =


--------------------------------------------------------------------------------

--------------------------------------------------------------------------------


myDraw               :: MyModel -> View IO MyAction
myDraw model texture =
  -- do
  --   renderDiagramTo texture $ diagramDraw model
  --   pure Skip
  Skip <$ case fmap fromIntegral <$> model^.mousePosition of
    Nothing -> print "cursor outside screen"
    Just p  -> do SDL.TextureInfo _ _ w h <- SDL.queryTexture texture
                  withCairoTexture texture $ do
                    rectangle (opaque white) (Rectangle origin (Point2 w h))
                    disk (opaque green) (Disk p 4)
                    renderIn (flipY $ Vector2 w h) $ do
                      for_ myRectangles $ \(r :+ ats) -> rectangle ats r
                      for_ myTriangles  $ \(r :+ ats) -> triangle ats r

--------------------------------------------------------------------------------

main :: IO ()
main = runApp $
       AppConfig
         { _appModel        = defaultModel
         , _handler         = myHandler
         , _startupAction   = Nothing
         , _liftSDLEvent    = withDefaultSDLEvents SDLEvent
         , _liftRenderEvent = RenderAction
         , _appRender       = myDraw
         , _windowTitle     = "My SDL2MVC App"
         }
