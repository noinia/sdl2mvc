{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import           Control.Lens
import           Data.Bifunctor
import           Data.Colour
import           Data.Colour.Names (red,blue,white,green)
import           Data.Colour.SRGB (RGB(..), toSRGB, sRGB24)
import           Data.Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import           Effectful
import           Effectful.Concurrent (threadDelay)
import           Effectful.Concurrent.Async (concurrently_)
import           Effectful.Concurrent.STM
import           HGeometry.Ext
import           HGeometry.Kernel
import           HGeometry.Interval
import           HGeometry.Transformation
import           HGeometry.Viewport
import           Linear (V2(..))
import qualified Linear.Affine as Linear
import qualified SDL
import           SDL2MVC
import           SDL2MVC.Layer
import qualified Vary

--------------------------------------------------------------------------------
-- * Model

type R = Double

data Canvas = Canvas { _canvasViewPort  :: Viewport R
                     , _zoomConfig      :: !(ZoomConfig R)
                     }
            deriving (Show,Eq)

makeLenses ''Canvas

-- instance HasZoomLevel Canvas R where
--   zoomLevel = zoomConfig.currentLevel

data Model = Model { _mousePosition :: !(Maybe (Linear.Point V2 Int))
                   , _layers        :: Seq.Seq Layer
                   , _mainCanvas    :: Canvas
                   , _nextLayerName :: NonEmpty.NonEmpty LayerName -- should be a stream
                   }
           deriving (Show,Eq)

makeLenses ''Model


defaultModel :: Model
defaultModel = Model { _mousePosition = Nothing
                     , _layers        = mempty
                     , _mainCanvas    = theCanvas
                     , _nextLayerName = NonEmpty.fromList
                                      $ cycle ["alpha","beta","gamma","delta"]
                     }
  where
    V2 w h = realToFrac <$> def @AppSettings ^.windowConfig.to SDL.windowInitialSize

    theCanvas = Canvas { _canvasViewPort  = graphicsOrigin
                                          $ Rect mainPanelWidth menuBarHeight w (mainHeight h)
                       , _zoomConfig      = ZoomConfig (ClosedInterval 0.01 10) 1
                       }

--------------------------------------------------------------------------------
-- * Controller

data Action = AddLayer LayerName Drawing
            deriving (Show,Eq)

type Messages = [SDL.Event, Action]


data ZoomDirection = ZoomIn | ZoomOut deriving (Show,Read,Eq)

-- | The actual update function
controller           :: forall es inMsgs msgs.
                        ( msgs  ~ [SDL.Event, Action]
                        , Send' Model msgs :> es
                        )
                     => Handler es Model (All Model msgs) msgs
controller model msg = case first Vary.intoOnly $ Vary.pop msg of
    Right e -> case SDL.eventPayload e of
        SDL.MouseMotionEvent mouseData -> let p = fromIntegral <$> SDL.mouseMotionEventPos mouseData
                                          in pure $ Changed (model&mousePosition ?~ p)

        SDL.MouseButtonEvent mouseData -> case SDL.mouseButtonEventMotion mouseData of
          SDL.Pressed -> case asPoint' <$> model^.mousePosition of
                           Nothing -> pure Unchanged
                           Just p' -> do let p              = toWorldIn (canvas^.canvasViewPort) p'
                                             (layer,model') = takeNextLayer model
                                         sendMsg @(All Model msgs) $ AddLayer layer (drawPt p)
                                         pure $ Changed model'
          _           -> pure Unchanged



        SDL.MouseWheelEvent mouseData -> let V2 _ delta = SDL.mouseWheelEventPos mouseData
                                             dir   = if delta < 0 then ZoomOut else ZoomIn
                                         in pure $ model&mainCanvas %%~ updateZoom dir

        _                              -> pure Unchanged

    Left act -> case act of
                  AddLayer name d -> pure $ Changed (model&layers %~ (Seq.:|> Layer name Visible d))


  where
    canvas = model^.mainCanvas

-- | Update the canvas by either  zooming in or zooming out.
-- zooming happens w.r.t the center of the viewport
updateZoom            :: ZoomDirection -> Canvas -> Updated Canvas
updateZoom dir canvas
    | z' == z   = Unchanged
    | otherwise = Changed $ canvas&zoomConfig.currentLevel .~ z'
                                  &canvasViewPort          %~ applyScaling delta
  where
    z  = canvas^.zoomConfig.currentLevel
    z' = clampTo (canvas^.zoomConfig.range) (z + delta)
    delta = case dir of
              ZoomIn  -> 0.05
              ZoomOut -> (-1)*0.05

    applyScaling delta' vp = vp&worldToHost %~ (|.| uniformScalingWrt c (1+delta'))
      where
        c = toWorldIn vp $ centerPoint $ vp^.viewPort


-- uniformScalingWrt          :: (Point_ point d r, Vector)
--                            =>
uniformScalingWrt p lambda =
  translation (p^.vector) |.| uniformScaling lambda |.| translation (negated $ p^.vector)


drawPt p = draw $ Disk p 25 :+ (def&pathColor .~ StrokeAndFill def (opaque red))

asPoint'   :: (Point_ point 2 r, Real r) => point -> Point 2 R
asPoint' p = p^.asPoint & coordinates %~ realToFrac

takeNextLayer model = ( NonEmpty.head $ model^.nextLayerName
                      , model&nextLayerName %~ NonEmpty.fromList . NonEmpty.tail
                      )





--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- * View

myDraw              :: Model -> View es msgs
myDraw model screen = case (\p -> ((p^.asPoint)&coordinates %~ fromIntegral :: Point 2 Double))
                           <$> model^.mousePosition of
    Nothing -> liftIO $ print "cursor outside screen"
    Just p  -> do renderDrawingIn (screen^.target)
                    [ draw $ Blank (opaque white) -- background
                    , myUI' model screen
                    , cursor p
                    ]

  where
    cursor p = draw $ Disk p 4 :+ (def&pathColor .~ FillOnly (opaque green))


myUI'              :: Model -> RenderTarget -> Drawing
myUI' model screen = draw [ draw menuBar
                          , mainSection
                          , draw footer
                          ]
  where
    Vector2 w h = size $ screen^.target.viewPort
    menuBar = drawIn (alignedOrigin $ Rect 0 0 w menuBarHeight) $
              Blank menuBarColor

    footer  = drawIn (alignedOrigin $ Rect 0 (h-footerHeight) w footerHeight) $
              [ draw $ Blank menuBarColor
              , draw $ TextLabel (Text.show $ model^.mousePosition) (Point2 5 20)
                       :+ (def&textColor .~ opaque white)
              ]

    mainSection = draw [ mainPanel
                       , mainArea
                       ]

    mainPanel = drawIn mainPanelVP
                  [ draw $ Blank panelColor
                  , rows [ button (def&pathColor .~ StrokeAndFill def (opaque red))
                                  [ draw $ TextLabel (l^.layerName) (origin @(Point 2 Double))
                                    :+ (def &textColor .~ opaque white)
                                  ]
                         | l <- model^..layers.traverse
                         ]
                  ] <> draw (mainPanelVP^.viewPort :+ def @PathAttributes)


    mainArea  = drawIn mainAreaVP
                  [ draw $ Blank (opaque white)
                  , foldMapOf (layers.traverse) drawLayer model
                  , draw $ Rect 0 0 100 (100 :: Double)
                         :+ (def&pathColor .~ StrokeAndFill def (red `withOpacity` 0.5))
                  ] <> draw (mainAreaVP^.viewPort :+ def @PathAttributes)

    mainPanelVP = alignedOrigin $ Rect 0 menuBarHeight w (mainHeight h)
    -- mainAreaVP  = normalizedCenteredOrigin $ Rect mainPanelWidth menuBarHeight w mainHeight
    mainAreaVP  = model^.mainCanvas.canvasViewPort


----------------------------------------

mainHeight h = h-footerHeight-menuBarHeight

menuBarHeight = 20
footerHeight  = 200

mainPanelWidth = 200

panelColor       = opaque $ sRGB24 249 250 251
menuBarColor     = opaque $ sRGB24 74 84 100
menuBarTextColor = opaque $ sRGB24 142 149 160

rows :: [Drawing] -> Drawing
rows = fold . snd . List.mapAccumL (\acc g -> (acc + 20, translateBy (Vector2 0 acc) g)) 0

--------------------------------------------------------------------------------

button             :: PathAttributes
                   -> [Drawing]
                   -> Drawing
button ats content = draw [ draw $ rect :+ ats
                          , drawIn (normalizedCenteredOrigin rect) content
                          ]
  where
    rect = Rect 0 0 mainPanelWidth 20

--------------------------------------------------------------------------------


-- | Debug
traceDraw        :: Drawable g
                 => String
                 -- ^ LayerNam e
                 -> g -> g
traceDraw name g = undefined -- unsafePerformIO $ runEff $ traceDraw' name g
{-# NOINLINE traceDraw #-}
-- TODO: we should get an IO op first, before we can unsafePerformIO it


-- | Send a trace message
traceDraw'        :: forall es g.
                     (Send' Model Messages :> es, Drawable g)
                  => String -> g -> Eff es ()
traceDraw' name g = sendMsg @(All Model Messages) $ AddLayer (Text.pack name) (draw g)




main :: IO ()
main = runEff . runDebuggerWith $ \app -> do
          liftIO $ putStrLn "woei"
          threadDelay 3_000_000
          traceDraw' "debugging" (Rect 0 10 100 (200 :: Double) :+ (def @PathAttributes))

debuggerConfig :: ( es ~ Send' Model Messages : Concurrent : os
                  , IOE :> os
                  )
               => AppConfig es Model Messages
debuggerConfig = AppConfig
                 { _appModel        = defaultModel
                 , _handler         = const controller
                 , _initialMessages = []
                 , _appRender       = myDraw
                 , _settings        = def&windowTitle .~ "HGeometry Debugger"
                 }

runDebuggerWithIO     :: IO () -> IO ()
runDebuggerWithIO act = runEff . runDebuggerWith $ const (liftIO act)


runDebuggerWith     :: ( es ~ Send' Model Messages : Concurrent : os
                       , IOE :> os
                       )
                    => (App es Model Messages -> Eff es ()) -> Eff os ()
runDebuggerWith act = withSDLApp debuggerConfig $ \app ->
  concurrently_ (runApp' app) (act app)
