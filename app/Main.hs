{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens hiding (elements)
import           Data.Bifunctor
import           Data.Colour
import           Data.Colour.Names (red,blue,white,green)
import           Data.Colour.SRGB (RGB(..), toSRGB, sRGB24)
import           Data.Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import           Effectful
import           HGeometry
import           HGeometry.Ball
import           HGeometry.ConvexHull
import           HGeometry.Ext
import           HGeometry.Polygon
import           HGeometry.Transformation
import           HGeometry.Viewport
import           Linear (V2(..))
import qualified Linear.Affine as Linear
import qualified SDL
import           SDL2MVC
import qualified Vary

--------------------------------------------------------------------------------
-- * Model

type R = Double

data Model = Model { _mousePosition :: !(Maybe (Linear.Point V2 Int))
                   , _mainViewPort  :: Viewport R
                   , _points        :: [Point 2 R]
                   }
           deriving (Show,Eq)

makeLenses ''Model

defaultModel :: Model
defaultModel = Model { _mousePosition = Nothing
                     , _mainViewPort  = graphicsOrigin
                                      $ Rect mainPanelWidth menuBarHeight w (mainHeight h)
                     , _points        = mempty
                     }
  where
    V2 w h = realToFrac <$> def @AppSettings ^.windowConfig.to SDL.windowInitialSize


--------------------------------------------------------------------------------
-- * Controller

data Action = AddPoint
            deriving (Show,Eq)

type Msgs = [Shutdown, Render, SDL.Event, Action]


-- | Handlers some default events already
withDefaultHandlers             :: forall msgs inMsgs msgs' es model.
                                   ( Send msgs :> es
                                   , IOE       :> es
                                   , msgs   ~ (Shutdown : inMsgs)
                                   , inMsgs ~ [Render, SDL.Event, Action]
                                   )
                                =>App  es Model msgs inMsgs
                                -> Model -> Vary inMsgs -> Eff es (Updated Model)
withDefaultHandlers app = handleRender app
                        $ withDefaultSDLEvents @msgs controller

controller           :: forall es inMsgs outMsgs.
                       ( inMsgs  ~ [SDL.Event, Action]
                       , outMsgs ~ (Shutdown : Render : inMsgs)
                       , Send outMsgs :> es
                       )
                     => Handler es Model outMsgs inMsgs
controller model msg = case first Vary.intoOnly $ Vary.pop msg of
  Right e -> case SDL.eventPayload e of
      SDL.MouseMotionEvent mouseData -> let p = fromIntegral <$> SDL.mouseMotionEventPos mouseData
                                        in pure $ Changed (model&mousePosition ?~ p)

      SDL.WindowShownEvent _         -> Unchanged <$ sendMsg @outMsgs Render
      SDL.WindowExposedEvent _       -> Unchanged <$ sendMsg @outMsgs Render

      SDL.MouseButtonEvent mouseData -> case SDL.mouseButtonEventMotion mouseData of
        SDL.Pressed -> case asPoint' <$> model^.mousePosition of
                         Nothing -> pure Unchanged
                         Just p'  -> let p                  = toWorldIn (model^.mainViewPort) p'
                                     in pure $ Changed (model&points %~ (p:))
        _           -> pure Unchanged

      _                              -> pure Unchanged

  Left act -> pure Unchanged

    -- case act of
    --             AddLayer name d -> pure $ Changed (model&layers %~ (Seq.:|> Layer name Visible d))



asPoint'   :: (Point_ point 2 r, Real r) => point -> Point 2 R
asPoint' p = p^.asPoint & coordinates %~ realToFrac

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

    mainPanel = drawIn mainPanelVP $
                  [ draw $ Blank panelColor
                  -- , rows [ button (def&pathColor .~ StrokeAndFill def (opaque red))
                  --                 [ draw $ textLabel (l^.layerName)
                  --                   :+ (def &textColor .~ opaque white)
                  --                 ]
                  --        | l <- model^..layers.traverse
                  --        ]
                  ]

    mainArea  = drawIn mainAreaVP $
                  [ draw $ Blank (opaque white)
                  , foldMapOf (points.traverse) drawPt model
                  , draw theHull
                  ]
    drawPt p = draw $ Disk p 25 :+ (def&pathColor .~ StrokeAndFill def (opaque red))

    mainPanelVP = alignedOrigin $ Rect 0 menuBarHeight w (mainHeight h)
    -- mainAreaVP  = normalizedCenteredOrigin $ Rect mainPanelWidth menuBarHeight w mainHeight
    mainAreaVP  = model^.mainViewPort

    theHull = case (do pts@(_ NonEmpty.:| _:_) <- NonEmpty.nonEmpty (model^.points)
                       let ch = convexHull pts
                       fromPoints (toNonEmptyOf outerBoundary ch))
                        :: Maybe (SimplePolygon (Point 2 Double))
                   of
                Nothing -> []
                Just ch -> [ draw $ ch :+ (def&pathColor .~ StrokeAndFill def (red `withOpacity` 0.5))
                           ]

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

main :: IO ()
main = runEff $ SDL2MVC.runApp $
       AppConfig
         { _appModel        = defaultModel
         , _handler         = withDefaultHandlers
         , _initialMessages = []
         , _appRender       = myDraw
         , _settings        = def&windowTitle .~ "Demo"
         }
