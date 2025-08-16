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
import           HGeometry.LineSegment
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
                   , _path          :: ClosedLineSegment (Point 2 R)
                   , _time          :: R
                   }
           deriving (Show,Eq)

makeLenses ''Model

defaultModel :: Model
defaultModel = Model { _mousePosition = Nothing
                     , _mainViewPort  = graphicsOrigin
                                      $ Rect mainPanelWidth menuBarHeight w (mainHeight h)
                     , _points        = mempty
                     , _time          = 0
                     , _path          = ClosedLineSegment (Point2 100 100) (Point2 200 100)
                     }
  where
    V2 w h = realToFrac <$> def @AppSettings ^.windowConfig.to SDL.windowInitialSize


--------------------------------------------------------------------------------
-- * Controller

data Action = StartAnimate
            | StopAnimate
            deriving (Show,Eq)

type Msgs = [SDL.Event, Action]


controller           :: forall es.
                       ( Send' Model Msgs :> es
                       , IOE :> es
                       )
                     => Handler es Model (All Model Msgs) Msgs
controller model msg = case first Vary.intoOnly $ Vary.pop msg of
  Right e -> case SDL.eventPayload e of


      SDL.MouseMotionEvent mouseData -> let p = fromIntegral <$> SDL.mouseMotionEventPos mouseData
                                        in pure $ Changed (model&mousePosition ?~ p)

      SDL.WindowShownEvent _         -> Unchanged <$ sendMsg @(All Model Msgs) Render
      SDL.WindowExposedEvent _       -> Unchanged <$ sendMsg @(All Model Msgs) Render

      SDL.MouseButtonEvent mouseData -> case SDL.mouseButtonEventMotion mouseData of
        SDL.Pressed -> case asPoint' <$> model^.mousePosition of
                         Nothing -> pure Unchanged
                         Just p'  -> let p                  = toWorldIn (model^.mainViewPort) p'
                                     in pure $ Changed (model&points %~ (p:))
        _           -> pure Unchanged

      SDL.TextInputEvent txtData -> case SDL.textInputEventText txtData of
        "s" -> Unchanged <$ sendMsg @(All Model Msgs) StartAnimate
        "t" -> Unchanged <$ sendMsg @(All Model Msgs) StopAnimate
        _   -> pure Unchanged
      _                              -> pure Unchanged

  Left act -> case act of
    StartAnimate -> do let until m | m^.time >= 1 = Stop
                                   | otherwise    = Continue

                       liftIO $ putStrLn "Started!"
                       sendMsg @(All Model Msgs) (Animate until)
                         -- hmm, this is not very useful at this point ...
                         -- time should somehow change
                       pure $ Changed (model&time .~ 0)
    StopAnimate  -> pure Unchanged


-- [Event {eventTimestamp = 2299
--   , eventPayload =


--     KeyboardEvent (KeyboardEventData {keyboardEventWindow = Just (Window 0x00006000029bc4b0)
--                                    , keyboardEventKeyMotion = Pressed
--                                    , keyboardEventRepeat = False
--                                    , keyboardEventKeysym =
--                                        Keysym {keysymScancode = Scancode {unwrapScancode = 22}
--                                               , keysymKeycode = Keycode {unwrapKeycode = 115}
--                                               , keysymModifier = KeyModifier
--                                                                  { keyModifierLeftShift = False
--                                                                  , keyModifierRightShift = False
--                                                                  , keyModifierLeftCtrl = False
--                                                                  , keyModifierRightCtrl = False
--                                                                  , keyModifierLeftAlt = False
--                                                                  , keyModifierRightAlt = False
--                                                                  , keyModifierLeftGUI = False
--                                                                  , keyModifierRightGUI = False
--                                                                  , keyModifierNumLock = False
--                                                                  , keyModifierCapsLock = False
--                                                                  , keyModifierAltGr = False}
--                                               }
--                                    })}


-- ,Event {eventTimestamp = 2300, eventPayload = TextInputEvent (TextInputEventData {textInputEventWindow = Just (Window 0x00006000029bc4b0), textInputEventText = "s"})}]


-- [Event {eventTimestamp = 2354, eventPayload = KeyboardEvent (KeyboardEventData {keyboardEventWindow = Just (Window 0x00006000029bc4b0), keyboardEventKeyMotion = Released, keyboardEventRepeat = False, keyboardEventKeysym = Keysym {keysymScancode = Scancode {unwrapScancode = 22}, keysymKeycode = Keycode {unwrapKeycode = 115}, keysymModifier = KeyModifier {keyModifierLeftShift = False, keyModifierRightShift = False, keyModifierLeftCtrl = False, keyModifierRightCtrl = False, keyModifierLeftAlt = False, keyModifierRightAlt = False, keyModifierLeftGUI = False, keyModifierRightGUI = False, keyModifierNumLock = False, keyModifierCapsLock = False, keyModifierAltGr = False}}})}]


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
                  , draw $ (model^.path) :+ def @PathAttributes
                  , drawPt (interpolate (model^.time) $ model^.path)
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
         , _handler         = const controller
         , _initialMessages = []
         , _appRender       = myDraw
         , _settings        = def&windowTitle .~ "Demo"
         }
