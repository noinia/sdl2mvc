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
import           HGeometry.Ext
import           HGeometry.Kernel
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

data Model = Model { _mousePosition :: !(Maybe (Linear.Point V2 Int))
                   , _layers        :: Seq.Seq Layer
                   , _mainViewPort  :: Viewport R
                   , _nextLayerName :: NonEmpty.NonEmpty LayerName -- should be a stream
                   }
           deriving (Show,Eq)

makeLenses ''Model

defaultModel :: Model
defaultModel = Model { _mousePosition = Nothing
                     , _layers        = mempty
                     , _mainViewPort  = graphicsOrigin
                                        $ Rect mainPanelWidth menuBarHeight w (mainHeight h)
                     , _nextLayerName = NonEmpty.fromList
                                      $ cycle ["alpha","beta","gamma","delta"]
                     }
  where
    V2 w h = realToFrac <$> def @AppSettings ^.windowConfig.to SDL.windowInitialSize

--------------------------------------------------------------------------------
-- * Controller

data Action = AddLayer LayerName Drawing
            deriving (Show,Eq)

type Messages = [SDL.Event, Action]


-- -- | Handlers some default events already
-- myHandler             :: forall msgs msgs' es model.
--                                    ( Send msgs :> es
--                                    , IOE       :> es
--                                    , msgs ~ [SDL.Event, Action]
--                                    )
--                                 => App es Model msgs
--                                 -> Model -> Vary msgs -> Eff es (Updated Model)
-- myHandler app = handleRender rendererData
--               $ withDefaultSDLEvents @msgs controller
--   where
--     rendererData = RendererData (app^.rendererRef) (app^.textureRef) (app^.config.appRender)

-- myHandler :: App es Model Messages Messages
--           -> Handler es Model Messages Messages
-- myHandler = withDefaultHandlers controller

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
                         Just p' -> do let p              = toWorldIn (model^.mainViewPort) p'
                                           (layer,model') = takeNextLayer model
                                       sendMsg @(All Model msgs) $ AddLayer layer (drawPt p)
                                       pure $ Changed model'
        _           -> pure Unchanged

      _                              -> pure Unchanged

  Left act -> case act of
                AddLayer name d -> pure $ Changed (model&layers %~ (Seq.:|> Layer name Visible d))


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

    mainPanel = drawIn mainPanelVP $
                  [ draw $ Blank panelColor
                  , rows [ button (def&pathColor .~ StrokeAndFill def (opaque red))
                                  [ draw $ TextLabel (l^.layerName) (origin @(Point 2 Double))
                                    :+ (def &textColor .~ opaque white)
                                  ]
                         | l <- model^..layers.traverse
                         ]
                  ]

    mainArea  = drawIn mainAreaVP $
                  [ draw $ Blank (opaque white)
                  , foldMapOf (layers.traverse) drawLayer model
                  ]

    mainPanelVP = alignedOrigin $ Rect 0 menuBarHeight w (mainHeight h)
    -- mainAreaVP  = normalizedCenteredOrigin $ Rect mainPanelWidth menuBarHeight w mainHeight
    mainAreaVP  = model^.mainViewPort


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
                     (Send Messages :> es, Drawable g)
                  => String -> g -> Eff es ()
traceDraw' name g = sendMsg @Messages $ AddLayer (Text.pack name) (draw g)



main :: IO ()
main = runEff $ runApp $
       AppConfig
         { _appModel        = defaultModel
         , _handler         = const controller
         , _initialMessages = []
         , _appRender       = myDraw
         , _settings        = def&windowTitle .~ "HGeometry Debugger"
         }
