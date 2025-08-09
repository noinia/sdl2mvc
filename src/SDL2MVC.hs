{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module SDL2MVC
  ( main
  ) where

import           Control.Concurrent.Async (mapConcurrently_, withAsync, uninterruptibleCancel)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as Queue
import           Control.Lens hiding (elements)
import           Data.Colour
import qualified Data.Colour as Colour
import           Data.Colour.Names (red,blue,white,green,orange)
import qualified Data.Colour.Names as Colour
import           Data.Colour.SRGB (RGB(..), toSRGB, sRGB24)
import           Data.Default.Class
import           Data.Foldable
import           Data.Foldable (for_)
import qualified Data.List as List
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import           Debug.Trace
import           Effectful
import           GHC.Natural
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Matrix as CairoM
import           HGeometry.Ball
import           HGeometry.Box
import           HGeometry.Ext
import           HGeometry.Point
import           HGeometry.Transformation
import           HGeometry.Triangle
import           HGeometry.Vector
import           HGeometry.Viewport
import           Linear(V2(..))
import qualified Linear.Affine as Linear
import qualified SDL
import           SDL2MVC.App
import           SDL2MVC.Cairo
import           SDL2MVC.Drawing
import           SDL2MVC.Framework
import           SDL2MVC.Layer
import           SDL2MVC.Reaction
import           SDL2MVC.Render
import           SDL2MVC.Send
import           SDL2MVC.Updated
import qualified Vary


import           SDL2MVC.Renderable(Box(..))
--------------------------------------------------------------------------------
-- * Model







data MyModel = MyModel { _mousePosition :: !(Maybe (Linear.Point V2 Int))
                       , _layers        :: Seq.Seq Layer
                       }
             deriving (Show,Eq)

makeLenses ''MyModel

defaultModel :: MyModel
defaultModel = MyModel { _mousePosition = Nothing
                       , _layers        = mempty
                       }


--------------------------------------------------------------------------------
-- * Controller

data MyAction = AddLayer LayerName Drawing
              deriving (Show,Eq)

-- data WithBasicActions = LoopAction RenderAction

----------------------------------------

toEither   :: forall l r. Vary.Vary [l,r] -> Either l r
toEither v = v&( Vary.on @l Left
               $ Vary.on @r Right
               $ Vary.exhaustiveCase
               )

-- myHandler          -- :: -- MyModel -> Vary '[SDL.Event, MyAction] -> Eff es (Updated MyModel)

myHandler           :: forall es inMsgs outMsgs.
                       ( inMsgs ~ [SDL.Event, MyAction]
                       , outMsgs ~ (Shutdown : Render : inMsgs)
                       , Send outMsgs :> es
                       )
                    => Handler es MyModel outMsgs inMsgs
myHandler model msg = case toEither msg of
    Left e -> case SDL.eventPayload e of
      SDL.MouseMotionEvent mouseData -> let p = fromIntegral <$> SDL.mouseMotionEventPos mouseData

                                                  in pure $ Changed (model&mousePosition ?~ p)

      SDL.WindowShownEvent _         -> Unchanged <$ sendMsg @outMsgs Render
      SDL.WindowExposedEvent _       -> Unchanged <$ sendMsg @outMsgs Render


      SDL.MouseButtonEvent mouseData -> case SDL.mouseButtonEventMotion mouseData of
        SDL.Pressed -> Unchanged <$ sendMsg @outMsgs (AddLayer "dummy" (draw myTri))
        _           -> pure Unchanged


      _                              -> pure Unchanged
    Right (AddLayer name d) -> pure $ Changed (model&layers %~ (Seq.:|> Layer name Visible d))


  where
    myTri :: Triangle (Point 2 Double) :+ PathAttributes
    myTri = scaleUniformlyBy 200 $
      Triangle (Point2 (-0.5) (-0.5))
                     (Point2 (0.5)  (-0.5))
                     (Point2 0       0.5)
            :+ (def&pathColor .~ StrokeAndFill def (opaque red))




myHandler'           :: forall msgs inMsgs es.
                        ( Send msgs :> es
                       , IOE       :> es
                       , msgs   ~ (Shutdown : inMsgs)
                       , inMsgs ~ [Render, SDL.Event, MyAction]
                       )
              => App es MyModel msgs inMsgs
              -> MyModel -> Vary inMsgs -> Eff es (Updated MyModel)
myHandler' app = handleRender app $ withDefaultSDLEvents @msgs myHandler


-- model = \case



--   RenderAction renderAct -> Unchanged <$ handleRender app model renderAct
--   SDLEvent e                   -> case SDL.eventPayload e of
--     SDL.MouseMotionEvent mouseData -> let p = fromIntegral <$> SDL.mouseMotionEventPos mouseData
--                                       in do -- sendMessage $ Continue (RenderAction Render)
--                                             pure $ Changed (model&mousePosition ?~ p)
--     _                              -> pure Unchanged
--     -- SDL.WindowShownEvent _         -> model <# (pure $ Continue (RenderAction Render))
--     -- SDL.WindowExposedEvent _       -> model <# (pure $ Continue (RenderAction Render))
--     -- SDL.WindowGainedKeyboardFocusEvent _  -> model <# (pure $ Continue (RenderAction Render))

  -- Skip                   -> pure Unchanged



--------------------------------------------------------------------------------
-- * View


headerHeight = 10
footerHeight = 20

paneWidth    = 100
toolBarWidth = 16




-- -- | draw on SDL texture with Render monad from Cairo
-- withCairoTexture     :: SDL.Texture -> Render () -> IO ()
-- withCairoTexture t m = withCairoTexture' t (\s -> renderWith s m)





--------------------------------------------------------------------------------
-- type Attributes = AlphaColour Double
-- fillColor = id


myRectangles :: [Rectangle (Point 2 Double) :+ PathAttributes]
myRectangles =
  [ Rectangle origin (Point2 1 1)          :+ (def&pathColor .~ FillOnly (opaque red))

  , Rectangle origin (Point2 0.5 0.5)       :+ (def&pathColor .~ FillOnly (opaque orange))
  , Rectangle (Point2 (-1) (-1)) origin :+ (def&pathColor .~ FillOnly (opaque blue))
  ]


  -- [ Rectangle origin (Point2 0.5 0.5)       :+ (def&pathColor .~ FillOnly (opaque red))
  -- ]

myTriangles = [ Triangle origin (Point2 400 10) (Point2 430 30) :+
                (def&pathColor .~ StrokeAndFill def (opaque orange))
              ]

--------------------------------------------------------------------------------
-- * Layout

-- data DimSpec w = Absolute {-#UNPACK#-}!Double -- ^ in the range [0,1]
--                |

columns    :: [(Double,Color)]
           -> Vector 2 Double
           -> [Rectangle (Point 2 Double) :+ PathAttributes]
columns ws (Vector2 screenWidth screenHeight) =
  map (\(x,w,c) -> Rectangle (Point2 x 0) (Point2 (x+w) screenHeight) :+
                   (def&pathColor .~ FillOnly c)
      ) $ scaleWeights ws screenWidth


-- | returns the (leftOffset, width, a) tuples
scaleWeights              :: Fractional w => [(w, a)] -> w -> [(w,w,a)]
scaleWeights xs available = let totalWeight = sum $ fst <$> xs
                            in snd $ List.mapAccumL (\left (fWidth,a) ->
                                                       let width = available * fWidth / totalWeight
                                                       in (left + width, (left, width, a))
                                                    ) 0 xs



test = scaleWeights [(1,"foo"),(2,"bar"),(2,"bazel")] (100 :: Double)

byLength xss = scaleWeights (map (\xs -> (List.genericLength xs, xs)) xss)

-- textLabel                    :: Attributes
--                              -> Point 2 Double
--                              -> Text
--                              -> Cairo.Render ()
-- textLabel ats (Point2 x y) t =


-- triangle :: Attributes -> Triangle (Point 2 Double) -> Cairo.Render ()
-- triangle


--------------------------------------------------------------------------------
-- * UI


data ButtonAttributes msg = OnClick msg
                          | Background' Color


data SectionAttributes msg = SectionAttributes (Vector 2 Double) -- size
                           | Background Color

data UserInterface msg where
  TextNode :: Text                                           -> UserInterface msg
  Section  :: [SectionAttributes msg] -> [UserInterface msg] -> UserInterface msg
  Button   :: [ButtonAttributes msg]  -> [UserInterface msg] -> UserInterface msg


myUI   :: model -> UserInterface msg
myUI _ = Section []
                 [ Button []
                          [TextNode "foo"]
                 ]

-- renderUI       :: model -> UserInterface msg -> Cairo.Render ()
-- renderUI model = go
--   where
--     go = \case
--       TextNode text   -> pure ()
--       Section ats chs ->


-- data Dynamic model a where
--   Constant :: a           -> Dynamic model a
--   Dynamic :: (model -> a) -> Dynamic model a

-- instance Functor (Dynamic model) where
--   fmap f = \case
--     Constant x -> Constant (f x)
--     Dynamic g  -> Dynamic (f . g)



-- data UserInterface f msg where
--   TextNode :: f Text                                                       -> UserInterface f msg
--   Section  :: f [f (SectionAttributes msg)] -> f [f (UserInterface f msg)] -> UserInterface f msg
--   Button   :: f [f (ButtonAttributes msg)]  -> f [f (UserInterface f msg)] -> UserInterface f msg



-- mapF   :: forall f g msg.(Functor f)
--        => (forall a. f a -> g a)
--        -> UserInterface f msg -> UserInterface g msg
-- mapF f = \case
--   TextNode text   -> TextNode $ f text
--   Section ats chs -> let ats' :: f [g (SectionAttributes msg)]
--                          ats' = fmap (map f) ats
--                          chs' :: f [g (UserInterface g msg)]
--                          chs' = fmap (map (f . fmap (mapF f))) chs
--                      in Section (f ats') (f chs')


-- foo fh = traverse fh :: [f SecA] -> h [g b]
-- bar fh = fh . fmap (traverse fh) :: h (h [g b])

--   _           :: f (h [g b])


-- traverseF    :: (Applicative h, Applicative f)
--              => (forall a. f a -> h (g a))
--              -> UserInterface f msg -> h (UserInterface g msg)
-- traverseF fh = \case
--   TextNode fText    -> TextNode <$> fg fText
--   Section fAts fChs -> Section <$> hgAts <*> hgChs
--     where
--       hgAts = fg
--             . fmap (\fSAts -> fmap fg

--                    )
--       ats

    -- fg (

    --                       )



  -- ats chs -> let ats' :: f [g (SectionAttributes msg)]
  --                        ats' = fmap (map f) ats
  --                        chs' :: f [g (UserInterface g msg)]
  --                        chs' = fmap (map (f . fmap (mapF f))) chs
  --                    in Section f ats') (f chs')

-- traverseAts :: (forall a. f a -> h (g a)) -> f (SectionAttributes msg)




data MyModel2 = MyModel2 { theText :: Text
                         , theInt  :: Int
                         }

-- myUI :: UserInterface (Dynamic MyModel2) msg
-- myUI = Section (Constant [])
--                (Constant [ Constant $ TextNode (Constant "foo")
--                          , Constant $ TextNode (Dynamic theText)
--                          ]
--                )

-- renderText :: model -> Dynamic model Text -> Cairo.Render ()
-- renderText = undefined

-- renderUI          :: model -> UserInterface (Dynamic model) msg -> Cairo.Render ()
-- renderUI theModel = \case
--   TextNode text -> renderText


-- myUI model = Section [Const $ SectionAttributes (Vector2 10 20)]
--                      [

--                      ]



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
-- TODO: Move to HGeometry.Viewport

-- | Create a viewport whose world-space is \([-1,1] \times [-1,1]\) whose origin is in
-- the center of the screen (which is defined by the given input rectangle)
normalizedCenteredOrigin       :: ( Fractional r, Rectangle_ rectangle point
                                  , Point_ point 2 r
                                  )
                               => rectangle -> Viewport r
normalizedCenteredOrigin rect = let Vector2 w h = size rect
                                    s           = Vector2 (w/2) ((-1)*h/2)
                                in mkViewport rect $ scaling s


-- | Creates a viewport in which the origin at the top left of the viewport
--
-- (moreover, this does not flip the coordinate system, so in a typical graphics setup
-- coordinates move downward)
graphicsOrigin       :: ( Num r
                        ) => Rectangle (Point 2 r) -> Viewport r
graphicsOrigin rect' = Viewport rect' $
                       (translation $ topLeft .-. origin) |.| scaling (Vector2 1 (-1))
  where
    Vector2 _ h = size rect'
    topLeft     = (rect'^.minPoint)&yCoord +~ h

--------------------------------------------------------------------------------





                                     -- (Point2 (realToFrac w) (realToFrac h))

myDraw              :: MyModel -> View es msgs
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

                    -- -- , draw $ head myRectangles
                    -- , draw $ Disk (origin :: Point 2 Double) 100 :+ (def&pathColor .~ FillOnly (opaque black))
                    -- ,

                    -- , draw $ TextLabel "foo" (Point2 500 100) :+ (def @TextAttributes)
                    -- , drawIn centeredVP $ TextLabel "origin" origin' :+ (def @TextAttributes)
                    -- , drawIn centeredVP $ myRectangles
                    -- --   tail myRectangles
                    -- -- , drawIn centeredVP $ head myRectangles
                    -- ]

  -- where
  --   origin'  = origin :: Point 2 Double
  --   centeredVP = traceShowWith ("centeredvp",) $
  --     normalizedCenteredOrigin $ (/2) <$> screen^.target.viewPort.to size



      -- Cairo.setFontSize 20
      --             rectangle (def&pathColor .~ FillOnly (opaque white))
      --                       (screen^.target.viewPort)

      --             disk (def&pathColor .~ FillOnly (opaque green))
      --                    (Disk p 4)
      --             for_ (columns [ (1, opaque blue)
      --                           , (2, opaque red )
      --                           , (2, opaque orange)
      --                           ]
      --                           (size (screen^.target.viewPort))
      --                  ) $ \(r :+ ats) -> rectangle ats r
      --             renderTextAt (Point2 100 200) "foo"


--------------------------------------------------------------------------------

textLabel   :: Text -> TextLabel
textLabel t = TextLabel t origin

myUI'              :: MyModel -> RenderTarget -> Drawing
myUI' model screen = mainSection



  -- draw [ draw menuBar
  --                         , mainSection
  --                         , draw footer
  --                         ]
  where
    Vector2 w h = size $ screen^.target.viewPort
    menuBar = Rectangle origin                        (Point2 w menuBarHeight)
              :+ (def&pathColor .~ FillOnly menuBarColor)


    footer  = Rectangle (Point2 0 (h - footerHeight)) (Point2 w h)
              :+ (def&pathColor .~ FillOnly menuBarColor)

    mainSection = draw [ mainPanel
                       , mainArea
                       ]

    mainPanel = drawIn mainPanelVP $
                  [ draw $ Blank panelColor
                  , rows [ draw $ textLabel (l^.layerName) :+ (def @TextAttributes)
                         | l <- model^..layers.traverse
                         ]
                  ]

    mainArea  = drawIn mainAreaVP $
                  [ draw $ Blank (opaque Colour.yellow)
                  , foldMapOf (layers.traverse) drawLayer model
                  ]

    mainPanelVP = graphicsOrigin           $ Rect 0 menuBarHeight w mainHeight
    -- mainAreaVP  = normalizedCenteredOrigin $ Rect mainPanelWidth menuBarHeight w mainHeight
    mainAreaVP  = alignedOrigin $ Rect mainPanelWidth menuBarHeight w mainHeight

    mainHeight = h-footerHeight-menuBarHeight

    menuBarHeight = 20
    footerHeight  = 20

    mainPanelWidth = 200

    panelColor       = opaque $ sRGB24 249 250 251
    menuBarColor     = opaque $ sRGB24 74 84 100
    menuBarTextColor = opaque $ sRGB24 142 149 160

rows :: [Drawing] -> Drawing
rows = fold . snd . List.mapAccumL (\acc g -> (acc + 20, translateBy (Vector2 0 acc) g)) 0

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------

main :: IO ()
main = runEff $ runApp $
       AppConfig
         { _appModel        = defaultModel
         , _handler         = myHandler'
         , _initialMessages = []
         , _appRender       = myDraw
         , _settings        = def&windowTitle .~ "Demo"
         }
