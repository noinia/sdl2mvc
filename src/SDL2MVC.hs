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
import           Data.Colour.SRGB (RGB(..), toSRGB)
import           Data.Default.Class
import           Data.Foldable (for_)
import qualified Data.List as List
import           Debug.Trace
import           Effectful
import           GHC.Natural
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Matrix as CairoM
import           HGeometry.Ball
import           HGeometry.Box
import           HGeometry.Ext
import           HGeometry.Matrix
import           HGeometry.Point
import           HGeometry.Transformation
import           HGeometry.Triangle
import           HGeometry.Vector
import           HGeometry.Viewport
import           Linear
import qualified Linear.Affine as Linear
import qualified SDL
import           SDL2MVC.App
import           SDL2MVC.Cairo
import           SDL2MVC.Framework
import           SDL2MVC.Reaction
import           SDL2MVC.Render
import           SDL2MVC.Updated
import           SDL2MVC.Send

import           Data.Text (Text)
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

-- data WithBasicActions = LoopAction RenderAction

----------------------------------------

myHandler           :: ( Send (LoopAction MyAction) :> es
                       , IOE :> es
                       )
                    => App es MyModel MyAction
                    -> MyModel -> MyAction -> Eff es (Updated MyModel)
myHandler app model = \case
  RenderAction renderAct -> Unchanged <$ handleRender app model renderAct
  SDLEvent e                   -> case SDL.eventPayload e of
    SDL.MouseMotionEvent mouseData -> let p = fromIntegral <$> SDL.mouseMotionEventPos mouseData
                                      in do sendMessage $ Continue (RenderAction Render)
                                            pure $ Changed (model&mousePosition ?~ p)
    _                              -> pure Unchanged
    -- SDL.WindowShownEvent _         -> model <# (pure $ Continue (RenderAction Render))
    -- SDL.WindowExposedEvent _       -> model <# (pure $ Continue (RenderAction Render))
    -- SDL.WindowGainedKeyboardFocusEvent _  -> model <# (pure $ Continue (RenderAction Render))

  Skip                   -> pure Unchanged



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

type Color = Colour.AlphaColour Double

data Stroke = Stroke { _strokeWidth :: {-#UNPACK#-}!Double
                     , _strokeColor :: !Color
                     }
            deriving (Show,Eq)

makeLenses ''Stroke

instance Default Stroke where
  def = Stroke 1 (opaque Colour.black)

-- | Paths must have stroke, fill, or both
data StrokeAndFillSpec =
    StrokeOnly  {-# UNPACK #-}!Stroke
  | FillOnly    {-# UNPACK #-}!Color
  | StrokeAndFill  {-# UNPACK #-}!Stroke -- ^ stroke spec
                   {-# UNPACK #-}!Color -- ^ fill color
    deriving stock (Show,Eq)

instance Default StrokeAndFillSpec where
  def = StrokeOnly def

-- | Lens to access the Stroke of a StrokeAndFill. This may set the stroke.
_Stroke :: Lens' StrokeAndFillSpec (Maybe Stroke)
_Stroke = lens (\case
                   StrokeOnly s      -> Just s
                   StrokeAndFill s _ -> Just s
                   _                 -> Nothing
               )
               (\spec -> \case
                   Nothing -> case spec of
                                StrokeAndFill _ f -> FillOnly f
                                _                 -> spec -- impossible to delete stroke
                   Just s  -> case spec of
                                StrokeOnly _      -> StrokeOnly s
                                StrokeAndFill _ f -> StrokeAndFill s f
                                FillOnly f        -> StrokeAndFill s f
               )

-- | Lens to access the fill color of a StrokeAndFill. This may set the stroke and fill
_Fill :: Lens' StrokeAndFillSpec (Maybe Color)
_Fill = lens (\case
                 FillOnly f        -> Just f
                 StrokeAndFill _ f -> Just f
                 _                 -> Nothing
             )
             (\spec -> \case
                 Nothing -> case spec of
                              StrokeAndFill s _ -> StrokeOnly s
                              _                 -> spec -- impossible to delete fill
                 Just f  -> case spec of
                              StrokeOnly s      -> StrokeAndFill s f
                              FillOnly _        -> FillOnly f
                              StrokeAndFill s _ -> StrokeAndFill s f
             )



data PathAttributes = PathAttributes { _strokeAndFill :: {-# UNPACK #-} !StrokeAndFillSpec
                                     }
  deriving stock (Show,Eq)

pathColor :: Lens' PathAttributes StrokeAndFillSpec
pathColor = lens _strokeAndFill (\ats c -> ats { _strokeAndFill = c} )

instance Default PathAttributes where
  def = PathAttributes def

instance Semigroup PathAttributes where
  -- ^ Later options may overwrite former ones.
  (PathAttributes c) <> (PathAttributes c') = PathAttributes c'

instance Monoid PathAttributes where
  mempty = def



--------------------------------------------------------------------------------
-- type Attributes = AlphaColour Double
-- fillColor = id


renderTextAt                  :: Real r => Point 2 r -> Text -> Cairo.Render ()
renderTextAt (Point2 x y) txt = do Cairo.save
                                   Cairo.translate (realToFrac x) (realToFrac y)
                                   Cairo.showText txt
                                   Cairo.restore

toCairoMatrix   :: Real r => Matrix 3 3 r -> Cairo.Matrix
toCairoMatrix m = case m&elements %~ realToFrac of
  Matrix (Vector3
          (Vector3 a b c)
          (Vector3 d e f)
          _
         ) -> CairoM.Matrix a d b e c f

renderIn           :: Real r => Viewport r -> Cairo.Render () -> Cairo.Render ()
renderIn vp render = do
                        Cairo.save
                        Cairo.transform (vp^.worldToHost.transformationMatrix.to toCairoMatrix)
                        render
                        Cairo.restore


setStroke s = do Cairo.setLineWidth $ s^.strokeWidth
                 setColor $ s^.strokeColor


withStrokeAndFill                 :: StrokeAndFillSpec -> Cairo.Render () -> Cairo.Render ()
withStrokeAndFill spec renderPath = case spec of
  StrokeOnly s      -> do setStroke s
                          renderPath
                          Cairo.stroke
  FillOnly f        -> do setColor f
                          renderPath
                          Cairo.fill
  StrokeAndFill s f -> do setColor f
                          renderPath
                          Cairo.fillPreserve
                          setStroke s
                          renderPath
                          Cairo.stroke

disk          :: (Point_ point 2 r, Real r)
              => PathAttributes -> Disk point -> Cairo.Render ()
disk ats disk = withStrokeAndFill (ats^.pathColor) $ do
                   let Point2 x y = disk^.center.asPoint
                                    & coordinates %~ realToFrac
                       r          = realToFrac $ disk^.squaredRadius
                   Cairo.arc x y (sqrt r) 0 (2*pi)

toRGBA col = ( toSRGB $ col `Data.Colour.over` black
             , alphaChannel col
             )

setColor col = let (RGB r g b, a) = toRGBA col
               in Cairo.setSourceRGBA r g b a

rectangle          :: (Point_ point 2 r, Real r)
                   => PathAttributes -> Rectangle point -> Cairo.Render ()
rectangle ats rect = withStrokeAndFill (ats^.pathColor) $ do
                        let Point2 x y  = rect^.minPoint.asPoint
                                          & coordinates %~ realToFrac
                            Vector2 w h = realToFrac <$> size rect
                        Cairo.rectangle x y w h

triangle         :: (Point_ point 2 r, Real r)
                 => PathAttributes -> Triangle point -> Cairo.Render ()
triangle ats tri = withStrokeAndFill (ats^.pathColor) $ do
                      let Triangle a b c = toPoints tri
                      Cairo.moveTo (a^.xCoord) (a^.yCoord)
                      Cairo.lineTo (b^.xCoord) (b^.yCoord)
                      Cairo.lineTo (c^.xCoord) (c^.yCoord)
                      Cairo.closePath

toPoints :: (Functor f, Point_ point 2 r, Real r) => f point -> f (Point 2 Double)
toPoints = fmap (\p -> p^.asPoint &coordinates %~ realToFrac)


myRectangles =
  [ Rectangle origin (Point2 0.5 0.5)       :+ (def&pathColor .~ FillOnly (opaque red))
  , Rectangle (Point2 (-1) (-1)) origin :+ (def&pathColor .~ FillOnly (opaque blue))
  ]

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

--------------------------------------------------------------------------------

-- | Create a viewport whose world-space is \([-1,1] \times [-1,1]\) whose origin is in
-- the center of the screen, i.e. the rectangle \([0,w] \times [0,h]\)), which is given
-- by the input Vector w h
normalizedCenteredOrigin       :: (Real r', Fractional r) => Vector 2 r' -> Viewport r
normalizedCenteredOrigin dims' = let Vector2 w h = realToFrac <$> dims'
                                     rect        = Rectangle origin (Point2 w h)
                                     s           = Vector2 (w/2) ((-1)*h/2)
                                 in mkViewport rect $ scaling s




                                     -- (Point2 (realToFrac w) (realToFrac h))

myDraw               :: IOE :> es => MyModel -> View es MyAction
myDraw model texture =
  -- do
  --   renderDiagramTo texture $ diagramDraw model
  --   pure Skip
  case fmap fromIntegral <$> model^.mousePosition of
    Nothing -> liftIO $ print "cursor outside screen"
    Just p  -> do SDL.TextureInfo _ _ w h <- SDL.queryTexture texture
                  liftIO $ withCairoTexture texture $ do
                    Cairo.setFontSize 20
                    rectangle (def&pathColor .~ FillOnly (opaque white))
                              (Rectangle origin (Point2 w h))
                    disk (def&pathColor .~ FillOnly (opaque green))
                         (Disk p 4)
                    for_ (columns [ (1, opaque blue)
                                  , (2, opaque red )
                                  , (2, opaque orange)
                                  ]
                                  (realToFrac <$> Vector2 w h)
                         ) $ \(r :+ ats) -> rectangle ats r
                    renderTextAt (Point2 100 200) "foo"

                    -- let myViewport = normalizedCenteredOrigin (Vector2 w h)
                    -- renderIn myViewport $ do
                    --   for_ myRectangles $ \(r :+ ats) -> rectangle ats r


                    -- renderIn (flipY $ Vector2 w h)


                    -- --   $ do
                    --   for_ myRectangles $ \(r :+ ats) -> rectangle ats r
                      -- for_ myTriangles  $ \(r :+ ats) -> triangle ats r

--------------------------------------------------------------------------------

main :: IO ()
main = runEff $ runApp $
       AppConfig
         { _appModel        = defaultModel
         , _handler         = myHandler
         , _startupAction   = Nothing
         , _liftSDLEvent    = withDefaultSDLEvents SDLEvent
         , _liftRenderEvent = RenderAction
         , _appRender       = myDraw
         , _settings        = def&windowTitle .~ "Demo"
         }
