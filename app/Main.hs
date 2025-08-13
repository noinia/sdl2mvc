{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens hiding (elements)
import           Data.Bifunctor
import           Data.Colour
import           Effectful
import qualified SDL
import           SDL2MVC
import qualified Vary

--------------------------------------------------------------------------------
-- * Model
{-
data Model = Model { _mousePosition :: !(Maybe (Linear.Point V2 Int))
                   , _mainViewPort  :: Viewport R
                   , _points        :: [Point 2 R]
                   }
           deriving (Show,Eq)

makeLenses ''MyModel

defaultModel :: MyModel
defaultModel = MyModel { _mousePosition = Nothing
                       , _layers        = mempty
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

-}
-- controller           :: forall es inMsgs outMsgs.
--                        ( inMsgs  ~ [SDL.Event, MyAction]
--                        , outMsgs ~ (Shutdown : Render : inMsgs)
--                        , Send outMsgs :> es
--                        )
--                      => Handler es Model outMsgs inMsgs
-- controller model msg = case first Vary.intoOnly $ Vary.pop msg of
--   Right e -> case SDL.eventPayload e of
--       SDL.MouseMotionEvent mouseData -> let p = fromIntegral <$> SDL.mouseMotionEventPos mouseData
--                                         in pure $ Changed (model&mousePosition ?~ p)

--       SDL.WindowShownEvent _         -> Unchanged <$ sendMsg @outMsgs Render
--       SDL.WindowExposedEvent _       -> Unchanged <$ sendMsg @outMsgs Render

--       SDL.MouseButtonEvent mouseData -> case SDL.mouseButtonEventMotion mouseData of
--         SDL.Pressed -> case asPoint' <$> model^.mousePosition of
--                          Nothing -> pure Unchanged
--                          Just p'  -> let (nextLayer,model') = takeNextLayer model
--                                          p                  = traceShowWith ("P",) $
--                                            toWorldIn (model^.mainViewPort) p'
--                                          disk = Disk p 25
--                                               :+ (def&pathColor .~ StrokeAndFill def (opaque red))
--                                      in Changed (model'&points %~ (p:))
--                                         <$ sendMsg @outMsgs (AddLayer nextLayer (draw disk))

--         _           -> pure Unchanged

--       _                              -> pure Unchanged

--   Left act -> case act of
--                 AddLayer name d -> pure $ Changed (model&layers %~ (Seq.:|> Layer name Visible d))


--------------------------------------------------------------------------------
-- * View



--------------------------------------------------------------------------------

main :: IO ()
main = runEff $ SDL2MVC.runApp $
       AppConfig
         { _appModel        = defaultModel
         , _handler         = myHandler'
         , _initialMessages = []
         , _appRender       = myDraw
         , _settings        = def&windowTitle .~ "Demo"
         }
