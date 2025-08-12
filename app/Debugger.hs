{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import qualified SDL
import           SDL2MVC
import qualified Data.Sequence as Seq
import           Data.Text (Text)

--------------------------------------------------------------------------------
-- * Model

type R = Double

data Model = Model { _mousePosition :: !(Maybe (Linear.Point V2 Int))
                   , _layers        :: Seq.Seq Layer
                   , _mainViewPort  :: Viewport R
                   , _nextLayerName :: NonEmpty.NonEmpty LayerName -- should be a stream
                   }
           deriving (Show,Eq)

makeLenses ''MyModel

defaultModel :: MyModel
defaultModel = MyModel { _mousePosition = Nothing
                       , _layers        = mempty
                       , _mainViewPort  = graphicsOrigin
                                        $ Rect mainPanelWidth menuBarHeight w (mainHeight h)
                       , _nextLayerName = NonEmpty.fromList
                                        $ cycle ["alpha","beta","gamma","delta"]
                       , _points        = mempty
                       }
  where
    V2 w h = realToFrac <$> def @AppSettings ^.windowConfig.to SDL.windowInitialSize



--------------------------------------------------------------------------------
-- * Controller

data Action = AddLayer LayerName Drawing
            deriving (Show,Eq)

type Messages = [Shutdown, Render, SDL.Event, Action]

controller :: forall es inMsgs outMsgs.
              ( inMsgs  ~ [SDL.Event, MyAction]
              , outMsgs ~ (Shutdown : Render : inMsgs)
              , Send outMsgs :> es
              )
           => Handler es Model outMsgs inMsgs
controller = undefined

--------------------------------------------------------------------------------

render = undefined


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
                     (Send MyMsgs :> es, Drawable g)
                  => String -> g -> Eff es ()
traceDraw' name g = sendMsg @MyMsgs $ AddLayer (Text.pack name) (draw g)



main :: IO ()
main = runEff $ runApp $
       AppConfig
         { _appModel        = defaultModel
         , _handler         = withDefaultActions controller
         , _initialMessages = []
         , _appRender       = myDraw
         , _settings        = def&windowTitle .~ "HGeometry Debugger"
         }
