module Debugger
  ( main
  ) where

import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified GI.Cairo.Render as Cairo
import           SDL2MVC.App
import           SDL2MVC.Cairo
import           SDL2MVC.Framework
import           SDL2MVC.Reaction
import           SDL2MVC.Render

--------------------------------------------------------------------------------
-- * Model

data Visible = InVisible | Visible
  deriving (Show,Eq,Enum,Bounded)

toggle = \case
  InVisible -> Visible
  Visible   -> InVisible

data Layer = Layer { _layerName    :: !Text
                   , _visibility   :: {-#UNPACK#-}!Visible
                   , _render       :: Cairo.Render ()
                   }

renderLayer                :: Layer -> Cairo.Render ()
renderLayer l | l^.visibility = l^.render
              | otherwise     = pure ()


data Model = Model { _layers :: Seq.Seq Layer

                   }

defaultModel = Model { _layers = mempty }

--------------------------------------------------------------------------------
-- * View



type Draw a = Cairo.Render a


renderView        :: View IO Action
renderView model = \texture -> Skip <$ do
do SDL.TextureInfo _ _ w h <- SDL.queryTexture texture
                  withCairoTexture texture $ do

  for_ (model^.layers.traverse) renderLayer


--------------------------------------------------------------------------------
-- * Controller

data Action = AddLayer Text (Cairo.Render ())
            | ToggleVisibility Int
            | Skip


theHandler            :: Handler IO Model Action
theHandler _app model = \case
  AddLayer name content -> model&layers %~ (Seq.|> Layer name Visible content)
                           <# pure ()
  ToggleVisibility i    -> model&layers.ix i %~ l&visibility %~ toggle
  Skip                  -> noEff model



--------------------------------------------------------------------------------
-- * The app



main :: IO ()
main = runApp $
       AppConfig
         { _appModel        = defaultModel
         , _handler         = theHandler
         , _startupAction   = Nothing
         , _liftSDLEvent    = withDefaultSDLEvents SDLEvent
         , _liftRenderEvent = RenderAction
         , _appRender       = renderView
         , _settings        = def&windowTitle .~ "Demo"
         }
