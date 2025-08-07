{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
module SDL2MVC.App
  ( AppConfig(..)
  , appModel, handler, startupAction, liftSDLEvent, liftRenderEvent, appRender
  , windowTitle

  , AppSettings(..)
  , HasAppSettings(..)

  , Extended(Ex)

  , App(..)
  , config, windowRef, rendererRef, textureRef, eventQueue

  , View
  ) where

import qualified Control.Concurrent.STM.TBQueue as Queue
import           Control.Lens
import           Data.Default.Class
import           Data.Text (Text, pack)
import qualified SDL
import           SDL2MVC.Reaction
import           Effectful

--------------------------------------------------------------------------------


type View (es :: [Effect]) msg = SDL.Texture -> Eff es msg


data Extended model where
  -- ^ Extended hides the underlying app.
  Extended :: App es model action -> Extended model

-- | Pattern to match on the model
pattern Ex       :: model -> Extended model
pattern Ex model <- (getModel -> model)


getModel                :: Extended model -> model
getModel (Extended app) = _appModel . _config $ app



-- |  Configuration data for the App
data AppConfig (es :: [Effect]) model action =
  AppConfig { _appModel        :: model
            , _handler         :: App es model action -> Handler (Eff es) model action
            , _startupAction   :: Maybe action
            , _liftSDLEvent    :: SDL.Event -> LoopAction action
            , _liftRenderEvent :: Render -> action
            , _appRender       :: model -> View es action
            , _settings        :: !AppSettings
            }

data AppSettings =
  AppSettings { _windowTitle     :: !Text
              , _windowConfig    :: !SDL.WindowConfig
              }




instance Default AppSettings where
  def = AppSettings { _windowTitle = pack "SDL2MVC App"
                    , _windowConfig = SDL.defaultWindow
                                      { SDL.windowInitialSize = SDL.V2 1228 768
                                      , SDL.windowHighDPI     = True
                                      }
                    }
--------------------------------------------------------------------------------

-- | A raw SDL2MVC App
data App es model action =
     App { _config          :: AppConfig es model action
         , _windowRef       :: SDL.Window
         , _rendererRef     :: SDL.Renderer
         , _textureRef      :: SDL.Texture
         , _eventQueue      :: Queue.TBQueue (LoopAction action)
          -- ^ the event queue we are using
         }


makeLenses ''AppConfig

makeClassy ''AppSettings


makeLenses ''App



instance HasAppSettings (AppConfig es model action) where
  appSettings = settings
