{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
module SDL2MVC.App
  ( AppConfig(..)
  , appModel, handler, initialMessages, appRender
  , windowTitle

  , AppSettings(..)
  , HasAppSettings(..)

  , Extended(Ex)

  , App(..)
  , config, windowRef, rendererRef, textureRef, eventQueue

  , View
  , Vary.Vary

  , RenderTarget(RenderTarget), targetTexture, target
  ) where

import qualified Control.Concurrent.STM.TBQueue as Queue
import           Control.Lens
import           Data.Default.Class
import           Data.Kind (Type)
import           Data.Text (Text, pack)
import           Effectful
import           HGeometry.Viewport
import qualified SDL
import           SDL2MVC.Reaction
import           SDL2MVC.Send
import           SDL2MVC.Updated
import qualified Vary
import qualified GI.Cairo.Render as Cairo

--------------------------------------------------------------------------------



data RenderTarget = RenderTarget { _targetTexture  :: SDL.Texture
                                 , _target         :: Viewport Double
                                 }

makeLenses ''RenderTarget


type View (es :: [Effect]) (msgs :: [Type]) = RenderTarget -> Cairo.Render () -- Eff es ()


data Extended model where
  -- ^ Extended hides the underlying app.
  Extended :: App es model msgs inMsgs -> Extended model

-- | Pattern to match on the model
pattern Ex       :: model -> Extended model
pattern Ex model <- (getModel -> model)


getModel                :: Extended model -> model
getModel (Extended app) = _appModel . _config $ app



-- |  Configuration data for the App
data AppConfig (es :: [Effect]) model (msgs :: [Type]) (inMsgs :: [Type]) =
  AppConfig { _appModel        :: model
            , _handler         :: App es model msgs inMsgs
                               -> model -> Vary.Vary inMsgs -> Eff es (Updated model)
            , _initialMessages :: [Vary.Vary msgs]
            , _appRender       :: model -> View es msgs
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
                                      , SDL.windowHighDPI     = False -- True
                                        -- TODO: enabling this somehow creates some weird
                                        -- scaling effect
                                      }
                    }
--------------------------------------------------------------------------------

-- | A raw SDL2MVC App
data App es model msgs inMsgs =
     App { _config          :: AppConfig es model msgs inMsgs
         , _windowRef       :: SDL.Window
         , _rendererRef     :: SDL.Renderer
         , _textureRef      :: SDL.Texture
         , _eventQueue      :: Queue.TBQueue (Vary.Vary msgs)
          -- ^ the event queue we are using
         }


makeLenses ''AppConfig

makeClassy ''AppSettings


makeLenses ''App



instance HasAppSettings (AppConfig es model msgs inMsgs) where
  appSettings = settings
