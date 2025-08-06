{-# LANGUAGE TemplateHaskell #-}
module SDL2MVC.App
  ( AppConfig(..)
  , appModel, handler, startupAction, liftSDLEvent, liftRenderEvent, appRender

  , App(..)
  , config, windowRef, rendererRef, textureRef, eventQueue

  , View
  ) where

import qualified Control.Concurrent.STM.TBQueue as Queue
import           Control.Lens
import qualified SDL
import           SDL2MVC.Reaction

--------------------------------------------------------------------------------


type View m action = SDL.Texture -> m action



-- |  Configuration data for the App
data AppConfig m model action =
  AppConfig { _appModel        :: model
            , _handler         :: App m model action -> Handler m model action
            , _startupAction   :: action
            , _liftSDLEvent    :: SDL.Event -> LoopAction action
            , _liftRenderEvent :: Render -> action
            , _appRender       :: model -> View m action
            }

-- | A raw SDL2MVC App
data App m model action =
     App { _config          :: AppConfig m model action
         , _windowRef       :: SDL.Window
         , _rendererRef     :: SDL.Renderer
         , _textureRef      :: SDL.Texture
         , _eventQueue      :: Queue.TBQueue (LoopAction action)
          -- ^ the event queue we are using
         }


makeLenses ''AppConfig

makeLenses ''App
