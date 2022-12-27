{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  SDL2MVC.UIState
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- MVC view on teh SDL Renderer
--
--------------------------------------------------------------------------------
module SDL2MVC.UIState
  ( WindowState(..), window, renderer, texture
  , initializeWindowState

  , UIState(..), drawing, windowState
  , initializeUIState

  , update

  , Action(..)
  , rerender
  ) where

import           Control.Lens
import           Data.Text (Text)
import qualified SDL
import           SDL (($=))
import qualified SDL.Cairo
import           SDL2MVC.Effect
import           SDL2MVC.View

--------------------------------------------------------------------------------
-- * Model

-- | Internal state of the SDL app
data WindowState = WindowState { _window   :: SDL.Window
                               , _renderer :: SDL.Renderer
                               , _texture  :: SDL.Texture
                               }
  deriving stock (Eq)
makeLenses ''WindowState


initializeWindowState   :: Text -- ^ window title
                        -> IO WindowState
initializeWindowState t = do
    window'   <- SDL.createWindow t SDL.defaultWindow
    renderer' <- SDL.createRenderer window' (-1) SDL.defaultRenderer
    texture'  <- SDL.Cairo.createCairoTexture' renderer' window'
    pure $ WindowState { _window   = window'
                       , _renderer = renderer'
                       , _texture  = texture'
                       }

data UIState action = UIState { _drawing :: View action
                                -- ^ current view
                              , _windowState :: WindowState
                                -- ^ state of the window and its renderer etc.
                              }
                    deriving (Eq)
makeLenses ''UIState

-- | initializes the UI state
initializeUIState   :: Text -> IO (UIState action)
initializeUIState t = UIState Blank <$> initializeWindowState t

--------------------------------------------------------------------------------
-- * Controller

data Action action = Redraw (View action)
                   | UpdateTitle Text

-- | The update function
update   :: UIState action
         -> Action action
         -> Effect () (UIState action)
update m = \case
  Redraw d -> (m&drawing .~ d) <# do runRender (m^.windowState.renderer) d
                                     SDL.clear (m^.windowState.renderer)
                                     SDL.present (m^.windowState.renderer)
  UpdateTitle t           -> m <# do SDL.windowTitle (m^.windowState.window) $= t

--------------------------------------------------------------------------------
-- * View

-- | Obtain a drawing of the current UI state
rerender   :: UIState action -> View action
rerender m = m^.drawing
