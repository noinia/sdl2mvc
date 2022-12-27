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
import           Foreign.C.Types (CInt)
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
         -> Effect (Maybe (Action action)) (UIState action)
update m = \case
  Redraw d      -> (m&drawing .~ d)         <# do rerender m d
                                                  pure Nothing
  UpdateTitle t -> m <# do SDL.windowTitle (m^.windowState.window) $= t
                           pure Nothing




--------------------------------------------------------------------------------
-- * View

-- | reruns the renderer
rerender     :: UIState action' -> View action -> IO ()
rerender m d = do SDL.clear renderer'
                  tInfo <- SDL.queryTexture texture'
                  let size = SDL.V2 (SDL.textureWidth tInfo) (SDL.textureHeight tInfo)
                  runRender texture' $ blank size
                  -- cleared the renderer and the texture
                  runRender texture' d
                  -- render the drawing onto the texture
                  SDL.copy renderer' texture' Nothing Nothing
                  SDL.present renderer'
                  -- present the rendered drawing
  where
    renderer' = m^.windowState.renderer
    texture'  = m^.windowState.texture

-- | filled white background
blank      :: SDL.V2 CInt -> View action
blank size = Colored white
           $ Rect r mempty
  where
    r :: SDL.Rectangle Double
    r = SDL.Rectangle (SDL.P (SDL.V2 0 0)) (realToFrac <$> size)

    white = SDL.V4 255 255 255 255


  -- Clear         -> m <# do size <- pure $ SDL.V2 600 800
  --                            -- SDL.get SDL.windowSize -- (m^.windowState.window)
  --                          pure . Just $ Redraw (blank size)
