{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  SDL2MVC.UIState
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- MVC implementation of the UI State
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
import           Data.Colour.Names (white)
import           Data.Text (Text)
import           Foreign.C.Types (CInt)
import           HGeometry.Box
import           HGeometry.Point
import           HGeometry.Vector
import           HGeometry.Viewport
import qualified SDL
import           SDL (($=))
import qualified SDL.Cairo
import           SDL2MVC.Attribute
import           SDL2MVC.Effect
import           SDL2MVC.Render
import           SDL2MVC.View

--------------------------------------------------------------------------------
-- * Model

-- | Internal state of the SDL app
data WindowState = WindowState { _window       :: SDL.Window
                               , _renderer     :: SDL.Renderer
                               , _texture      :: SDL.Texture
                               , _mainViewport :: Viewport Double
                               }
  deriving stock (Eq)
makeLenses ''WindowState


initializeWindowState   :: Text -- ^ window title
                        -> IO WindowState
initializeWindowState t = do
    let windowCfg = SDL.defaultWindow
    window'   <- SDL.createWindow t windowCfg
    renderer' <- SDL.createRenderer window' (-1) SDL.defaultRenderer
    texture'  <- SDL.Cairo.createCairoTexture' renderer' window'
    let vp = flipY . fromLinear $ SDL.windowInitialSize windowCfg
    pure $ WindowState { _window       = window'
                       , _renderer     = renderer'
                       , _texture      = texture'
                       , _mainViewport = vp
                       }
  where
    fromLinear              :: SDL.V2 CInt -> Vector 2 Double
    fromLinear (SDL.V2 w h) = Vector2 (realToFrac w) (realToFrac h)


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
                   deriving (Eq)

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
                  let size' = Vector2 (realToFrac $ SDL.textureWidth tInfo) (realToFrac h)
                      h     = SDL.textureHeight tInfo
                  runRender texture' h $ blank size'
                  -- cleared the renderer and the texture
                  runRender texture' h d
                  -- render the drawing onto the texture
                  SDL.copy renderer' texture' Nothing Nothing
                  SDL.present renderer'
                  -- present the rendered drawing
  where
    renderer' = m^.windowState.renderer
    texture'  = m^.windowState.texture

    -- size'' = size $ m^.windowState.mainViewport.viewport


-- | filled white background
blank       :: Vector 2 Double -> View action
blank size' = rectangle_ [Fill :=> white ] $ Rectangle origin (origin .+^ size')


  -- Clear         -> m <# do size <- pure $ SDL.V2 600 800
  --                            -- SDL.get SDL.windowSize -- (m^.windowState.window)
  --                          pure . Just $ Redraw (blank size)
