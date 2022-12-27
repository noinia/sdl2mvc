{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad
import Data.Text (Text)
import Data.Word
import SDL
import SDL2MVC.SDLApp
import SDL2MVC.View
import SDL2MVC.Effect
import SDL2MVC.SimpleSDLEvent

import Debug.Trace

--------------------------------------------------------------------------------
-- * Model

type Color = V4 Word8

newtype Model = Model { _theColor :: Color }
  deriving (Show,Eq)

makeLenses ''Model

blue = V4 0 255 0 0

initialModel = Model { _theColor = blue }


--------------------------------------------------------------------------------
-- * Controller

data MyAction = Started
              | HandleEvent SDL.Event
  deriving (Show,Eq)

update   :: Model -> MyAction -> Effect (Action MyAction Model) Model
update m = \case
  Started       -> m <# do print "Started"
                           pure Skip
  HandleEvent e -> case e of
    KeyPress KeycodeQ -> m <# pure Quit
    KeyPress KeycodeR -> noEff $ m&theColor .~ V4 255 0 0 255
    KeyPress KeycodeB -> noEff $ m&theColor .~ V4 0 0 255 255
    MouseMove p       -> m <# do print p
                                 pure Skip
    _                 -> noEff m

  -- HandleEvent e
  --   | eventIsPress KeycodeQ -> m <# pure Quit
  --   | eventIsPress KeycodeR -> noEff $ m&theColor .~ V4 255 0 0 255
  --   | eventIsPress KeycodeB -> noEff $ m&theColor .~ V4 0 0 255 255
  --   | otherwise             -> noEff m
  --   where
  --     eventIsPress keyCode =
  --         case eventPayload e of
  --           KeyboardEvent keyboardEvent ->
  --             keyboardEventKeyMotion keyboardEvent == Pressed &&
  --             keysymKeycode (keyboardEventKeysym keyboardEvent) == keyCode
  --           _ -> False

--------------------------------------------------------------------------------
-- * View

render   :: Model -> View (Action MyAction Model)
render m = Colored (m^.theColor)
         $ Rect r mempty
  where
    r = SDL.Rectangle (P (V2 10 20)) (V2 200 300)

--------------------------------------------------------------------------------
-- * Main

myApp :: AppConfig MyAction Model
myApp = AppConfig { _update             = update
                  , _render             = render
                  , _startupAction      = Started
                  , _interpretSDLEvent  = HandleEvent
                  , _initialWindowTitle = "MyApp :)"
                  }

main :: IO ()
main = runApp initialModel myApp




{-

{-

data App action model = App { _model         :: model
                            , _update        :: model -> action -> Effect action model
                            , _view          :: model -> View action model
                            , _subs          :: [Sub action]
                            , _initialAction :: action
                            }
-- makeLenses ''App


data Model action model = Model { _appModel :: model
                                , _drawing  :: View action model
                                }

data Action action model = Quit
                         | Skip
                         | AppAction action
                         | Redraw (View action model)

update = undefined

view'   :: Model -> View Action Model
view' _ = Blank

myApp :: App Action Model
myApp = App { _model         = ()
            , _update        = update
            , _view          = view'
            , _subs          = []
            , _initialAction = Pass
            }

--------------------------------------------------------------------------------

-- handleEvents          :: Model model action -> [Event] -> Efect (Action GlobalAction Model)
-- handleEvents m events
--   | containsQuit events -> Quit
--   | shouldRedraw        ->

--   = case events of
--   [] -> Skip


interpretEvent   :: Event -> Maybe (Action action model)
interpretEvent _ = Nothing

type SDLModel = Model () ()

data SDLEffect = SDLEffect Model Action

handle :: SDLModel -> [Event] -> Effect () SDLModel
handle = undefined

-}

data UserAction = PrintHello
                | SomethingExpensive


-- type Sub action = action -> IO ()


data Action model = Skip
                  | Redraw model
                  | Quit
                  deriving (Show,Eq)












myApp :: Renderer -> SDLApp
myApp = SDLApp (Model Blank "foo")

handle             :: Model -> Event -> Action Model
handle model event
  -- | traceShow event False = undefined
  | eventIsPress KeycodeQ = Quit
  | eventIsPress KeycodeR = traceShowId $ Redraw $ model&drawing .~ Color (V4 255 0 0 255) Blank
  | eventIsPress KeycodeB = traceShowId $ Redraw $ model&drawing .~ Color (V4 0 0 255 255) Blank
  | otherwise             = Skip
  where
    eventIsPress keyCode =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == keyCode
          _ -> False



data ContinueX model = QuitX
                     | Continue model [Renderer -> IO ()]



handleAll        :: Model -> [Event] -> ContinueX Model
handleAll model0 = go (Continue model0 [])
  where
    go k@(Continue model scheduled) = \case
      []     -> k
      (e:es) -> case handle model e of
                  Quit            -> QuitX
                  Skip            -> go k es
                  (Redraw model') -> go (Continue model' (redraw model' : scheduled) ) es
    go QuitX                       = const QuitX


redraw                :: Model -> Renderer -> IO ()
redraw model renderer = do render renderer (model^.drawing)
                           clear renderer
                           present renderer

appLoop     :: SDLApp -> IO ()
appLoop app = do
    events <- pollEvents
    case handleAll (app^.appModel) events of
      QuitX                    -> pure ()
      Continue model scheduled -> do mapM_ ($ app^.renderer) scheduled -- fixme
                                     appLoop (app&appModel .~ model)
      -- Skip     -> continue app
      -- Redraw m -> do render renderer (m^.drawing)
      --                clear renderer
      --                present renderer
      --                continue $ app&appModel .~ m
  -- where
  --   continue app' = appLoop app' renderer

-- | Starts the app
startApp     :: SDLApp -> IO ()
startApp app = do rendererDrawColor (app^.renderer) $= V4 255 255 255 255
                  clear $ app^.renderer
                  present $ app^.renderer
                  appLoop app

main :: IO ()
main = mainWith myApp

mainWith     :: (Renderer -> SDLApp) -> IO ()
mainWith app = do
  initializeAll
  window <- createWindow "mytitle" defaultWindow
  renderer' <- createRenderer window (-1) defaultRenderer
  startApp (app renderer')
  destroyWindow window



--------------------------------------------------------------------------------

renderToFile :: FilePath -> View -> IO ()
renderToFile fp = undefined

-}
