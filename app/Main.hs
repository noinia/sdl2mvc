{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad
import Data.Text (Text)
import Data.Word
import SDL
import SDL2MVC (someFunc)


import Debug.Trace

{-

data Effect action model = Effect model [Sub action]


data Drawing action model = Blank
                          -- | Rectangle

data App action model = App { _model         :: model
                            , _update        :: model -> action -> Effect action model
                            , _view          :: model -> Drawing action model
                            , _subs          :: [Sub action]
                            , _initialAction :: action
                            }
-- makeLenses ''App


data Model action model = Model { _appModel :: model
                                , _drawing  :: Drawing action model
                                }

data Action action model = Quit
                         | Skip
                         | AppAction action
                         | Redraw (Drawing action model)

update = undefined

view'   :: Model -> Drawing Action Model
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

data Drawing = Blank
             | Color (V4 Word8) Drawing
             deriving (Eq,Show)




data Model = Model { _drawing     :: Drawing
                   , _title   :: Text
                   }
           deriving (Show)
makeLenses ''Model

data SDLApp = SDLApp { _appModel :: Model
                     }
makeLenses ''SDLApp


myApp :: SDLApp
myApp = SDLApp (Model Blank "foo")

handle             :: Model -> Event -> Action Model
handle model event
  -- | traceShow event False = undefined
  | eventIsPress KeycodeQ = Quit
  | eventIsPress KeycodeR = traceShowId $ Redraw $ model&drawing .~ Color (V4 0 0 255 255) Blank
  | otherwise             = Skip
  where
    eventIsPress keyCode =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == keyCode
          _ -> False

handleAll        :: Model -> [Event] -> Action Model
handleAll model0 = go model0 Skip
  where
    go model act = \case
      []     -> act
      (e:es) -> case handle model e of
                  Quit                 -> Quit
                  Skip                 -> go model  act es
                  act'@(Redraw model') -> go model' act' es

-- handle                                  :: Model -> [Event] -> Action Model
-- handle model events
--     | any (eventIsPress KeycodeQ) events = Quit
--     | any (eventIsPress KeycodeR) events =
--         Redraw $ model&drawing .~ Color (V4 0 0 255 255) Blank
--     | otherwise                          = Skip
--   where
--     eventIsPress keyCode event =
--         case eventPayload event of
--           KeyboardEvent keyboardEvent ->
--             keyboardEventKeyMotion keyboardEvent == Pressed &&
--             keysymKeycode (keyboardEventKeysym keyboardEvent) == keyCode
--           _ -> False

render          :: Renderer -> Drawing -> IO ()
render renderer = go
  where
    go = \case
      Blank     -> pure ()
      Color c d -> do rendererDrawColor renderer $= c
                      go d

appLoop              :: SDLApp -> Renderer -> IO ()
appLoop app renderer = do
    events <- pollEvents
    case traceShowId $ handleAll (app^.appModel) events of
      Quit     -> pure ()
      Skip     -> continue app
      Redraw m -> do render renderer (m^.drawing)
                     clear renderer
                     present renderer
                     continue $ app&appModel .~ m
  where
    continue app' = appLoop app' renderer

-- | Starts the app
startApp              :: SDLApp -> Renderer -> IO ()
startApp app renderer = do rendererDrawColor renderer $= V4 255 255 255 255
                           clear renderer
                           present renderer
                           appLoop app renderer

main :: IO ()
main = mainWith myApp

mainWith     :: SDLApp -> IO ()
mainWith app = do
  initializeAll
  window <- createWindow (app^.appModel.title) defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  startApp app renderer
  destroyWindow window



--------------------------------------------------------------------------------

renderToFile :: FilePath -> Drawing -> IO ()
renderToFile fp = undefined
