{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad
import Data.Colour (opaque)
import Data.Colour.Names
import Data.Text (Text)
import Data.Word
import Debug.Trace
import SDL
import SDL2MVC.Attribute
import SDL2MVC.Effect
import SDL2MVC.Render
import SDL2MVC.SDLApp
import SDL2MVC.SimpleSDLEvent
import SDL2MVC.View

--------------------------------------------------------------------------------
-- * Model

newtype Model = Model { _theColor :: Color }
  deriving (Show,Eq)

makeLenses ''Model

initialModel :: Model
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
    KeyPress KeycodeR -> noEff $ m&theColor .~ red
    KeyPress KeycodeB -> noEff $ m&theColor .~ blue
    MouseMove p       -> m <# do print p
                                 pure Skip
    _                 -> noEff m

--------------------------------------------------------------------------------
-- * View

render   :: Model -> View (Action MyAction Model)
render m = group_ [ rectangle_ [ Fill   :=> (m^.theColor)
                               , Stroke :=> black
                               ] r
                  , rectangle_ [ Stroke :=> green ] r2
                  , text_ [ Stroke :=> black] (P (V2 400 400)) "test"
                  ]

  where
    r = SDL.Rectangle (P (V2 10 20)) (V2 200 300)
    r2 = SDL.Rectangle (P (V2 100 300)) (V2 300 200)

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
