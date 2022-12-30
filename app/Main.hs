{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Data.Colour.Names
import HGeometry.Box
import HGeometry.Point
import HGeometry.Vector
import SDL hiding (Point(..), Rectangle(..))
import SDL2MVC.Attribute
import SDL2MVC.Effect
import SDL2MVC.Render
import SDL2MVC.SDLApp
import SDL2MVC.SimpleSDLEvent
import SDL2MVC.View

--------------------------------------------------------------------------------
-- * Model

type R = Double

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
                  , text_ [ Stroke :=> black] (Point2 400 (400 :: R)) "test"
                  , circle_ [ Stroke :=> blue ] (Point2 500 400) (20 :: R)
                  , point_ [ Stroke :=> blue ] (Point2 550 (400 :: R))
                  ]

  where
    r, r2 :: Rectangle (Point 2 R)
    r  = Rectangle (Point2 10 20) (Point2 210 320)
    r2 = Rectangle (Point2 100 300) (Point2 400 500)

--------------------------------------------------------------------------------
-- * Main

myApp :: AppConfig MyAction Model
myApp = AppConfig { _update             = update
                  , _render             = render
                  , _startupAction      = AppAction' Started
                  , _interpretSDLEvent  = HandleEvent
                  , _initialWindowTitle = "MyApp :)"
                  }

main :: IO ()
main = runApp initialModel myApp
