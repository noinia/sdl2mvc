{-# LANGUAGE  TemplateHaskell  #-}
{-# LANGUAGE  OverloadedStrings  #-}
module Main where

import           Control.Lens
import           Data.Colour.Names
import           Data.Ext
import           HGeometry.Line
import           HGeometry.Point
import           Prelude hiding (lines)
import qualified SDL
import           SDL2MVC.Attribute
import           SDL2MVC.Effect
import           SDL2MVC.Render
import           SDL2MVC.SDLApp
import           SDL2MVC.SimpleSDLEvent
import           SDL2MVC.View

--------------------------------------------------------------------------------
-- * Model

type R = Double

data Space = Space { _points :: [Point 2 R :+ Color ]
                   , _lines  :: [LineEQ R :+ Color ]
                   }
           deriving (Show,Eq)
makeLenses ''Space

instance Semigroup Space where
  (Space pts lns) <> Space pts' lns' = Space (pts <> pts') (lns <> lns')
instance Monoid Space where
  mempty = Space mempty mempty



-- | The model of our app
data Model = Model { _primal  :: Space
                     -- ^ the geometries in the primal space that the user created
                   , _dual    :: Space
                     -- ^ the geometries in the sual space that the user created
                   }
           deriving (Show,Eq)
makeLenses ''Model


initialModel :: Model
initialModel = Model { _primal = Space [ origin     :+ red
                                       , Point2 1 3 :+ green
                                       , Point2 2 1 :+ blue
                                       ]
                                       [ LineEQ 1 1 :+ black ]
                     , _dual   = mempty
                     }


--------------------------------------------------------------------------------
-- * Controller

data MyAction = AddPoint (Point 2 R) Color
              | HandleEvent SDL.Event
  deriving (Show,Eq)

update   :: Model -> MyAction -> Effect (Action MyAction Model) Model
update m = \case
  AddPoint p c -> noEff $ m&primal.points %~ ((p :+ c) :)
  HandleEvent e -> case e of
    MouseClick p      -> m <# pure (AppAction' $ AddPoint p black)
    _                 -> noEff m

--------------------------------------------------------------------------------
-- * View

space_ s = group_ $ pts <> lns
  where
    pts = [ point_ [ Fill :=> c ] p | p :+ c <- s^.points ]
    -- lns = [ line_ [ Stroke :=> c ] l | l :+ c <- s^.lines ]
    lns = []

render   :: Model -> View (Action MyAction Model)
render m = group_ [ primalSpace
                  , dualSpace
                  ]
  where
    primalSpace = space_ (m^.primal)
    dualSpace = Blank

--------------------------------------------------------------------------------
-- * Main

myApp :: AppConfig MyAction Model
myApp = AppConfig { _update             = update
                  , _render             = render
                  , _startupAction      = Skip
                  , _interpretSDLEvent  = HandleEvent
                  , _initialWindowTitle = "Duality Viewer"
                  }

main :: IO ()
main = runApp initialModel myApp
