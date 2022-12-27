--------------------------------------------------------------------------------
-- |
-- Module      :  SDL2MVC.SimpleSDLEvent
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Helpers for working with SDL Events
--
--------------------------------------------------------------------------------
module SDL2MVC.SimpleSDLEvent
  ( pattern KeyPress
  , pattern MouseMove
  , pattern MouseClick, pattern RightClick
  ) where

import Data.Int
import SDL

--------------------------------------------------------------------------------

pattern KeyPress         :: Keycode -> Event
pattern KeyPress keyCode <- (simpleKeyPress -> Just keyCode)

simpleKeyPress    :: Event -> Maybe Keycode
simpleKeyPress e = case eventPayload e of
    KeyboardEvent keyboardEvent
      | keyboardEventKeyMotion keyboardEvent == Pressed ->
          Just $ keysymKeycode (keyboardEventKeysym keyboardEvent)
    _ -> Nothing

--------------------------------------------------------------------------------

pattern MouseMove   :: Point V2 Int32 -> Event
pattern MouseMove p <- (simpleMouseMove -> Just p)

simpleMouseMove   :: SDL.Event -> Maybe (Point V2 Int32)
simpleMouseMove e = case eventPayload e of
      MouseMotionEvent md -> Just $ mouseMotionEventPos md
      _                   -> Nothing

--------------------------------------------------------------------------------

pattern MouseClick  :: Point V2 Int32 -> Event
pattern MouseClick p <- (simpleMouseClick -> Just (ButtonLeft, p))

pattern RightClick  :: Point V2 Int32 -> Event
pattern RightClick p <- (simpleMouseClick -> Just (ButtonRight, p))

simpleMouseClick   :: Event -> Maybe (MouseButton, Point V2 Int32)
simpleMouseClick e = case eventPayload e of
      MouseButtonEvent md -> Just ( mouseButtonEventButton md
                                  , mouseButtonEventPos md
                                  )
      _                   -> Nothing
