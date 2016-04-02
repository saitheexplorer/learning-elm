import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)
import Signal
import Keyboard
import Time
import Debug
import Window

type alias Position = {x: Int, y: Int}

sprite =
  circle 50 |> filled red

scene : Position -> (Int, Int) -> Element
scene position (w,h) =
  let
    (x, y) = (position.x, position.y)
    (dx, dy) = (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
    log = Debug.watch "position" (position.x, position.y)
    log2 = Debug.watch "delta" (dx, dy)
  in
    collage w h [
      sprite |> move (dx, dy)
    ]

input : Signal Position
input =
  Signal.sampleOn (Time.fps 30) Keyboard.arrows

initialPosition : Position
initialPosition = {x = 100, y = 100}

updatePosition : Position -> Position -> Position
updatePosition delta old =
  { x = old.x + delta.x * 10
  , y = old.y - delta.y * 10
  }

main : Signal Element
main =
  Signal.map2 scene (Signal.foldp updatePosition initialPosition input) Window.dimensions
