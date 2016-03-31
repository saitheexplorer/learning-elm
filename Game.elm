module Game where

import Graphics.Element exposing (..)
import Keyboard exposing ( arrows )

type Action = Up | Down | Left | Right

type alias Player =
  { x : Int
  , y : Int
  }

initPlayer : Player
initPlayer =
  { x = 500
  , y = 500
  }

limitRange =
  clamp 0 1000

movePlayer : {x : Int, y : Int} -> Player -> Player
movePlayer action player =
  { player |
      y = limitRange (player.y - action.y * 10)
    , x = limitRange (player.x + action.x * 10)
  }

render : Player -> Element
render player =
  let
    element = show player
  in
    container player.x player.y middle element


main : Signal Element
main =
  Signal.map render (Signal.foldp movePlayer initPlayer Keyboard.arrows)
