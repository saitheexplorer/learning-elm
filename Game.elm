import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)
import Signal
import Keyboard
import Time
import Debug
import Window

type Direction = Left | Right
type alias Keys = {x: Int, y: Int}

type alias Model =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , dir : Direction
  }

robot : Model
robot =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , dir = Right
  }

walk : Keys -> Model -> Model
walk keys model =
  { model |
      vx = toFloat keys.x * 5,
      dir =
        if keys.x < 0 then Left
        else if keys.x > 0 then Right
        else model.dir
  }
jump : Keys -> Model -> Model
jump keys model =
  if keys.y > 0 && model.vy == 0 then
    { model | vy = 6.0 }
  else model

gravity : Float -> Model -> Model
gravity dt model =
  { model |
      vy = if model.y > 0 then model.vy - dt/4 else 0
  }


physics : Float -> Model -> Model
physics dt model =
  { model |
      x = max 0 (model.x + dt * model.vx)
    , y = max 0 (model.y + dt * model.vy)
  }

update : (Float, Keys) -> Model -> Model
update (dt, keys) model =
  let
    log = Debug.watchSummary "model" .x model
  in
    model
      |> gravity dt
      |> walk keys
      |> jump keys
      |> physics dt

-- VIEWS

view : (Int, Int) -> Model -> Element
view (w, h) model =
  let
    verb =
      if model.y > 0 then "jump"
      else if model.vx /= 0 then "run"
      else "stand"

    dir =
      case model.dir of
        Left -> "left"
        Right -> "right"

    src = "/assets/game/img/player/" ++ verb ++ "_" ++ dir ++ ".gif"

    player =
      image 100 100 src

    groundY = 100 - toFloat h/2
    leftX = 50 - toFloat w/2

    position =
      (model.x + leftX , model.y + groundY)
  in
    collage w h [
      player
        |> toForm
        |> move position
    ]

-- SIGNALS

input : Signal (Float, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) (Time.fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)

main : Signal Element
main =
  let
    state = Signal.foldp update robot input
  in
    Signal.map2 view Window.dimensions state


