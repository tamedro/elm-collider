import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window
import Html exposing (div, button, text)
import Html.Events exposing (onClick)

-- Model

(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (300,200)

type alias Ball =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  }

type State = Play | Pause

type alias Game =
  { state : State
  , balls : List Ball
  }

defaultGame : Game
defaultGame =
  { state = Pause
  , balls = []
  }

type alias Input =
  { space : Bool
  , enter : Bool
  , dir1 : Int
  , dir2 : Int
  , delta : Time
  }
  
-- Update
update : Input -> Game -> Game
update {space,enter,dir1,dir2,delta} ({state,balls} as game) =
    game
    
-- View


view : (Int,Int) -> Game -> Element
view (w,h) game =
  container w h middle <|
    collage gameWidth gameHeight
      [ rect gameWidth gameHeight
          |> filled (rgb 60 60 60)
      ]

-- Signals

main =
  Signal.map2 view Window.dimensions gameState

gameState : Signal Game
gameState =
  Signal.foldp update defaultGame input

delta =
  Signal.map inSeconds (fps 35)
  
input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map5 Input
      Keyboard.space
      Keyboard.enter
      (Signal.map .y Keyboard.wasd)
      (Signal.map .y Keyboard.arrows)
      delta
