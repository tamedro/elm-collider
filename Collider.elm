import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window
import Html exposing (..)
import Html.Events exposing (onClick)
import Html exposing (Html, Attribute, text, toElement, div, input)
import Html.Attributes exposing (..)
import StartApp.Simple exposing (start)

-- Model

(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (300,200)

-- types 

type State = Play | Pause

type Debug = Message | Nothing

type alias Game =
  { state : Bool
  , balls : List Ball
  , debug : String
  , gun : Gun
  }

type alias Ball =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , r : Float
  }

type alias Gun =
  { dir : Int
  , rotSpeed : Int
  }

type alias Update =
  { clickButton : Bool
  , input : Input
  }

type alias Input =
  { delta : Time
  , enter : Bool
  --, add : Bool
  }

-- defaults

defaultGame : Game
defaultGame =
  { state = False
  , balls = []
  , debug = ""
  , gun = defaultGun
  }

defaultBall : Ball
defaultBall =
  { x = 0
  , y = 0
  , vx = -100
  , vy = -100
  , r = 15 
  }

defaultGun : Gun
defaultGun =
  { dir = 0
  , rotSpeed = 5
  }

-- Update

update : Signal.Address Bool ->Update -> Game -> Game
update address {clickButton, input} ({state,balls,debug,gun} as game) =
  let
    newBalls =
      if clickButton /= state then
        addBalls balls
      else 
        balls
    
    newState = clickButton
    
    newGun = spinGun gun
    
  in 
    {
      game |
        gun = newGun,
        balls = newBalls,
        state = newState
    }

{-
  Adds a  ball to the list of balls
-}
addBalls : List Ball -> List Ball
addBalls balls = defaultBall :: balls

spinGun : Gun -> Gun
spinGun gun = 
    { gun | dir = ((gun.dir + gun.rotSpeed) % 360) }
    
-- View

view : Signal.Address Bool -> Bool -> (Int,Int) -> Game -> Element
view address message (w,h) game =
  flow down [
    (container w h middle <|
      collage gameWidth gameHeight
        [ rect gameWidth gameHeight
          |> filled (rgb 60 60 60)
        , rect 1 10
          |> filled red          
          |> rotate (0 - (degrees (toFloat game.gun.dir)))
        ]
    )
    , toElement 100 100 (button [buttonStyle, onClick address (not game.state)] [Html.text (toString (List.length game.balls))])
    , toElement 100 100 (div [] [Html.text game.debug])
  ]


mb : Signal.Mailbox Bool
mb =
  Signal.mailbox False

-- Signals

main =
  Signal.map3 (view mb.address) mb.signal Window.dimensions gameState 

gameState : Signal Game
gameState =
  Signal.foldp (update mb.address) defaultGame input 

delta =
  Signal.map inSeconds (fps 35)

input : Signal Update
input =
  Signal.map2 Update
    mb.signal
    (Signal.sampleOn delta <|
      Signal.map2 Input
        delta
        Keyboard.enter)

-- Styling
buttonStyle : Attribute
buttonStyle =
  style
    [ ("background-color", "#4CAF50")
    , ("border", "none")
    , ("color", "white")
    , ("padding", "10px 32px")
    , ("text-align", "center")
    , ("text-decoration", "none")
    , ("display", "inline-block")
    , ("font-size", "16px")
    , ("position", "absolute")
    , ("left", "50%")
    , ("transform", "translate(-50%,-50%)")
    ]

