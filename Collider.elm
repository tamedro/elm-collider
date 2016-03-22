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

-- Model

(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (300,200)

-- types 

type State = Play | Pause

type alias Game =
  { state : State
  , balls : List Ball
  }

type alias Ball =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , r : Float
  }

type alias Input =
  { delta : Time
  }

-- defaults

defaultGame : Game
defaultGame =
  { state = Pause
  , balls = []
  }

defaultBall : Ball
defaultBall =
  { x = 0
  , y = 0
  , vx = -100
  , vy = -100
  , r = 15 
  }
  
-- Update

update : Input -> Game -> Game
update {delta} ({state,balls} as game) =
    game

{-
  Adds a  ball to the list of balls
-}
addBalls : List Ball -> List Ball
addBalls balls = defaultBall :: balls
    
-- View

view : (Int,Int) -> Game -> Element
view (w,h) game =
  flow down [
    (container w h middle <|
      collage gameWidth gameHeight
        [ rect gameWidth gameHeight
          |> filled (rgb 60 60 60)
        ]
    )
    , toElement 100 100 (button [buttonStyle] [Html.text "+"])
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
    Signal.map Input
      delta
      
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

