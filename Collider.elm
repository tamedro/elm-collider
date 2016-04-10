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

type alias Game =
  { state : Bool
  , balls : List Ball
  , debug : String
  , gun : Gun
  }

type alias Ball =
  { id : Int
  , x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , r : Float
  }

type alias Gun =
  { dir : Int
  , rotSpeed : Int
  }

type alias UpdateInputs =
  { clickButton : Bool
  , ballSignal : String
  , inputs : Inputs
  }

type alias Inputs =
  { delta : Time
  , enter : Bool
  }
  
type alias BallSignal =
  { radius : String
  , id : Int
  }

-- defaults

defaultGame : Game
defaultGame =
  { state = False
  , balls = []
  , debug = ""
  , gun = defaultGun
  }

defaultBall : Int -> Ball
defaultBall num =
  { id = num
  , x = 0
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

update : UpdateInputs -> Game -> Game
update {clickButton, ballSignal, inputs} ({state,balls,debug,gun} as game) =
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

addBalls : List Ball -> List Ball
addBalls balls = defaultBall (List.length balls) :: balls

spinGun : Gun -> Gun
spinGun gun = 
    { gun | dir = ((gun.dir + gun.rotSpeed) % 360) }
    
-- View

--view : (Int,Int) -> Game -> Element
view ballSignal (w,h) game =
  flow down ([
    (container w h middle <|
      collage gameWidth gameHeight
        ([ rect gameWidth gameHeight
          |> filled (rgb 60 60 60)
        , rect 1 10
          |> filled red          
          |> rotate (0 - (degrees <| toFloat game.gun.dir))
        ] ++ List.map formBall game.balls)
    )
    , toElement 10 50 <| button [style (buttonStyle ++ centerStyle), onClick mb.address <| not game.state] 
                                [Html.text "+"]
    , toElement 10 10 <| div [] [Html.text game.debug]
  ] ++ List.map ballDiv game.balls)

-- View Utilities

formBall : Ball -> Form
formBall ball = 
  oval ball.r ball.r 
    |> filled white
    |> move (ball.x, ball.y)

ballDiv : Ball -> Element
ballDiv ball =
  let evth = Html.Events.on "change" Html.Events.targetValue (Signal.message mbox.address)
  in
  toElement 50 50 <| div [style (centerStyle ++ ballStyle)] 
    [ div [style innerBallStyle] [ Html.text <| toString ball.id ]
    , input 
        [ type' "range"
        , Html.Attributes.min "0"
        , Html.Attributes.max "20"
        , value <| toString ball.r
        , evth
        ] []
    ]
--toElement 50 50 <| div [style (centerStyle ++ ballStyle)] [Html.text <| toString ball.id]


-- Signals

main =
  Signal.map3 view mbox.signal Window.dimensions gameState 

gameState : Signal Game
gameState =
  Signal.foldp update defaultGame inputs

mb : Signal.Mailbox Bool
mb =
  Signal.mailbox False
  
mbox = 
  Signal.mailbox "0"
  --Signal.mailbox {radius = "0", id = 0}

delta =
  Signal.map inSeconds (fps 35)

inputs : Signal UpdateInputs
inputs =
  Signal.map3 UpdateInputs
    mb.signal
    mbox.signal
    (Signal.sampleOn delta <|
      Signal.map2 Inputs
        delta
        Keyboard.enter)

-- Styling

centerStyle : List (String, String)
centerStyle =
    [ ("position", "absolute")
    , ("left", "50%")
    , ("transform", "translate(-50%,-50%)")
    ]

buttonStyle : List (String, String)
buttonStyle =
    [ ("background-color", "#4CAF50")
    , ("border", "none")
    , ("color", "white")
    , ("padding", "10px 32px")
    , ("text-align", "center")
    , ("text-decoration", "none")
    , ("display", "inline-block")
    , ("font-size", "16px")
    ]

ballStyle : List (String, String)
ballStyle = 
    [ ("border-style", "solid")
    , ("padding-top", "10px")
    , ("padding-bottom", "10px")
    , ("padding-left", "10px")
    , ("padding-right", "20px")
    , ("text-align", "center")
    , ("text-decoration", "none")
    , ("font-size", "16px")
    , ("left", "50%")
    , ("width", "200px")
    , ("margin", "5px")
    ]
 
innerBallStyle : List (String, String)
innerBallStyle = 
  [ ("display", "inline-block")
  , ("text-align", "center")
  , ("float", "left")
  , ("padding-left", "15px")
  ]
