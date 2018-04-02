module MyPong exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Text 
import Char 
import Time exposing (..)
import Window 
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Keyboard exposing (..)
import Set exposing (Set)
import Task
import AnimationFrame

import PongModel exposing(..)

{- Main -}
--main : Program Never (Model, Cmd Msg) Msg
main = Html.program
               { init = init--, initialSizeCmd)
               , view = view
               , update = update
               , subscriptions = subscriptions 
               }

-- these are all of the elements that prompt what aspects to update
type Msg = 
          KeyDown KeyCode
         | KeyUp KeyCode
         | KeyPress Keyboard.KeyCode
         | WindowResize (Int,Int)
         | Tick Float
         | Mode PongModel.Mode
         | Difficulty PongModel.Difficulty
         | NoOp

-- INITIALIZE EVERYTHING:

(gameWidth, gameHeight) = (600, 400)
(halfWidth, halfHeight) = (gameWidth / 2, gameHeight / 2)

initialSizeCmd : Cmd Msg
initialSizeCmd =
  Task.perform sizeToMsg (Window.size)

sizeToMsg : Window.Size -> Msg
sizeToMsg size =
  WindowResize (size.width, size.height)

init : (Model,Cmd Msg)
init = let
      model = ModeScreen
  in (model,Cmd.none)

--initialize the player and ball
initialP1 = (player (20 - halfWidth))
initialP2 = (player (halfWidth - 20))
initBall  = { x = 0, y = 0, vx = 200, vy = 200 }

player : Float -> Player
player initialX =
  { x = initialX
  , y = 0
  , vx = 0
  , vy = 0
  , score = 0
  }

type alias Input = {
    space : Bool,
    reset : Bool,
    pause : Bool,
    dir   : Int,
    dir1  : Int,
    delta : Time
}


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = case model of
    Initializing _ ->
        Keyboard.downs KeyDown

    Playing _ _ _ _ _ _ _ _ ->
        Sub.batch
            [ Keyboard.downs KeyDown
            , Keyboard.ups KeyUp
            , Window.resizes sizeToMsg
            , AnimationFrame.diffs Tick
            ]

    GameOver _ _-> 
        Keyboard.presses KeyPress

    _ ->
        Sub.none

-- VIEW

--this is a link to a bunch of other states that the game can be in
view : Model -> Html Msg
view model = let
      wrapper = (\body -> div [] [ body ] )
    in case model of
        ModeScreen -> wrapper (modeView model)
        Initializing _ -> wrapper (difficultyView model)
        Playing _ _ _ _ _ _ _ _ -> wrapper (pongView model)
        GameOver _ _-> wrapper (finishView model)

--Display the two different options for the mode
modeView : Model -> Html Msg
modeView model = div [style [("background-color", "tomato"), ("color", "white"), ("text-align", "center")]]
                    [ h1 [] [Html.text "Select Mode"]
                    , button [ (onClick (Mode OnePlayer)), style [("padding", "10px")] ] [ Html.text "One Player" ]
                    , button [ (onClick (Mode TwoPlayer)), style [("padding", "10px")] ] [ Html.text "Two Player" ]
                    ]

--Display the different difficulty options
difficultyView : Model -> Html Msg
difficultyView model = div [style [("background-color", "tomato"), ("color", "white"), ("text-align", "center")]]
                    [ h1 [] [Html.text "Select Difficulty"]
                    ,  button [ (onClick (Difficulty Beginner)), style [("padding", "10px")] ] [ Html.text "Beginner" ]
                    ,  button [ (onClick (Difficulty Normal)), style [("padding", "10px")] ] [ Html.text "Normal" ]
                    ,  button [ (onClick (Difficulty Ninja)), style [("padding", "10px")] ] [ Html.text "Ninja" ]
                    ,  p [style [("padding", "10px")]] [Html.text "* Press any key to begin after this screen *"]
                    ]

--This is the actual game display
pongView : Model -> Html Msg
pongView model = case model of
    (Playing player1 player2 ball mode difficulty state windowDimensions keys) ->
          let scores : Element
              scores = txt (Text.height 50) (toString player1.score ++ "  " ++ toString player2.score)
              (w,h) = windowDimensions
          in
              toHtml <|
              container w h middle <|
              collage gameWidth gameHeight
                [ rect gameWidth gameHeight
                    |> filled pongGreen
                , verticalLine gameHeight
                    |> traced (dashed red)
                , oval 15 15
                    |> make ball
                , rect 10 40
                    |> make player1
                , rect 10 40
                    |> make player2
                , toForm scores
                    |> move (0, gameHeight/2 - 40)
                , toForm (statusMessage state)
                    |> move (0, 40 - gameHeight/2)
                ]
    _ -> Html.text ""

--this is the Game Over screen
finishView : Model -> Html Msg
finishView model = div [style [("background-color", "tomato"), ("color", "white"), ("text-align", "center")]]
                    [ h1 [] [ Html.text "Game Over!" ]
                    , p [] [ Html.text "Press SPACE to return to the main menu." ]
                    ]

--the following is a number of helper functions for the main game view

statusMessage state =
    case state of
        Play    -> txt identity ""
        Pause   -> txt identity pauseMessage

verticalLine height =
     path [(0, height), (0, -height)]

pongGreen = rgb 60 100 60
textGreen = rgb 160 200 160
txt f = Text.fromString >> Text.color textGreen >> Text.monospace >> f >> leftAligned
pauseMessage = "SPACE to start, P to pause, R to reset, WS and &uarr;&darr; to move"

make obj shape =
    shape
      |> filled white
      |> move (obj.x,obj.y)

-- UPDATE

update : Msg -> Model -> (Model,Cmd Msg)
update msg model = case model of

  -- the home screen
    ModeScreen -> 
        case msg of
            Mode OnePlayer -> (Initializing OnePlayer,Cmd.none)
            Mode TwoPlayer -> (Initializing TwoPlayer,Cmd.none)
            _              -> (Initializing TwoPlayer, Cmd.none)

  -- difficulty screen update
    Initializing mode ->
        case msg of
            Difficulty Beginner -> 
                (Playing initialP1 initialP2 initBall mode Beginner Pause (0,0) Set.empty, Cmd.none)
            
            Difficulty Normal -> 
                (Playing initialP1 initialP2 initBall mode Normal Pause (0,0) Set.empty, Cmd.none)
            
            Difficulty Ninja -> 
                (Playing initialP1 initialP2 initBall mode Ninja Pause (0,0) Set.empty, Cmd.none)

            _ -> (Playing initialP1 initialP2 initBall mode Ninja Pause (0,0) Set.empty, Cmd.none)

    -- update the pong field
    Playing p1 p2 ball mode diff state window keys ->
        case msg of
            KeyDown key ->
                let newKey = Set.insert key keys
                in (Playing p1 p2 ball mode diff state (600, 400) newKey,Cmd.none)
            
            KeyUp key ->
                let newKeyUp = Set.remove key keys
                in (Playing p1 p2 ball mode diff state (600, 400) newKeyUp,Cmd.none)
            
            Tick delta ->
              if (p1.score == 5) || (p2.score == 5)
              then
                  (GameOver p1 p2, Cmd.none)
              else
                  let input = getInput (model) delta
                  in (updateGame input model, Cmd.none)
            
            WindowResize (600, 400) ->
              (Playing p1 p2 ball mode diff state (600, 400) keys,Cmd.none)
            
            NoOp -> 
              (model, Cmd.none)
            _ -> (model, Cmd.none)

    -- FILLERRRRRR
    GameOver p1 p2 ->
      case msg of
          KeyPress 32 ->
            (ModeScreen, Cmd.none)

          _ ->
            (model,Cmd.none)

--helper function
getInput : Model -> Float -> Input
getInput model delta = case model of
    (Playing p1 p2 ball mode diff state window keys) ->
          { space = Set.member (Char.toCode ' ') (keys)
           , reset = Set.member (Char.toCode 'R') (keys)
           , pause = Set.member (Char.toCode 'P') (keys)
           , dir = if Set.member 87 (keys) then 1 -- down arrow
                   else if Set.member 83 (keys) then -1 -- up arrow
                   else 0
           , dir1 = if Set.member 38 (keys) then 1 -- down arrow
                   else if Set.member 40 (keys) then -1 -- up arrow
                   else 0
           , delta = inSeconds delta
           }

    _ -> 
        { space = False
        , reset = False
        , pause = False
        , dir = 0
        , dir1 = 0
        , delta = inSeconds delta
        }

--helper function that handles the different difficulties and modes
updateGame : Input -> Model -> Model
updateGame {space, reset, pause, dir, dir1, delta} model = 
  case model of
    (Playing player1 player2 ball mode diff state window keys) ->
        
        --if (player1.score == 5) || (player2.score == 5)

        case diff of
            
            Beginner ->

                case mode of

                    OnePlayer ->
                      let score1 = if ball.x >  halfWidth then 1 else 0
                          score2 = if ball.x < -halfWidth then 1 else 0

                          newState =
                              if  space then Play 
                              else if (pause) then Pause 
                              else if (score1 /= score2) then Pause 
                              else state

                          newBall =
                              if state == Pause
                                  then ball
                                  else updateBall delta ball player1 player2

                    in
                        if reset
                           then (Playing initialP1 initialP2 initBall mode diff Pause (0,0) Set.empty)
                           else (Playing (updatePlayer delta dir score1 player1) (updateComputer newBall score2 player2) newBall mode diff newState window keys)


                    TwoPlayer ->
                      let score1 = if ball.x >  halfWidth then 1 else 0
                          score2 = if ball.x < -halfWidth then 1 else 0

                          newState =
                              if  space then Play 
                              else if (pause) then Pause 
                              else if (score1 /= score2) then Pause 
                              else state

                          newBall =
                              if state == Pause
                                  then ball
                                  else updateBall delta ball player1 player2

                    in
                        if reset
                           then (Playing initialP1 initialP2 initBall mode diff Pause (0,0) Set.empty)
                           else (Playing (updatePlayer delta dir score1 player1) (updatePlayer delta dir1 score2 player2) newBall mode diff newState window keys)

            Normal ->

                case mode of

                    OnePlayer ->
                      let score1 = if ball.x >  halfWidth then 1 else 0
                          score2 = if ball.x < -halfWidth then 1 else 0

                          newState =
                              if  space then Play 
                              else if (pause) then Pause 
                              else if (score1 /= score2) then Pause 
                              else state

                          newBall =
                              if state == Pause
                                  then ball
                                  else updateBall (1.5*delta) ball player1 player2

                    in
                        if reset
                           then (Playing initialP1 initialP2 initBall mode diff Pause (0,0) Set.empty)
                           else (Playing (updatePlayer (1.5*delta) dir score1 player1) (updateComputer newBall score2 player2) newBall mode diff newState window keys)


                    TwoPlayer ->
                      let score1 = if ball.x >  halfWidth then 1 else 0
                          score2 = if ball.x < -halfWidth then 1 else 0

                          newState =
                              if  space then Play 
                              else if (pause) then Pause 
                              else if (score1 /= score2) then Pause 
                              else state

                          newBall =
                              if state == Pause
                                  then ball
                                  else updateBall (1.5*delta) ball player1 player2

                    in
                        if reset
                           then (Playing initialP1 initialP2 initBall mode diff Pause (0,0) Set.empty)
                           else (Playing (updatePlayer (1.5*delta) dir score1 player1) (updatePlayer (1.5*delta) dir1 score2 player2) newBall mode diff newState window keys)

            Ninja ->

                case mode of

                    OnePlayer ->
                      let score1 = if ball.x >  halfWidth then 1 else 0
                          score2 = if ball.x < -halfWidth then 1 else 0

                          newState =
                              if  space then Play 
                              else if (pause) then Pause 
                              else if (score1 /= score2) then Pause 
                              else state

                          newBall =
                              if state == Pause
                                  then ball
                                  else updateBall (2.5*delta) ball player1 player2

                    in
                        if reset
                           then (Playing initialP1 initialP2 initBall mode diff Pause (0,0) Set.empty)
                           else (Playing (updatePlayer (2.5*delta) dir score1 player1) (updateComputer newBall score2 player2) newBall mode diff newState window keys)


                    TwoPlayer ->
                      let score1 = if ball.x >  halfWidth then 1 else 0
                          score2 = if ball.x < -halfWidth then 1 else 0

                          newState =
                              if  space then Play 
                              else if (pause) then Pause 
                              else if (score1 /= score2) then Pause 
                              else state

                          newBall =
                              if state == Pause
                                  then ball
                                  else updateBall (2.5*delta) ball player1 player2

                    in
                        if reset
                           then (Playing initialP1 initialP2 initBall mode diff Pause (0,0) Set.empty)
                           else (Playing (updatePlayer (2.5*delta) dir score1 player1) (updatePlayer (2.5*delta) dir1 score2 player2) newBall mode diff newState window keys)

    _ -> model

-- the following are a number of update helper functions 

updateBall : Time -> Ball -> Player -> Player -> Ball
updateBall t ({x, y, vx, vy} as ball) p1 p2 =
  if not (ball.x |> near 0 halfWidth)
    then { ball | x = 0, y = 0 }
    else physicsUpdate t
            { ball |
                vx = stepV vx (within ball p1) (within ball p2),
                vy = stepV vy (y < 7-halfHeight) (y > halfHeight-7)
            }


updatePlayer : Time -> Int -> Int -> Player -> Player
updatePlayer t dir points player =
  let player1 = physicsUpdate  t { player | vy = toFloat dir * 200 }
  in
      { player1 |
          y = clamp (22 - halfHeight) (halfHeight - 22) player1.y,
          score = player.score + points
      }

updateComputer : Ball -> Int -> Player -> Player
updateComputer ball points player =
    { player |
        y = clamp (22 - halfHeight) (halfHeight - 22) ball.y,
        score = player.score + points
    }

physicsUpdate t ({x, y, vx, vy} as obj) =
  { obj |
      x = x + vx * t,
      y = y + vy * t
  }

near : Float -> Float -> Float -> Bool
near k c n =
    n >= k-c && n <= k+c

within : Ball -> Player -> Bool
within ball paddle =
    near paddle.x 8 ball.x && near paddle.y 20 ball.y


stepV v lowerCollision upperCollision =
  if lowerCollision then abs v
  else if upperCollision then 0 - abs v
  else v