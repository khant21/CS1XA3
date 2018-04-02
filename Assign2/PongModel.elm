module PongModel exposing (..)

import Keyboard exposing (..)
import Set exposing (Set)

-- data type for the pong ball
type alias Ball = {
    x: Float,
    y: Float,
    vx: Float,
    vy: Float
}

-- data type for the paddle
type alias Player = {
    x: Float,
    y: Float,
    vx: Float,
    vy: Float,
    score: Int
}

-- different difficulty settings
type Difficulty
  = Beginner
  | Normal
  | Ninja

-- choose how many players
type Mode
  = OnePlayer
  | TwoPlayer

--states during the actual gameplay
type State = Play | Pause

--Used for setting dimensions of the game
type alias WindowDimensions = (Int, Int)


-- models the different screens that the game can be in
type Model =
    ModeScreen
    | Initializing Mode
    | Playing Player Player Ball Mode Difficulty State WindowDimensions (Set KeyCode)
    | GameOver Player Player

{-
    GAME NOTES
        starting: Mode selecton
        Next: Difficulty selection
        Next: Play pong
        Next: Game over screen
        Next: Mode selection 

-}