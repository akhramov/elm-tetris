module Model exposing (..)

import Grid exposing (Grid)
import Messages exposing (Msg(..))

import Color exposing (Color)
import Array exposing (Array)
import Time exposing (Time)
import Figure exposing (Figure)
import Window
import Task

type GameState = Stopped | Running | Paused

type alias Model =
    { position: (Int, Int)
    , grid: Grid
    , currentFigureStates: Maybe (Array Figure)
    , time: Time
    , period: Float
    , accelerationPeriod: Float
    , rotation: Bool
    , figureState: Int
    , score: Int
    , moving: Bool
    , state: GameState
    }


init : (Model, Cmd Msg)
init =
    ({ position = (0, -1)
     , grid = Grid.init 20 (Color.rgb 220 230 255)
     , currentFigureStates = Nothing
     , figureState = 0
     , time = 0
     , period = 500
     , accelerationPeriod = 25
     , rotation = False
     , score = 0
     , moving = False
     , state = Stopped
    }, Task.perform Resize Window.size)
