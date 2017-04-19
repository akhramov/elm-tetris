module Messages exposing (Msg(..))

import Either exposing (Either(..))
import Time exposing (Time)
import Window

type Msg
    = Tick Time
    | NewFigure Int
    | Rotate
    | Move (Either () ())
    | Accelerate Bool
    | GameStateChange
    | NoOp
    | Resize Window.Size
