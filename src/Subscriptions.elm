module Subscriptions exposing (subscriptions)

import Model exposing (..)
import Messages exposing (Msg(..))

import Either exposing (Either(..))
import Keyboard exposing (KeyCode)
import AnimationFrame
import Window

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.state == Running then
        Sub.batch [AnimationFrame.diffs Tick
                  , Keyboard.downs keyDowns
                  , Keyboard.ups keyUps
                  , Window.resizes Resize
                  ]
    else
        Window.resizes Resize

keyDowns : KeyCode -> Msg
keyDowns keyCode =
    case keyCode of
        37 -> Move (Left ())
        39 -> Move (Right ())
        38 -> Rotate
        40 -> Accelerate True

        _ -> NoOp

keyUps : KeyCode -> Msg
keyUps keyCode =
    case keyCode of
        40 -> Accelerate False

        _ -> NoOp
