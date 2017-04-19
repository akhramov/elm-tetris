module View exposing (view)

import Model exposing (..)
import Messages exposing (Msg(..))
import Grid exposing (Grid)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

view : Model -> Html Msg
view model =
    div [] [
         div [scoreStyles] [ text (String.padLeft 6 '0' <|toString model.score) ]
         , gameMap model ]

scoreStyles : Html.Attribute a
scoreStyles  =
    style [("display", "flex")
         , ("justify-content", "center")
         , ("font-size", "50px")]

gameMapStyles : Html.Attribute a
gameMapStyles =
    style [("display", "flex"), ("justify-content", "center")]

gameMap : Model -> Html Msg
gameMap { grid } =
    div [ gameMapStyles, onClick (GameStateChange)] (Grid.toHtml grid)
