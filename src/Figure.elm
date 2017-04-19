module Figure exposing (Figure, figures)

import Array exposing (Array(..))
import Color exposing (Color)

type alias Figure = (Color, List (Int, Int))

figures : Array (Array Figure)
figures = Array.fromList [is, js, ls, os, ss, zs, ts]

mirrorFigures : Array Figure -> Array Figure
mirrorFigures figures =
    let
        figures_ = Array.toList figures

        offset =
            figures_
               |> List.unzip
               |> Tuple.second
               |> List.concat
               |> List.unzip
               |> Tuple.first
               |> List.maximum
               |> Maybe.withDefault 0
               |> (//) 2

        mirrorFigure (color, coords) =
            (Color.complement color
            , List.map (\(x, y) -> (2 * offset - x, y)) coords
            )
    in
        Array.fromList <| List.map mirrorFigure figures_

is : Array Figure
is = Array.fromList [((Color.rgb 60 214 99), [ (0, 1), (1, 1), (2, 1), (3, 1) ])
      , ((Color.rgb 60 214 99), [ (2, 0), (2, 1), (2, 2), (2, 3) ])
      , ((Color.rgb 60 214 99), [ (0, 2), (1, 2), (2, 2), (3, 2) ])
      , ((Color.rgb 60 214 99), [ (1, 0), (1, 1), (1, 2), (1, 3) ])]

js : Array Figure
js = Array.fromList [((Color.rgb 214 99 60), [ (0, 0), (0, 1), (1, 1), (2, 1) ])
                   , ((Color.rgb 214 99 60), [ (1, 0), (1, 1), (1, 2), (2, 0) ])
                   , ((Color.rgb 214 99 60), [ (0, 1), (1, 1), (2, 1), (2, 2) ])
                   , ((Color.rgb 214 99 60), [ (0, 2), (1, 2), (1, 1), (1, 0) ])]

ls : Array Figure
ls = mirrorFigures js

os : Array Figure
os = Array.fromList [((Color.rgb 50 60 20), [ (1, 0), (2, 0), (1, 1), (2, 1) ])]

ss : Array Figure
ss = Array.fromList [((Color.rgb 60 99 214), [ (1, 0), (2, 0), (0, 1), (1, 1) ])
        , ((Color.rgb 60 99 214), [ ( 1, 0 ), (1, 1), (2, 1), (2, 2) ])
        , ((Color.rgb 60 99 214), [ ( 1, 1 ), (2, 1), (0, 2), (1, 2) ])
        , ((Color.rgb 60 99 214), [ ( 0, 0 ), (0, 1), (1, 1), (1, 2) ])]

zs : Array Figure
zs = mirrorFigures ss

ts : Array Figure
ts = Array.fromList [((Color.rgb 99 111 60), [ (1, 0), (0, 1), (1, 1), (2, 1) ])
                    , ((Color.rgb 99 111 60), [ (1, 0), (1, 1), (1, 2), (2, 1) ])
                    , ((Color.rgb 99 111 60), [ (0, 1), (1, 1), (2, 1), (1, 2) ])
                    , ((Color.rgb 99 111 60), [ (0, 1), (1, 0), (1, 1), (1, 2) ])]
