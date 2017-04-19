module Grid exposing (Grid, init, toHtml, move, rotate, removeSettledRows, isPositionFree)

import Collage
import Element
import Html exposing (Html)
import Color exposing (Color)

import Matrix exposing (Matrix)
import List.Extra as List

import Figure exposing (Figure)

type CellState = Moving | Settled | Empty
type alias Cell = (Color, CellState)

type alias Grid =
    { grid: Matrix Cell
    , width: Int
    , height: Int
    , blockSize: Int
    , color: Color
    , settled: Bool
    }


init : Int -> Color -> Grid
init blockSize color =
    let
        width = 10
        height = 20
    in
        { grid = Matrix.matrix width height (always (color, Empty))
        , blockSize = blockSize
        , height = height
        , width = width
        , color = color
        , settled = True
        }


move : Grid -> Figure -> (Int, Int) -> (Int, Int) -> (Grid, (Int, Int))
move grid figure newPosition oldPosition =
    let
        gridWithoutFigure = drawFigure grid.grid (grid.color, Empty) oldPosition figure

        moveIsPossible = isPositionFree grid figure newPosition
        state = if (moveIsPossible || Tuple.second oldPosition == Tuple.second newPosition) then Moving else Settled

        position = if moveIsPossible then newPosition else oldPosition
    in
        ({ grid |
              grid = drawFigure gridWithoutFigure (Tuple.first figure, state) position figure,
              settled = state == Settled
        },
             position
        )

rotate : Grid -> Figure -> Figure -> (Int, Int) -> (Grid, (Int, Int), Bool)
rotate grid oldFigure newFigure position  =
    let
        gridWithoutFigure = drawFigure grid.grid (grid.color, Empty) position oldFigure

        rotationIsPossible offset =
            let
                shift = Tuple.mapFirst (\x -> offset + x)
                position_ = shift position
                coords = List.map shift (Tuple.second newFigure)
            in
                isPositionFree grid (Tuple.first newFigure, coords) position_


        offset =
            [0, 1, -1]
                |> List.find rotationIsPossible

        (figure, position_) =
            case offset of
                Just offset ->
                    (newFigure, Tuple.mapFirst (\x -> 2 * offset + x) position)
                Nothing -> (oldFigure, position)
    in
        ({ grid |
              grid = drawFigure gridWithoutFigure (Tuple.first figure, Moving) position_ figure }
        , position_, figure /= oldFigure)


removeSettledRows : Grid -> (Grid, Int)
removeSettledRows grid =
    let
        list
            = grid.grid
              |> Matrix.toList
              |> List.transpose
              |> List.filterNot (\row -> List.all (\cell -> Tuple.second cell == Settled) row)

        newRowsCount = grid.height - (List.length list)

        grid_ =
            list
                |> List.append (List.repeat newRowsCount (List.repeat grid.width (grid.color, Empty)))
                |> List.transpose
                |> Matrix.fromList

    in
        ({ grid | grid = grid_ }, newRowsCount)


isPositionFree : Grid -> Figure -> (Int, Int) -> Bool
isPositionFree { grid } (_, coords) (x, y) =
    let
        isPositionFree_ (figureX, figureY) =
            case Matrix.get (figureX + x, figureY + y) grid of
                Just (_, Empty) -> True
                Just (_, Moving) -> True
                _ -> False
    in
        List.all isPositionFree_ coords


drawFigure : Matrix Cell -> Cell -> (Int, Int) -> Figure -> Matrix Cell
drawFigure grid cell (x, y) (figureColor, figureCoords) =
    let
        figureCoords_ = List.map (\(x_, y_) -> (x_ + x, y_ + y)) figureCoords

        fn location elem =
            if List.member location figureCoords_ then
                cell
            else
                elem
    in
        Matrix.mapWithLocation fn grid

toHtml : Grid -> List (Html a)
toHtml grid =
    grid
        |> toForms
        |> (uncurry Collage.collage) (originalSize grid)
        |> Element.toHtml
        |> List.singleton

toForms : Grid -> List Collage.Form
toForms grid =
    grid.grid
        |> Matrix.mapWithLocation (drawBlock grid)
        |> Matrix.toList
        |> List.concat

drawBlock : Grid -> Matrix.Location -> Cell -> Collage.Form
drawBlock { blockSize, width, height } location (color, _) =
    let
        block = drawSquare ((toFloat blockSize) - 0.2) color
    in
        Collage.move (getCellLocation width height blockSize location) block

drawSquare : Float -> Color -> Collage.Form
drawSquare blockSize color =
    Collage.filled color (Collage.square blockSize)

getCellLocation : Int -> Int -> Int -> Matrix.Location -> (Float, Float)
getCellLocation width height blockSize location =
    let
        row = ((toFloat <| Matrix.row location) + (1 - toFloat width) / 2) * (toFloat blockSize)
        col = ((toFloat <| Matrix.col location) + (1 - toFloat height) / 2) * -(toFloat blockSize)
    in
        (row, col)

originalSize : Grid -> (Int, Int)
originalSize { width, height, blockSize } = (width * blockSize, height * blockSize)
