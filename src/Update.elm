module Update exposing (update)

import Model exposing (..)
import Messages exposing (Msg(..))
import Figure exposing (Figure, figures)
import Grid exposing (..)

import Random
import Array exposing (Array)
import Either exposing (Either(..))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick time ->
            if model.grid.settled then
                (model, getNewFigure)
            else
                if model.period < model.time then
                    let
                       model_ = model
                               |> moveFigure (Tuple.mapSecond (\y -> y + 1) model.position)
                               |> removeFullRows
                    in
                        ({ model_ | moving = False, rotation = False, time = 0 }, Cmd.none)
                else
                    ({ model | time = model.time + time }, Cmd.none)


        Rotate ->
            (rotateFigure model, Cmd.none)

        Move direction ->
            let
                position = case (direction, model.moving) of
                               (Left (), False) -> Tuple.mapFirst (\x -> x - 1) model.position
                               (Right (), False) -> Tuple.mapFirst (\x -> x + 1) model.position
                               _ -> model.position
            in
               (moveFigure position model, Cmd.none)

        NewFigure num ->
            let
                grid_ = model.grid
                grid = { grid_ | settled = False }
                currentFigureStates = Array.get num figures
                position = (2, 0)
                model_ = { model | grid = grid, currentFigureStates = currentFigureStates, figureState = 0, position = position }
                maybeFigure = getFigure model_ 0
            in
                case maybeFigure of
                    Just figure ->
                        case Grid.isPositionFree grid figure position of
                            True ->
                                (model_, Cmd.none)
                            False ->
                                ({ model | state = Stopped }, Cmd.none)
                    Nothing ->
                        (model, Cmd.none)

        GameStateChange ->
            case model.state of
                Stopped -> Tuple.mapFirst (\model -> { model | state = Running }) Model.init
                Running -> ({ model | state = Paused }, Cmd.none)
                Paused -> ({ model | state = Running }, Cmd.none)

        Accelerate on ->
            let
                period = if on then model.accelerationPeriod else calculateFrequency model 0
            in
                ({ model | period = period }, Cmd.none)

        Resize { width, height } ->
            let
                grid = model.grid
                a = Debug.log (toString (width, height))
                grid_ = { grid | blockSize = (height - 100) // 20 }
            in
                a

                ({ model | grid = grid_ }, Cmd.none)

        NoOp -> (model, Cmd.none)



moveFigure : (Int, Int) -> Model -> Model
moveFigure newPosition ({ grid, position } as model) =
    case getFigure model 0 of
        Just figure ->
            let
                (grid_, position_) = Grid.move grid figure newPosition position
            in

                { model | position = position_, grid = grid_ }

        Nothing -> model


rotateFigure : Model -> Model
rotateFigure model =
    case (getFigure model 0, getFigure model 1, model.grid.settled) of
        (Just figure, Just nextFigure, False) ->
            let
                (grid, position, rotated) = Grid.rotate model.grid figure nextFigure model.position
            in
                if rotated then
                    { model | grid = grid, position = position, figureState = getState model 1 }
                else
                    model

        _ -> model

removeFullRows : Model -> Model
removeFullRows model =
    let
        (grid, rowsCleared) = Grid.removeSettledRows model.grid
        period = if rowsCleared > 0 then calculateFrequency model rowsCleared else model.period
    in
        { model | grid = grid, score = model.score + rowsCleared, period = period }


calculateFrequency : Model -> Int -> Float
calculateFrequency model subtrahend =
    let
        { accelerationPeriod, period }  = (Tuple.first Model.init)
        newPeriod = period - ((toFloat model.score) + (toFloat subtrahend)) * 5
    in
        case newPeriod > accelerationPeriod of
            True -> newPeriod
            False -> accelerationPeriod

getNewFigure : Cmd Msg
getNewFigure =
    Random.generate NewFigure (Random.int 0 <| (Array.length figures) - 1)


getFigure : Model -> Int -> Maybe Figure
getFigure ({ currentFigureStates, figureState } as model) offset =
    case currentFigureStates of
        Just array ->
            Array.get (getState model offset) array

        Nothing -> Nothing


getState : Model -> Int -> Int
getState { currentFigureStates, figureState } offset =
    case currentFigureStates of
        Just array ->
            let
                length = Array.length array
            in
                (figureState + offset) % length

        Nothing -> 0
