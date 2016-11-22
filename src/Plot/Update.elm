module Plot.Update exposing (update)

import Plot.Types as Types exposing (Msg(..), State)
import Plot.Helpers exposing (..)
import Dom.Position
import Task

update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Hovering plotProps eventPosition ->
            ( { state | waiting = True }, getPosition plotProps eventPosition )

        ReceivePosition result ->
            case result of
                Ok position ->
                    if state.waiting then
                        ( { state | position = Just position }, Cmd.none )
                    else
                        ( state, Cmd.none )

                Err err ->
                    ( state, Cmd.none )

        ResetPosition ->
            ( { position = Nothing, waiting = False }, Cmd.none )


getPosition : Types.PlotProps -> ( Float, Float ) -> Cmd Msg
getPosition plotProps eventPosition =
    Task.map2
        (getRelativePosition plotProps eventPosition)
        (Dom.Position.left (getInnerId plotProps))
        (Dom.Position.top (getInnerId plotProps))
        |> Task.attempt ReceivePosition


getRelativePosition : Types.PlotProps -> ( Float, Float ) -> Float -> Float -> ( Float, Float )
getRelativePosition { fromSvgCoords, toNearestX } ( mouseX, mouseY ) left top =
    let
        ( x, y ) =
            fromSvgCoords ( mouseX - left, mouseY - top )
    in
        ( toNearestX x, y )