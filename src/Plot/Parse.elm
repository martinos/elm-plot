module Plot.Parse exposing (..)

import Plot.Types
    exposing
        ( Element(..)
        , MetaConfig
        , TickViewConfig
        , LabelViewConfig
        , LabelValues
        , AxisConfig
        , GridConfig
        , GridValues
        , AreaConfig
        , LineConfig
        , AxisScale
        , PlotProps
        , TooltipInfo
        , Orientation(..)
        , Msg(..)
        , Point
        , Style
        )

import Plot.Default as Default
import Plot.Helpers exposing (..)


import Html exposing (Html)
import Html.Attributes
import Html.Events
import Svg exposing (g)
import Svg.Attributes exposing (height, width, d, style)
import Svg.Events exposing (onMouseOver)
import Svg.Lazy
import String
import Task
import Json.Decode as Json
import Dom
import Dom.Position
import Round
import Debug


getScales : Float -> ( Float, Float ) -> ( Float, Float ) -> List Float -> AxisScale
getScales lengthTotal ( offsetLeft, offsetRight ) ( paddingBottomPx, paddingTopPx ) values =
    let
        length =
            lengthTotal - offsetLeft - offsetRight

        lowest =
            getLowest values

        highest =
            getHighest values

        range =
            getRange lowest highest

        paddingTop =
            pixelsToValue length range paddingTopPx

        paddingBottom =
            pixelsToValue length range paddingBottomPx
    in
        { lowest = lowest - paddingBottom
        , highest = highest + paddingTop
        , range = range + paddingBottom + paddingTop
        , length = length
        , offset = offsetLeft
        }


scaleValue : AxisScale -> Float -> Float
scaleValue { length, range } v =
    v * length / range


unScaleValue : AxisScale -> Float -> Float
unScaleValue { length, range } v =
    v * range / length


fromSvgCoords : AxisScale -> AxisScale -> Point -> Point
fromSvgCoords xScale yScale ( x, y ) =
    ( unScaleValue xScale x
    , unScaleValue yScale (yScale.length - y)
    )


toSvgCoordsX : AxisScale -> AxisScale -> Point -> Point
toSvgCoordsX xScale yScale ( x, y ) =
    ( xScale.offset + scaleValue xScale (abs xScale.lowest + x)
    , yScale.offset + scaleValue yScale (yScale.highest - y)
    )


toSvgCoordsY : AxisScale -> AxisScale -> Point -> Point
toSvgCoordsY xScale yScale ( x, y ) =
    toSvgCoordsX xScale yScale ( y, x )


getDiff : Float -> Float -> Float
getDiff a b =
    abs ((abs a) - (abs b))


getClosest : Float -> Float -> Float -> Float
getClosest value candidate closest =
    if getDiff value candidate < getDiff value closest then
        candidate
    else
        closest


toNearestX : List Float -> Float -> Float
toNearestX xValues value =
    List.foldr (getClosest value) 0 xValues


getTooltipInfo : List (Element msg) -> Float -> TooltipInfo
getTooltipInfo elements xValue =
    let
        yValues =
            List.foldr (collectYValues xValue) [] elements
    in
        TooltipInfo xValue yValues


getPlotProps : String -> MetaConfig -> List (Element msg) -> PlotProps
getPlotProps id { size, padding, margin } elements =
    let
        ( xValues, yValues ) =
            List.unzip (List.foldr collectPoints [] elements)

        ( width, height ) =
            size

        ( top, right, bottom, left ) =
            margin

        xScale =
            getScales width ( left, right ) ( 0, 0 ) xValues

        yScale =
            getScales height ( top, bottom ) padding yValues

        xTicks =
            getLastGetTickValues X elements <| xScale

        yTicks =
            getLastGetTickValues Y elements <| yScale
    in
        { scale = xScale
        , oppositeScale = yScale
        , toSvgCoords = toSvgCoordsX xScale yScale
        , oppositeToSvgCoords = toSvgCoordsY xScale yScale
        , fromSvgCoords = fromSvgCoords xScale yScale
        , ticks = xTicks
        , oppositeTicks = yTicks
        , toNearestX = toNearestX xValues
        , getTooltipInfo = getTooltipInfo elements
        , id = id
        }


flipToY : PlotProps -> PlotProps
flipToY plotProps =
    let
        { scale
        , oppositeScale
        , toSvgCoords
        , oppositeToSvgCoords
        , ticks
        , oppositeTicks
        } = plotProps
    in
    { plotProps
    | scale = oppositeScale
    , oppositeScale = scale
    , toSvgCoords = oppositeToSvgCoords
    , oppositeToSvgCoords = toSvgCoords
    , ticks = oppositeTicks
    , oppositeTicks = ticks
    }



-- CALCULATE TICKS


getFirstTickValue : Float -> Float -> Float
getFirstTickValue delta lowest =
    ceilToNearest delta lowest


getTickCount : Float -> Float -> Float -> Float -> Int
getTickCount delta lowest range firstValue =
    floor ((range - (abs lowest - abs firstValue)) / delta)


getDeltaPrecision : Float -> Int
getDeltaPrecision delta =
    logBase 10 delta
        |> floor
        |> min 0
        |> abs


toTickValue : Float -> Float -> Int -> Float
toTickValue delta firstValue index =
    firstValue
        + (toFloat index)
        * delta
        |> Round.round (getDeltaPrecision delta)
        |> String.toFloat
        |> Result.withDefault 0


toTickValuesFromDelta : Float -> AxisScale -> List Float
toTickValuesFromDelta delta { lowest, range } =
    let
        firstValue =
            getFirstTickValue delta lowest

        tickCount =
            getTickCount delta lowest range firstValue
    in
        List.map (toTickValue delta firstValue) (List.range 0 tickCount)


toTickValuesFromCount : Int -> AxisScale -> List Float
toTickValuesFromCount appxCount scale =
    toTickValuesFromDelta (getTickDelta scale.range appxCount) scale


toTickValuesFromList : List Float -> AxisScale -> List Float
toTickValuesFromList values _ =
    values


toTickValuesAuto : AxisScale -> List Float
toTickValuesAuto =
    toTickValuesFromCount 10



-- GET LAST AXIS TICK CONFIG


getAxisConfig : Orientation -> Element msg -> Maybe (AxisConfig msg) -> Maybe (AxisConfig msg)
getAxisConfig orientation element lastConfig =
    case element of
        Axis config ->
            if config.orientation == orientation then
                Just config
            else
                lastConfig

        _ ->
            lastConfig


getLastGetTickValues : Orientation -> List (Element msg) -> AxisScale -> List Float
getLastGetTickValues orientation elements =
    List.foldl (getAxisConfig orientation) Nothing elements
        |> Maybe.withDefault Default.axisConfig
        |> .toTickValues



-- Collect points


collectPoints : Element msg -> List Point -> List Point
collectPoints element allPoints =
    case element of
        Area { points } ->
            allPoints ++ points

        Line { points } ->
            allPoints ++ points

        _ ->
            allPoints


collectYValues : Float -> Element msg -> List (Maybe Float) -> List (Maybe Float)
collectYValues xValue element yValues =
    case element of
        Area { points } ->
            getYValue xValue points :: yValues

        Line { points } ->
            getYValue xValue points :: yValues

        _ ->
            yValues


getYValue : Float -> List Point -> Maybe Float
getYValue xValue points =
    List.foldr (\(x, y) res -> if x == xValue then Just y else res) Nothing points



-- Helpers


(?) : Orientation -> a -> a -> a
(?) orientation x y =
    case orientation of
        X ->
            x

        Y ->
            y
