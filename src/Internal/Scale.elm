module Internal.Scale exposing (..)

import Internal.Types exposing (..)
import Internal.Stuff exposing (..)


type alias ScaleConfig =
    { length : Float
    , padding : Edges
    , margin : Edges
    , restrictRange : EdgesAny (Float -> Float)
    }


toLengthInner : ScaleConfig -> Float
toLengthInner { length, margin } =
    length - margin.lower - margin.upper


getScale : ScaleConfig -> List Value -> Maybe Edges -> Scale
getScale ({ length, padding, margin, restrictRange } as config) values pileEdges =
    let
        lengthInner =
            toLengthInner config

        lowest =
            getScaleLowest restrictRange.lower values pileEdges

        highest =
            getScaleHighest restrictRange.upper values pileEdges

        range =
            getRange lowest highest

        paddingTop =
            pixelsToValue lengthInner range padding.upper

        paddingBottom =
            pixelsToValue lengthInner range padding.lower
    in
        { lowest = lowest - paddingBottom
        , highest = highest + paddingTop
        , range = range + paddingBottom + paddingTop
        , length = lengthInner
        , offset = margin.lower
        } |> Debug.log "here"


getScaleLowest : (Float -> Float) -> List Value -> Maybe Edges -> Value
getScaleLowest restrictRange values pileEdges =
    getAutoLowest pileEdges (getLowest values)
        |> restrictRange


getAutoLowest : Maybe Edges -> Value -> Value
getAutoLowest pileEdges lowestFromValues =
    case pileEdges of
        Just { lower } ->
            min lower lowestFromValues

        Nothing ->
            lowestFromValues


getScaleHighest : (Float -> Float) -> List Value -> Maybe Edges -> Value
getScaleHighest restrictRange values pileEdges =
    getAutoHighest pileEdges (getHighest values)
        |> restrictRange


getAutoHighest : Maybe Edges -> Value -> Value
getAutoHighest pileEdges highestFromValues =
    case pileEdges of
        Just { upper } ->
            max upper highestFromValues

        Nothing ->
            highestFromValues


scaleValue : Scale -> Value -> Value
scaleValue { length, range, offset } v =
    (v * length / range) + offset


unScaleValue : Scale -> Value -> Value
unScaleValue { length, range, offset, lowest } v =
    ((v - offset) * range / length) + lowest


fromSvgCoords : Scale -> Scale -> Point -> Point
fromSvgCoords xScale yScale ( x, y ) =
    ( unScaleValue xScale x
    , unScaleValue yScale (yScale.length - y)
    )


toSvgCoordsX : Scale -> Scale -> Point -> Point
toSvgCoordsX xScale yScale ( x, y ) =
    ( scaleValue xScale (abs xScale.lowest + x)
    , scaleValue yScale (yScale.highest - y)
    )


toSvgCoordsY : Scale -> Scale -> Point -> Point
toSvgCoordsY xScale yScale ( x, y ) =
    toSvgCoordsX xScale yScale ( y, x )
