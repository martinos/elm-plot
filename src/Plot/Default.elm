module Plot.Default exposing (..)


import Plot.Helpers exposing (..)
import Plot.Types
    exposing
        ( Element(..)
        , MetaConfig
        , TickViewConfig
        , TickView
        , TickAttrFunc
        , LabelViewConfig
        , LabelView
        , LabelAttrFunc
        , LabelValues(..)
        , AxisConfig
        , GridConfig
        , GridValues(..)
        , AreaConfig
        , LineConfig
        , TooltipConfig
        , AxisScale
        , PlotProps
        , TooltipInfo
        , Orientation(..)
        , Msg(..)
        , Point
        , Style
        )

--import Plot.View as View

import Html exposing (Html)
import Html.Attributes
import Svg
import Svg.Attributes
import Round


metaConfig =
    { size = ( 800, 500 )
    , padding = ( 0, 0 )
    , margin = ( 0, 0, 0, 0 )
    , classes = []
    , style = [ ( "padding", "0" ), ( "stroke", "#000" ) ]
    , id = "elm-plot"
    }


tickViewConfig : TickViewConfig
tickViewConfig =
    { length = 7
    , width = 1
    , style = []
    , classes = []
    }


labelViewConfig : LabelViewConfig
labelViewConfig =
    { displace = Nothing
    , format = (\_ -> toString)
    , style = []
    , classes = []
    }


axisConfig =
    { toTickValues = toTickValuesAuto
    , tickView = tickView tickViewConfig
    , labelValues = LabelCustomFilter (\a b -> True)
    , labelView = labelView labelViewConfig
    , style = []
    , classes = []
    , axisLineStyle = []
    , axisCrossing = False
    , orientation = X
    }


gridConfig =
    { values = GridMirrorTicks
    , style = []
    , classes = []
    , orientation = X
    }


tooltipConfig : TooltipConfig msg
tooltipConfig =
    { view = tooltipView
    , showLine = True
    , lineStyle = []
    }


areaConfig =
    { style = []
    , points = []
    }


lineConfig =
    { style = []
    , points = []
    }


tickView : TickViewConfig -> Orientation -> Int -> Float -> Svg.Svg msg
tickView { length, width, style, classes } orientation _ _ =
    let
        displacement =
            (?) orientation "" (toRotate 90 0 0)

        styleFinal =
            style ++ [ ( "stroke-width", (toString width) ++ "px" ) ]
    in
        Svg.line
            [ Svg.Attributes.style (toStyle styleFinal)
            , Svg.Attributes.y2 (toString length)
            , Svg.Attributes.transform displacement
            , Svg.Attributes.class (String.join " " classes)
            ]
            []


tickViewDynamic : TickAttrFunc -> Orientation -> Int -> Float -> Svg.Svg msg
tickViewDynamic toTickAttrs orientation index float =
    let
        tickView =
            toTickView (toTickAttrs index float)
    in
        tickView orientation index float


toTickView : List (TickViewConfig -> TickViewConfig) -> TickView msg
toTickView attrs =
    tickView (List.foldl (<|) tickViewConfig attrs)


toTickViewDynamic : TickAttrFunc -> TickView msg
toTickViewDynamic toTickConfig =
    tickViewDynamic toTickConfig



labelStyleX : ( Style, ( Int, Int ) )
labelStyleX =
    ( [ ( "text-anchor", "middle" ) ], ( 0, 24 ) )


labelStyleY : ( Style, ( Int, Int ) )
labelStyleY =
    ( [ ( "text-anchor", "end" ) ], ( -10, 5 ) )


labelView : LabelViewConfig -> Orientation -> Int -> Float -> Svg.Svg msg
labelView { displace, format, style, classes } orientation index tick =
    let
        ( defaultStyle, defaultDisplacement ) =
            (?) orientation labelStyleX labelStyleY

        ( dx, dy ) =
            Maybe.withDefault defaultDisplacement displace
    in
        Svg.text_
            [ Svg.Attributes.transform (toTranslate ( toFloat dx, toFloat dy ))
            , Svg.Attributes.style (toStyle (defaultStyle ++ style))
            , Svg.Attributes.class (String.join " " classes)
            ]
            [ Svg.tspan [] [ Svg.text (format index tick) ] ]


toLabelView : List (LabelViewConfig -> LabelViewConfig) -> LabelView msg
toLabelView attrs =
   labelView (List.foldl (<|) labelViewConfig attrs)


labelViewDynamic : LabelAttrFunc -> Orientation -> Int -> Float -> Svg.Svg msg
labelViewDynamic toLabelAttrs orientation index float =
    let
        labelView =
            toLabelView (toLabelAttrs index float)
    in
       labelView orientation index float


toLabelViewDynamic : LabelAttrFunc -> LabelView msg
toLabelViewDynamic toLabelConfig =
   labelViewDynamic toLabelConfig



tooltipView : TooltipInfo -> Bool -> Html.Html msg
tooltipView { xValue, yValues } isLeftSide =
    let
        classes =
            [ ( "elm-plot__tooltip__default-view", True )
            , ( "elm-plot__tooltip__default-view--left", isLeftSide )
            , ( "elm-plot__tooltip__default-view--right", not isLeftSide )
            ]
    in
        Html.div
            [ Html.Attributes.classList classes ]
            [ Html.div [] [ Html.text ("X: " ++ toString xValue) ]
            , Html.div [] (List.indexedMap tooltipYValueView yValues)
            ]


tooltipYValueView : Int -> Maybe Float -> Html.Html msg
tooltipYValueView index yValue =
    let
        yValueDisplayed =
            case yValue of
                Just value ->
                    toString value

                Nothing ->
                    "No data"
    in
        Html.div []
            [ Html.span [] [ Html.text ("Serie " ++ toString index ++ ": ") ]
            , Html.span [] [ Html.text yValueDisplayed ]
            ]



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


(?) : Orientation -> a -> a -> a
(?) orientation x y =
    case orientation of
        X ->
            x

        Y ->
            y