module Plot.View
    exposing
        ( plot
        , elements
        , element
        , axis
        , grid
        , gridLine
        , area
        , line
        )

import Plot.Types
    exposing
        ( Element(..)
        , MetaConfig
        , TickViewConfig
        , LabelViewConfig
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

import Plot.Helpers exposing (..)
import Plot.Parse exposing (..)



getEventPostion : PlotProps -> Json.Decoder Msg
getEventPostion plotProps =
    Json.map2
        (\x y -> Hovering plotProps ( x, y ))
        (Json.field "clientX" Json.float)
        (Json.field "clientY" Json.float)



plot : MetaConfig -> PlotProps -> ( List (Svg.Svg Msg), List (Html.Html Msg) ) -> Svg.Svg Msg
plot { size, style, classes, margin } plotProps ( svgViews, htmlViews ) =
    let
        ( width, height ) =
            size

        ( top, right, bottom, left ) =
            margin

        paddingStyle =
            String.join "px " <| List.map toString [ top, right, bottom, left ]
    in
        Html.div
            [ Html.Attributes.class "elm-plot"
            , Html.Attributes.id plotProps.id
            ]
            [ Svg.svg
                [ Svg.Attributes.height (toString height)
                , Svg.Attributes.width (toString width)
                , Svg.Attributes.class "elm-plot__inner"
                ]
                svgViews
            , Html.div
                [ Html.Attributes.class "elm-plot__html" ]
                [ Html.div
                    [ Html.Attributes.class "elm-plot__html__inner"
                    , Html.Attributes.id (getInnerId plotProps)
                    , Html.Attributes.style
                        [ ( "width", toString (width - left - right) ++ "px" )
                        , ( "height", toString (height - top - bottom) ++ "px" )
                        , ( "padding", paddingStyle ++ "px" )
                        ]
                    , Html.Events.on "mousemove" (getEventPostion plotProps)
                    --, Html.Events.onMouseOut ResetPosition
                    ]
                    htmlViews
                ]
            ]



-- VIEW ELEMENTS


elements : PlotProps -> List (Element Msg) -> ( List (Svg.Svg Msg), List (Html.Html Msg) )
elements plotProps elements =
    List.foldr (element plotProps) ( [], [] ) elements


element : PlotProps -> Element Msg -> ( List (Svg.Svg Msg), List (Html.Html Msg) ) -> ( List (Svg.Svg Msg), List (Html.Html Msg) )
element plotProps element ( svgViews, htmlViews ) =
    case element of
        Axis config ->
            let
                plotPropsFitted =
                    case config.orientation of
                        X ->
                            plotProps

                        Y ->
                            flipToY plotProps
            in
                ( (axis plotPropsFitted config) :: svgViews, htmlViews )

        Tooltip config position ->
            let
                tooltipView = tooltip plotProps config position
            in
                ( svgViews, tooltipView :: htmlViews )

        Grid config ->
            let
                plotPropsFitted =
                    case config.orientation of
                        X ->
                            plotProps

                        Y ->
                            flipToY plotProps
            in
                ( (grid plotPropsFitted config) :: svgViews, htmlViews )

        Line config ->
            ( (line plotProps config) :: svgViews, htmlViews )

        Area config ->
            ( (area plotProps config) :: svgViews, htmlViews )



-- VIEW AXIS


filterTicks : Bool -> List Float -> List Float
filterTicks axisCrossing ticks =
    if axisCrossing then
        List.filter (\p -> p /= 0) ticks
    else
        ticks


zipWithDistance : Bool -> Int -> Int -> Float -> ( Int, Float )
zipWithDistance hasZero lowerThanZero index tick =
    let
        distance =
            if tick == 0 then
                0
            else if tick > 0 && hasZero then
                index - lowerThanZero
            else if tick > 0 then
                index - lowerThanZero + 1
            else
                lowerThanZero - index
    in
        ( distance, tick )


indexTicks : List Float -> List ( Int, Float )
indexTicks ticks =
    let
        lowerThanZero =
            List.length (List.filter (\i -> i < 0) ticks)

        hasZero =
            List.any (\t -> t == 0) ticks
    in
        List.indexedMap (zipWithDistance hasZero lowerThanZero) ticks


axis : PlotProps -> AxisConfig Msg -> Svg.Svg Msg
axis plotProps { toTickValues, tickView, labelView, labelValues, style, classes, axisLineStyle, axisCrossing, orientation } =
    let
        { scale, oppositeScale, toSvgCoords, oppositeToSvgCoords } =
            plotProps

        tickPositions =
            toTickValues scale
                |> filterTicks axisCrossing
                |> indexTicks

        labelPositions =
            case labelValues of
                LabelCustomValues values ->
                    indexTicks values

                LabelCustomFilter filter ->
                    List.filter (\( a, b ) -> filter a b) tickPositions
    in
        Svg.g
            [ Svg.Attributes.style (toStyle style)
            , Svg.Attributes.class (String.join " " classes)
            ]
            [ gridLine plotProps axisLineStyle 0
            , Svg.g [] (List.map (placeTick plotProps (tickView orientation)) tickPositions)
            , Svg.g [] (List.map (placeTick plotProps (labelView orientation)) labelPositions)
            ]


placeTick : PlotProps -> (Int -> Float -> Svg.Svg Msg) -> ( Int, Float ) -> Svg.Svg Msg
placeTick { toSvgCoords } view ( index, tick ) =
    Svg.g [ Svg.Attributes.transform (toTranslate (toSvgCoords ( tick, 0 ))) ] [ view index tick ]



-- VIEW GRID


getGridPositions : List Float -> GridValues -> List Float
getGridPositions tickValues values =
    case values of
        GridMirrorTicks ->
            tickValues

        GridCustomValues customValues ->
            customValues


grid : PlotProps -> GridConfig -> Svg.Svg Msg
grid plotProps { values, style, classes } =
    let
        { scale, toSvgCoords, oppositeTicks } =
            plotProps

        positions =
            getGridPositions oppositeTicks values
    in
        Svg.g
            [ Svg.Attributes.class (String.join " " classes) ]
            (List.map (gridLine plotProps style) positions)


gridLine : PlotProps -> Style -> Float -> Svg.Svg Msg
gridLine { toSvgCoords, scale } style position =
    let
        { lowest, highest } =
            scale

        ( x1, y1 ) =
            toSvgCoords ( lowest, position )

        ( x2, y2 ) =
            toSvgCoords ( highest, position )

        attrs =
            Svg.Attributes.style (toStyle style) :: (toPositionAttr x1 y1 x2 y2)
    in
        Svg.line attrs []



-- VIEW TOOLTIP


tooltip : PlotProps -> TooltipConfig Msg -> ( Float, Float ) -> Html.Html Msg
tooltip { toSvgCoords, scale, getTooltipInfo } { showLine, view } position =
    let
        info =
            getTooltipInfo (Tuple.first position)

        ( xSvg, ySvg ) =
            toSvgCoords (info.xValue, 0)

        flipped =
            xSvg < scale.length / 2

        lineView =
            if showLine then [ tooltipLine ( xSvg, ySvg ) ] else []
    in
        Html.div
            [ Html.Attributes.class "elm-plot__tooltip"
            , Html.Attributes.style [ ( "left", (toString xSvg) ++ "px" ) ]
            ]
            ((view info flipped) :: lineView)


tooltipLine : ( Float, Float ) -> Html.Html Msg
tooltipLine ( x, y ) =
    Html.div
        [ Html.Attributes.class "elm-plot__tooltip__line" 
        , Html.Attributes.style
            [ ( "left", (toString x) ++ "px" ) 
            , ( "height", toString y ++ "px")]
        ]
        []



-- VIEW AREA


area : PlotProps -> AreaConfig -> Svg.Svg a
area { toSvgCoords } { points, style } =
    let
        range =
            List.map Tuple.first points

        ( lowestX, highestX ) =
            ( getLowest range, getHighest range )

        svgCoords =
            List.map toSvgCoords points

        ( highestSvgX, originY ) =
            toSvgCoords ( highestX, 0 )

        ( lowestSvgX, _ ) =
            toSvgCoords ( lowestX, 0 )

        startInstruction =
            toInstruction "M" [ lowestSvgX, originY ]

        endInstructions =
            toInstruction "L" [ highestSvgX, originY ]

        instructions =
            coordToInstruction "L" svgCoords
    in
        Svg.path
            [ Svg.Attributes.d (startInstruction ++ instructions ++ endInstructions ++ "Z")
            , Svg.Attributes.style (toStyle style)
            ]
            []



-- VIEW LINE


line : PlotProps -> LineConfig -> Svg.Svg a
line { toSvgCoords } { points, style } =
    let
        svgPoints =
            List.map toSvgCoords points

        ( startInstruction, tail ) =
            startPath svgPoints

        instructions =
            coordToInstruction "L" svgPoints
    in
        Svg.path
            [ Svg.Attributes.d (startInstruction ++ instructions)
            , Svg.Attributes.style (toStyle style)
            ]
            []


