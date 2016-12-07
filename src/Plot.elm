module Plot
    exposing
        ( Attribute
        , ScaleAttribute
        , plot
        , plotInteractive
        , xAxis
        , yAxis
        , verticalGrid
        , horizontalGrid
        , hint
        , area
        , line
        , pile
        , scatter
        , custom
        , classes
        , id
        , style
        , scaleX
        , scaleY
        , rangeLower
        , rangeUpper
        , length
        , marginLower
        , marginUpper
        , paddingLower
        , paddingUpper
        , Element
        , initialState
        , update
        , Interaction(..)
        , State
        , getHoveredValue
        , Point
        , Style
        )

{-|
 This library aims to allow you to visualize a variety of graphs in
 an intuitve manner without comprimising flexibility regarding configuration.
 It is insprired by the elm-html api, using the `element attrs children` pattern.

# Definitions
@docs Attribute, ScaleAttribute, Element, Point, Style

# Elements
@docs plot, plotInteractive, scatter, line, area, pile, xAxis, yAxis, hint, verticalGrid, horizontalGrid, custom

# Styling and sizes
@docs classes, id, style, scaleX, scaleY

# Scaling and sizing
@docs length, marginUpper, marginLower, paddingUpper, paddingLower, rangeUpper, rangeLower 

# State
@docs State, initialState, update, Interaction, getHoveredValue


-}

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Svg.Lazy
import Task
import Json.Decode as Json
import Dom
import Dom.Position
import Plot.Axis as Axis
import Plot.Tick as Tick
import Plot.Grid as Grid
import Plot.Area as Area
import Plot.Pile as Pile
import Plot.Scatter as Scatter
import Plot.Line as Line
import Plot.Hint as Hint
import Internal.Grid as GridInternal
import Internal.Axis as AxisInternal
import Internal.Pile as PileInternal
import Internal.Bars as BarsInternal
import Internal.Area as AreaInternal
import Internal.Scatter as ScatterInternal
import Internal.Line as LineInternal
import Internal.Tick as TickInternal
import Internal.Hint as HintInternal
import Internal.Stuff exposing (..)
import Internal.Types exposing (..)
import Internal.Draw exposing (..)
import Internal.Scale exposing (..)


{-| Convinience type to represent coordinates.
-}
type alias Point =
    ( Float, Float )


{-| Convinience type to represent style.
-}
type alias Style =
    List ( String, String )


{-| Represents a child element of the plot.
-}
type Element msg
    = Line (LineInternal.Config msg) (List Point)
    | Area (AreaInternal.Config msg) (List Point)
    | Pile PileInternal.Config (List (PileInternal.Element msg)) PileMeta
    | Scatter (ScatterInternal.Config msg) (List Point)
    | Hint (HintInternal.Config msg) (Maybe Point)
    | Axis (AxisInternal.Config msg)
    | Grid (GridInternal.Config msg)
    | CustomElement ((Point -> Point) -> Svg.Svg msg)


type alias Config =
    { classes : List String
    , style : Style
    , scaleConfig : Oriented ScaleConfig
    , id : String
    }


defaultConfig : Config
defaultConfig =
    { classes = []
    , style = [ ( "padding", "0" ), ( "stroke", "#000" ) ]
    , scaleConfig = Oriented defaultScaleConfigX defaultScaleConfigY
    , id = "elm-plot"
    }


defaultScaleConfigX : ScaleConfig
defaultScaleConfigX =
    { length = 800
    , padding = Edges 0 0
    , margin = Edges 0 0
    , restrictRange = EdgesAny identity identity
    }


defaultScaleConfigY : ScaleConfig
defaultScaleConfigY =
    { defaultScaleConfigX
    | length = 500
    , restrictRange = EdgesAny (min 0) identity
    }


{-| -}
type alias Attribute =
    Config -> Config


{-| -}
type alias ScaleAttribute =
    ScaleConfig -> ScaleConfig


{-| Adds padding to your plot, meaning extra space below
 and above the lowest and highest point in your plot.
 The unit is pixels and the format is `( bottom, top )`.

 Default: `( 0, 0 )`
-}
paddingUpper : Int -> ScaleAttribute
paddingUpper upper ({ padding } as config) =
    { config | padding = { padding | upper = toFloat upper } }


{-| Adds padding to your plot, meaning extra space below
 and above the lowest and highest point in your plot.
 The unit is pixels and the format is `( bottom, top )`.

 Default: `( 0, 0 )`
-}
paddingLower : Int -> ScaleAttribute
paddingLower lower ({ padding } as config) =
    { config | padding = { padding | lower = toFloat lower } }


{-| Specify the size of your plot in pixels and in the format
 of `( width, height )`.

 Default: `( 800, 500 )`
-}
length : Int -> ScaleAttribute
length length config =
    { config | length = toFloat length }


{-| Adds padding to your plot, meaning extra space below
 and above the lowest and highest point in your plot.
 The unit is pixels and the format is `( bottom, top )`.

 Default: `( 0, 0 )`
-}
marginUpper : Int -> ScaleAttribute
marginUpper upper ({ margin } as config) =
    { config | margin = { margin | upper = toFloat upper } }


{-| Adds margin to your plot, meaning extra space below
 and above the lowest and highest point in your plot.
 The unit is pixels and the format is `( bottom, top )`.

 Default: `( 0, 0 )`
-}
marginLower : Int -> ScaleAttribute
marginLower lower ({ margin } as config) =
    { config | margin = { margin | lower = toFloat lower } }


{-| Adds padding to your plot, meaning extra space below
 and above the lowest and highest point in your plot.
 The unit is pixels and the format is `( bottom, top )`.

 Default: `( 0, 0 )`
-}
rangeUpper : (Float -> Float) -> ScaleAttribute
rangeUpper rangeConfig ({ restrictRange } as config) =
    { config | restrictRange = { restrictRange | upper = rangeConfig } }


{-| Adds range to your plot, meaning extra space below
 and above the lowest and highest point in your plot.
 The unit is pixels and the format is `( bottom, top )`.

 Default: `( 0, 0 )`
-}
rangeLower : (Float -> Float) -> ScaleAttribute
rangeLower rangeConfig ({ restrictRange } as config) =
    { config | restrictRange = { restrictRange | lower = rangeConfig } }


{-| -}
scaleX : List ScaleAttribute -> Attribute
scaleX attributes ({ scaleConfig } as config) =
    { config | scaleConfig = { scaleConfig | x = List.foldl (<|) defaultScaleConfigX attributes } }


{-| -}
scaleY : List ScaleAttribute -> Attribute
scaleY attributes ({ scaleConfig } as config) =
    { config | scaleConfig = { scaleConfig | y = List.foldl (<|) defaultScaleConfigY attributes } }


{-| Adds styles to the svg element.
-}
style : Style -> Attribute
style style config =
    { config | style = defaultConfig.style ++ style ++ [ ( "padding", "0" ) ] }


{-| Adds classes to the svg element.
-}
classes : List String -> Attribute
classes classes config =
    { config | classes = classes }


{-| Adds an id to the svg element.

 **Note:** If you have more than one plot in your DOM,
 then you most provide a unique id using this attribute for
 the hint to work!
-}
id : String -> Attribute
id id config =
    { config | id = id }


{-| -}
xAxis : List (Axis.Attribute msg) -> Element msg
xAxis attrs =
    Axis (List.foldl (<|) AxisInternal.defaultConfigX attrs)


{-| -}
yAxis : List (Axis.Attribute msg) -> Element msg
yAxis attrs =
    Axis (List.foldl (<|) AxisInternal.defaultConfigY attrs)


{-| -}
horizontalGrid : List (Grid.Attribute msg) -> Element msg
horizontalGrid attrs =
    Grid (List.foldr (<|) GridInternal.defaultConfigX attrs)


{-| -}
verticalGrid : List (Grid.Attribute msg) -> Element msg
verticalGrid attrs =
    Grid (List.foldr (<|) GridInternal.defaultConfigY attrs)


{-| Draws an area.

    myPlot : Svg.Svg msg
    myPlot =
        plot
            []
            [ area [] [ ( 0, -2 ), ( 2, 0 ), ( 3, 1 ) ] ]
-}
area : List (Area.Attribute msg) -> List Point -> Element msg
area attrs points =
    Area (List.foldr (<|) AreaInternal.defaultConfig attrs) points


{-| Draws an line.

    myPlot : Svg.Svg msg
    myPlot =
        plot [] [ line [] [ ( 0, 1 ), ( 2, 2 ), ( 3, 4 ) ] ]
-}
line : List (Line.Attribute msg) -> List Point -> Element msg
line attrs points =
    Line (List.foldr (<|) LineInternal.defaultConfig attrs) points


{-| Draws a scatter.

    myPlot : Svg.Svg msg
    myPlot =
        plot
            []
            [ scatter [] [ ( 0, -2 ), ( 2, 0 ), ( 3, 1 ) ] ]
-}
scatter : List (Scatter.Attribute msg) -> List Point -> Element msg
scatter attrs points =
    Scatter (List.foldr (<|) ScatterInternal.defaultConfig attrs) points


{-| Draws a bar chart.

    myPlot : Svg.Svg msg
    myPlot =
        plot
            []
            [ bars [] [ ( 0, -2 ), ( 2, 0 ), ( 3, 1 ) ] ]
-}
pile : List Pile.Attribute -> List (Pile.Element msg) -> Element msg
pile attrs barsConfigs =
    let
        config =
            List.foldr (<|) PileInternal.defaultConfig attrs
    in
        Pile config barsConfigs (PileInternal.toPileMeta config barsConfigs)


{-| -}
hint : List (Hint.Attribute msg) -> Maybe Point -> Element msg
hint attrs position =
    Hint (List.foldr (<|) HintInternal.defaultConfig attrs) position


{-| -}
custom : ((Point -> Point) -> Svg.Svg msg) -> Element msg
custom view =
    CustomElement view


{-| This is the function processing your entire plot configuration.
 Pass your meta attributes and plot elements to this function and
 a svg plot will be returned!
-}
plot : List Attribute -> List (Element msg) -> Svg msg
plot attrs =
    Svg.Lazy.lazy2 parsePlot (toPlotConfig attrs)


{-| So this is like `plot`, except the message to is `Interaction yourMsg`. It's a message wrapping
 your message, so you can use down the build in inteactions in the plot as well as adding your own.
 See example here (if I forget to insert link, please let me know).
-}
plotInteractive : List Attribute -> List (Element (Interaction yourMsg)) -> Svg (Interaction yourMsg)
plotInteractive attrs =
    Svg.Lazy.lazy2 parsePlotInteractive (toPlotConfig attrs)


toPlotConfig : List Attribute -> Config
toPlotConfig =
    List.foldl (<|) defaultConfig



-- MODEL


{-| -}
type State
    = State
        { position : Maybe ( Float, Float )
        , waiting : Bool
        }


{-| -}
initialState : State
initialState =
    State
        { position = Nothing
        , waiting = True
        }



-- UPDATE


{-| -}
type Interaction msg
    = Internal Msg
    | Custom msg


type Msg
    = Hovering Meta ( Float, Float )
    | ReceivePosition (Result Dom.Error Point)
    | ResetPosition


{-| -}
update : Msg -> State -> ( State, Cmd (Interaction msg) )
update msg (State state) =
    case msg of
        Hovering meta eventPosition ->
            ( State { state | waiting = True }, cmdPosition meta eventPosition )

        ReceivePosition result ->
            case result of
                Ok position ->
                    if state.waiting && positionChanged state.position position then
                        ( State { state | position = Just position }, Cmd.none )
                    else
                        ( State state, Cmd.none )

                Err err ->
                    ( State state, Cmd.none )

        ResetPosition ->
            ( State { position = Nothing, waiting = False }, Cmd.none )


{-| Get the hovered position from state.
-}
getHoveredValue : State -> Maybe Point
getHoveredValue (State { position }) =
    position


positionChanged : Maybe ( Float, Float ) -> ( Float, Float ) -> Bool
positionChanged position ( left, top ) =
    case position of
        Nothing ->
            True

        Just ( leftOld, topOld ) ->
            topOld /= top || leftOld /= left


cmdPosition : Meta -> ( Float, Float ) -> Cmd (Interaction msg)
cmdPosition meta eventPosition =
    Task.map2
        (getRelativePosition meta eventPosition)
        (Dom.Position.left meta.id)
        (Dom.Position.top meta.id)
        |> Task.attempt ReceivePosition
        |> Cmd.map Internal


getRelativePosition : Meta -> ( Float, Float ) -> Float -> Float -> Point
getRelativePosition { fromSvgCoords, toNearestX } ( mouseX, mouseY ) left top =
    let
        ( x, y ) =
            fromSvgCoords ( mouseX - left, mouseY - top )
    in
        ( toNearestX x, y )



-- VIEW


parsePlot : Config -> List (Element msg) -> Svg msg
parsePlot config elements =
    let
        meta =
            calculateMeta config elements
    in
        viewPlot config meta (viewElements meta elements)


parsePlotInteractive : Config -> List (Element (Interaction msg)) -> Svg (Interaction msg)
parsePlotInteractive config elements =
    let
        meta =
            calculateMeta config elements
    in
        viewPlotInteractive config meta (viewElements meta elements)


viewPlotInteractive : Config -> Meta -> ( List (Svg (Interaction msg)), List (Html (Interaction msg)) ) -> Html (Interaction msg)
viewPlotInteractive ({ scaleConfig } as config) meta ( svgViews, htmlViews ) =
    Html.div
        (plotAttributes config ++ plotAttributesInteraction meta)
        (viewSvg scaleConfig svgViews :: htmlViews)


viewPlot : Config -> Meta -> ( List (Svg msg), List (Html msg) ) -> Svg msg
viewPlot ({ scaleConfig } as config) meta ( svgViews, htmlViews ) =
    Html.div
        (plotAttributes config)
        (viewSvg scaleConfig svgViews :: htmlViews)


plotAttributes : Config -> List (Html.Attribute msg)
plotAttributes { scaleConfig, id } =
    [ Html.Attributes.class "elm-plot"
    , Html.Attributes.style <| sizeStyle scaleConfig
    , Html.Attributes.id id
    ]


plotAttributesInteraction : Meta -> List (Html.Attribute (Interaction msg))
plotAttributesInteraction meta =
    [ Html.Events.on "mousemove" (getMousePosition meta)
    , Html.Events.onMouseLeave (Internal ResetPosition)
    ]


viewSvg : Oriented ScaleConfig -> List (Svg msg) -> Svg msg
viewSvg { x, y } views =
    Svg.svg
        [ Svg.Attributes.height (toString y.length)
        , Svg.Attributes.width (toString x.length)
        , Svg.Attributes.class "elm-plot__inner"
        ]
        views


getMousePosition : Meta -> Json.Decoder (Interaction msg)
getMousePosition meta =
    Json.map2
        (\x y -> Internal <| Hovering meta ( x, y ))
        (Json.field "clientX" Json.float)
        (Json.field "clientY" Json.float)


sizeStyle : Oriented ScaleConfig -> Style
sizeStyle { x, y } =
    [ ( "height", toPixels y.length ), ( "width", toPixels x.length ) ]


viewElements : Meta -> List (Element msg) -> ( List (Svg msg), List (Html msg) )
viewElements meta elements =
    List.foldr (viewElement meta) ( [], [] ) elements


viewElement : Meta -> Element msg -> ( List (Svg msg), List (Html msg) ) -> ( List (Svg msg), List (Html msg) )
viewElement meta element ( svgViews, htmlViews ) =
    case element of
        Line config points ->
            ( (LineInternal.view meta config points) :: svgViews, htmlViews )

        Area config points ->
            ( (AreaInternal.view meta config points) :: svgViews, htmlViews )

        Scatter config points ->
            ( (ScatterInternal.view meta config points) :: svgViews, htmlViews )

        Pile config barsConfigs pileMeta ->
            ( (PileInternal.view meta pileMeta config barsConfigs) :: svgViews, htmlViews )

        Axis ({ orientation } as config) ->
            ( (AxisInternal.view (getFlippedMeta orientation meta) config) :: svgViews, htmlViews )

        Grid ({ orientation } as config) ->
            ( (GridInternal.view (getFlippedMeta orientation meta) config) :: svgViews, htmlViews )

        CustomElement view ->
            ( (view meta.toSvgCoords :: svgViews), htmlViews )

        Hint config position ->
            case position of
                Just point ->
                    ( svgViews, (HintInternal.view meta config point) :: htmlViews )

                Nothing ->
                    ( svgViews, htmlViews )



-- CALCULATIONS OF META


calculateMeta : Config -> List (Element msg) -> Meta
calculateMeta ({ scaleConfig, id } as config) elements =
    let
        values =
            toValuesOriented elements

        axisConfigs =
            toAxisConfigsOriented elements

        pileMetas =
            toPileMetas elements

        pileEdges =
            PileInternal.toPileEdges pileMetas

        xScale =
            getScale scaleConfig.x values.x pileEdges.x

        yScale =
            getScale scaleConfig.y values.y pileEdges.y

        xTicks =
            getFirstGetTickValues axisConfigs.x xScale

        yTicks =
            getFirstGetTickValues axisConfigs.y yScale
    in
        { scale = Oriented xScale yScale
        , toSvgCoords = toSvgCoordsX xScale yScale
        , oppositeToSvgCoords = toSvgCoordsY xScale yScale
        , fromSvgCoords = fromSvgCoords xScale yScale
        , ticks = xTicks
        , oppositeTicks = yTicks
        , axisCrossings = getAxisCrossings axisConfigs.x yScale
        , oppositeAxisCrossings = getAxisCrossings axisConfigs.y xScale
        , toNearestX = toNearest values.x
        , getHintInfo = getHintInfo elements
        , pileMetas = pileMetas
        , id = id
        }


toValuesOriented : List (Element msg) -> Oriented (List Value)
toValuesOriented elements =
    List.foldr foldPoints [] elements
    |> List.unzip
    |> (\(x, y) -> Oriented x y)


foldPoints : Element msg -> List Point -> List Point
foldPoints element allPoints =
    case element of
        Area config points ->
            allPoints ++ points

        Line config points ->
            allPoints ++ points

        Scatter config points ->
            allPoints ++ points

        Pile config pileElements _ ->
            allPoints ++ (PileInternal.toPilePoints pileElements)

        _ ->
            allPoints


flipMeta : Meta -> Meta
flipMeta ({ scale, toSvgCoords, oppositeToSvgCoords, ticks, oppositeTicks, axisCrossings, oppositeAxisCrossings } as meta) =
    { meta
        | scale = flipOriented scale
        , toSvgCoords = oppositeToSvgCoords
        , oppositeToSvgCoords = toSvgCoords
        , axisCrossings = oppositeAxisCrossings
        , oppositeAxisCrossings = axisCrossings
        , ticks = oppositeTicks
        , oppositeTicks = ticks
    }


getFlippedMeta : Orientation -> Meta -> Meta
getFlippedMeta orientation meta =
    case orientation of
        X ->
            meta

        Y ->
            flipMeta meta


getHintInfo : List (Element msg) -> Float -> HintInfo
getHintInfo elements xValue =
    HintInfo xValue <| List.foldr (collectYValues xValue) [] elements


toAxisConfigsOriented : List (Element msg) -> Oriented (List (AxisInternal.Config msg))
toAxisConfigsOriented =
    List.foldr foldAxisConfigs { x = [], y = [] }


foldAxisConfigs : Element msg -> Oriented (List (AxisInternal.Config msg)) -> Oriented (List (AxisInternal.Config msg))
foldAxisConfigs element axisConfigs =
    case element of
        Axis ({ orientation } as config) ->
            foldOriented (\configs -> config :: configs) orientation axisConfigs

        _ ->
            axisConfigs


getFirstGetTickValues : List (AxisInternal.Config msg) -> Scale -> List Value
getFirstGetTickValues axisConfigs =
    List.head axisConfigs
        |> Maybe.withDefault AxisInternal.defaultConfigX
        |> .tickConfig
        |> TickInternal.getValues


toPileMetas : List (Element msg) -> List PileMeta
toPileMetas =
    List.foldr foldPileMeta []


foldPileMeta : Element msg -> List PileMeta -> List PileMeta
foldPileMeta element allPileMetas =
    case element of
        Pile _ _ meta ->
            meta :: allPileMetas

        _ ->
            allPileMetas


collectYValues : Float -> Element msg -> List (Maybe Value) -> List (Maybe Value)
collectYValues xValue element yValues =
    case element of
        Area config points ->
            collectYValue xValue points :: yValues

        Line config points ->
            collectYValue xValue points :: yValues

        Scatter config points ->
            collectYValue xValue points :: yValues

        Pile config barsConfigs _ ->
            (List.map (PileInternal.toPoints >> collectYValue xValue) barsConfigs) ++ yValues

        _ ->
            yValues


collectYValue : Float -> List Point -> Maybe Value
collectYValue xValue points =
    List.foldr (getYValue xValue) Nothing points


getYValue : Float -> Point -> Maybe Value -> Maybe Value
getYValue xValue ( x, y ) result =
    if x == xValue then
        Just y
    else
        result


getAxisCrossings : List (AxisInternal.Config msg) -> Scale -> List Value
getAxisCrossings axisConfigs oppositeScale =
    List.map (AxisInternal.getAxisPosition oppositeScale << .position) axisConfigs
