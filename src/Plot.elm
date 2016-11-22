module Plot
    exposing
        ( plot
        , plotSize
        , plotPadding
        , plotClasses
        , plotMargin
        , plotStyle
        , xAxis
        , yAxis
        , axisClasses
        , axisStyle
        , axisLineStyle
        , tickValues
        , tickDelta
        , tickLength
        , tickWidth
        , tickClasses
        , tickStyle
        , tickConfigView
        , tickConfigViewFunc
        , tickCustomView
        , tickCustomViewIndexed
        , tickRemoveZero
        , labelValues
        , labelFilter
        , labelFormat
        , labelClasses
        , labelDisplace
        , labelStyle
        , labelConfigView
        , labelConfigViewFunc
        , labelCustomView
        , labelCustomViewIndexed
        , verticalGrid
        , horizontalGrid
        , gridValues
        , gridClasses
        , gridStyle
        , gridMirrorTicks
        , tooltip
        , tooltipCustomView
        , tooltipRemoveLine
        , area
        , areaStyle
        , line
        , lineStyle
        , Element
        , MetaAttr
        , TickViewAttr
        , LabelViewAttr
        , AxisAttr
        , AreaAttr
        , LineAttr
        , initialState
        , update
        , Msg
        , State
        )

{-|
 This library aims to allow you to visualize a variety of graphs in
 an intuitve manner without comprimising flexibility regarding configuration.
 It is insprired by the elm-html api, using the `element attrs children` pattern.

# Elements
@docs Element, plot, line, area, xAxis, yAxis, Point, Style

# Configuration

## Meta configuration
@docs MetaAttr, plotSize, plotPadding, plotMargin, plotClasses, plotStyle

## Line configuration
@docs LineAttr, lineStyle

## Area configuration
@docs AreaAttr, areaStyle

## Axis configuration
@docs AxisAttr, axisClasses, axisStyle, axisLineStyle

### Tick values configuration
@docs tickValues, tickDelta, tickRemoveZero

### Tick view configuration
@docs TickViewAttr, tickConfigView, tickConfigViewFunc, tickLength, tickWidth, tickClasses, tickStyle, tickCustomView, tickCustomViewIndexed

### Label values configuration
@docs labelValues, labelFilter

### Label values configuration
@docs LabelViewAttr, labelConfigView, labelConfigViewFunc, labelFormat, labelDisplace, labelClasses, labelStyle, labelCustomView, labelCustomViewIndexed

## Grid configuration
@docs verticalGrid, horizontalGrid, gridMirrorTicks, gridValues, gridClasses, gridStyle

## Tooltip configuration
@docs tooltip, tooltipRemoveLine, tooltipCustomView

# State
@docs State, initialState, update, msg


-}

import Task
import Dom.Position
import Svg
import Svg.Lazy

import Plot.Attributes as Attributes
import Plot.Types as Types exposing (Msg(..), Element(..), Orientation(..))
import Plot.Parse as Parse
import Plot.Default as Default exposing (axisConfig)
import Plot.View as View
import Plot.Helpers exposing (..)
import Plot.Update as Update


toConfig : b -> List a -> b
toConfig default attrs =
    List.foldl (<|)



type Element = 
    Element Types.Element


{-| The type representing an a meta configuration.
-}
type alias MetaAttr =
    Types.MetaConfig -> Types.MetaConfig


{-| Add padding to your plot, meaning extra space below
 and above the lowest and highest point in your plot.
 The unit is pixels.

 Default: `( 0, 0 )`
-}
plotPadding : ( Int, Int ) -> MetaAttr
plotPadding =
    Attributes.plotPadding


{-| Specify the size of your plot in pixels.

 Default: `( 800, 500 )`
-}
plotSize : ( Int, Int ) -> MetaAttr
plotSize =
    Attributes.plotSize


{-| Specify margin around the plot. Useful when your ticks are outside the
 plot and you would like to add space to see them! Values are in pixels and
 in the format of ( top, right, bottom, left ).

 Default: `( 0, 0, 0, 0 )`
-}
plotMargin : ( Int, Int, Int, Int ) -> MetaAttr
plotMargin =
    Attributes.plotMargin


{-| Add styles to the svg element.

 Default: `[ ( "padding", "30px" ), ( "stroke", "#000" ) ]`
-}
plotStyle : Types.Style -> MetaAttr
plotStyle style config =
    Attributes.plotStyle


{-| Add classes to the svg element.

 Default: `[]`
-}
plotClasses : List String -> MetaAttr
plotClasses classes config =
    Attributes.plotClasses


{-| This is the function processing your entire plot configuration.
 Pass your meta attributes and plot elements to this function and
 a svg plot will be returned!
-}
plot : String -> List MetaAttr -> List (Element msg) -> Svg.Svg msg
plot id attr elements =
    Svg.Lazy.lazy3 parsePlot id attr elements


parsePlot : String -> List MetaAttr -> List (Element msg) -> Svg.Svg msg
parsePlot id attrs elements =
    let
        metaConfig =
            List.foldr (<|) Default.metaConfig attrs

        plotProps =
            Parse.getPlotProps id metaConfig elements
    in
        View.plot metaConfig plotProps (View.elements plotProps elements)



{-| Type representing a tick view configuration attribute.
-}
type alias TickViewAttr =
    Types.TickViewConfig -> Types.TickViewConfig


{-| Set the length of the tick.

    main =
        plot
            []
            [ xAxis
                [ tickConfigView [ tickLength 10 ] ]
            ]
-}
tickLength : Int -> TickViewAttr
tickLength =
    Attributes.tickLength


{-| Set the width of the tick.

    main =
        plot
            []
            [ xAxis
                [ tickConfigView [ tickWidth 2 ] ]
            ]
-}
tickWidth : Int -> TickViewAttr
tickWidth =
    Attributes.tickWidth


{-| Add classes to the tick.

    main =
        plot
            []
            [ xAxis
                [ tickConfigView
                    [ tickClasses [ "my-class" ] ]
                ]
            ]
-}
tickClasses : List String -> TickViewAttr
tickClasses =
    Attributes.tickClasses


{-| Sets the style of the tick

    main =
        plot
            []
            [ xAxis
                [ tickConfigView
                    [ tickStyle [ ( "stroke", "blue" ) ] ]
                ]
            ]
-}
tickStyle : Types.Style -> TickViewAttr
tickStyle =
    Attributes.tickStyle


{-| Type representing a label view configuration attribute.
-}
type alias LabelViewAttr =
    Types.LabelViewConfig -> Types.LabelViewConfig


{-| Move the position of the label.

    main =
        plot
            []
            [ xAxis
                [ labelConfigView [ labelDisplace ( 0, 27 ) ] ]
            ]
-}
labelDisplace : ( Int, Int ) -> LabelViewAttr
labelDisplace =
    Attributes.labelDisplace


{-| Format the label based on its value.

    main =
        plot
            []
            [ xAxis
                [ labelConfigView
                    [ labelFormat (\l -> toString l ++ " DKK") ]
                ]
            ]
-}
labelFormat : (Float -> String) -> LabelViewAttr
labelFormat =
    Attributes.labelFormat


{-| Add classes to the label.

    main =
        plot
            []
            [ xAxis
                [ labelConfigView
                    [ labelClasses [ "my-class" ] ]
                ]
            ]
-}
labelClasses : List String -> LabelViewAttr
labelClasses =
    Attributes.labelClasses


{-| Format the label based on its value and/or index.

    formatter : Int -> Float -> String
    formatter index value =
        if isOdd index then
            toString l ++ " DKK"
        else
            ""

    main =
        plot
            []
            [ xAxis
                [ labelConfigView [ labelFormat formatter ] ]
            ]
-}
labelFormatIndexed : (Int -> Float -> String) -> LabelViewAttr
labelFormatIndexed =
    Attributes.labelFormatIndexed


{-| Move the position of the label.

    main =
        plot
            []
            [ xAxis
                [ labelConfigView
                    [ labelStyle [ ("stroke", "blue" ) ] ]
                ]
            ]
-}
labelStyle : Types.Style -> LabelViewAttr
labelStyle =
    Attributes.labelStyle


{-| The type representing an axis configuration.
-}
type alias AxisAttr msg =
    Types.AxisConfig msg -> Types.AxisConfig msg


{-| Add style to the container holding your axis. Most properties are
 conveniently inherited by your ticks and labels.

    main =
        plot
            []
            [ xAxis [ axisStyle [ ( "stroke", "red" ) ] ] ]

 Default: `[]`
-}
axisStyle : Types.Style -> AxisAttr msg
axisStyle =
    Attributes.axisStyle


{-| Add classes to the container holding your axis.

    main =
        plot
            []
            [ xAxis [ axisClasses [ "my-class" ] ] ]

 Default: `[]`
-}
axisClasses : List String -> AxisAttr msg
axisClasses =
    Attributes.axisClasses


{-| Add styling to the axis line.

    main =
        plot
            []
            [ xAxis [ axisLineStyle [ ( "stroke", "blue" ) ] ] ]

 Default: `[]`
-}
axisLineStyle : Types.Style -> AxisAttr msg
axisLineStyle =
    Attributes.axisLineStyle


{-| Defines what ticks will be shown on the axis by specifying a list of values.

    main =
        plot
            []
            [ xAxis [ tickValues [ 0, 1, 2, 4, 8 ] ] ]

 **Note:** If in the list of axis attributes, this attribute is followed by a
 `tickDelta` attribute, then this attribute will have no effect.
-}
tickValues : List Float -> AxisAttr msg
tickValues =
    Attributes.tickValues


{-| Defines what ticks will be shown on the axis by specifying the delta between the ticks.
 The delta will be added from zero.

    main =
        plot
            []
            [ xAxis [ tickDelta 4 ] ]

 **Note:** If in the list of axis attributes, this attribute is followed by a
 `tickValues` attribute, then this attribute will have no effect.
-}
tickDelta : Float -> AxisAttr msg
tickDelta =
    Attributes.tickDelta


{-| Defines how the tick will be displayed by specifying a list of tick view attributes.

    main =
        plot
            []
            [ xAxis
                [ tickConfigView
                    [ tickLength 10
                    , tickWidth 2
                    , tickStyle [ ( "stroke", "red" ) ]
                    ]
                ]
            ]

 If you do not define another view configuration,
 the default will be `[ tickLength 7, tickWidth 1, tickStyle [] ]`

 **Note:** If in the list of axis attributes, this attribute is followed by a
 `tickCustomView`, `tickConfigViewFunc` or a `tickCustomViewIndexed` attribute,
 then this attribute will have no effect.
-}
tickConfigView : List TickViewAttr -> AxisAttr msg
tickConfigView =
    Attributes.tickConfigView


{-| Defines how the tick will be displayed by specifying a list of tick view attributes.

    toTickConfig : Int -> Float -> List TickViewAttr
    toTickConfig index tick =
        if isOdd index then
            [ tickLength 7
            , tickStyle [ ( "stroke", "#e4e3e3" ) ]
            ]
        else
            [ tickLength 10
            , tickStyle [ ( "stroke", "#b9b9b9" ) ]
            ]

    main =
        plot
            []
            [ xAxis
                [ tickConfigViewFunc toTickConfig ]
            ]

 **Note:** If in the list of axis attributes, this attribute is followed by a
 `tickConfigView`, `tickCustomView` or a `tickCustomViewIndexed` attribute,
 then this attribute will have no effect.
-}
tickConfigViewFunc : Types.TickAttrFunc -> AxisAttr msg
tickConfigViewFunc =
    Attributes.tickConfigViewFunc


{-| Defines how the tick will be displayed by specifying a function which returns your tick html.

    viewTick : Float -> Svg.Svg a
    viewTick tick =
        text_
            [ transform ("translate(-5, 10)") ]
            [ tspan [] [ text "✨" ] ]

    main =
        plot [] [ xAxis [ tickCustomView viewTick ] ]

 **Note:** If in the list of axis attributes, this attribute is followed by a
 `tickConfigView` or a `tickCustomViewIndexed` attribute, then this attribute will have no effect.
-}
tickCustomView : (Float -> Svg.Svg msg) -> AxisAttr msg
tickCustomView =
    Attributes.tickCustomView


{-| Same as `tickCustomConfig`, but the functions is also passed a value
 which is how many ticks away the current tick is from the zero tick.

    viewTick : Int -> Float -> Svg.Svg a
    viewTick index tick =
        text_
            [ transform ("translate(-5, 10)") ]
            [ tspan
                []
                [ text (if isOdd index then "🌟" else "⭐") ]
            ]

    main =
        plot [] [ xAxis [ tickCustomViewIndexed viewTick ] ]

 **Note:** If in the list of axis attributes, this attribute is followed by a
 `tickConfigView` or a `tickCustomView` attribute, then this attribute will have no effect.
-}
tickCustomViewIndexed : (Int -> Float -> Svg.Svg msg) -> AxisAttr msg
tickCustomViewIndexed =
    Attributes.tickCustomViewIndexed


{-| Remove tick at origin. Useful when two axis' are crossing and you do not
 want the origin the be cluttered with labels.

    main =
        plot
            []
            [ xAxis [ tickRemoveZero ] ]
-}
tickRemoveZero : AxisAttr msg
tickRemoveZero =
    Attributes.tickRemoveZero


{-| Add a list of values where labels will be added.

    main =
        plot
            []
            [ xAxis [ labelValues [ 20, 40, 60 ] ] ]
-}
labelValues : List Float -> AxisAttr msg
labelValues =
    Attributes.labelValues


{-| Add a filter determining which of the ticks are added a label. The first argument passed
 to the filter is a number describing how many ticks a way the current tick is. The second argument
 is the value of the tick.

    onlyEvenTicks : Int -> Float -> Bool
    onlyEvenTicks index value =
        rem 2 index == 0

    main =
        plot
            []
            [ xAxis [ labelValues onlyEvenTicks ] ]

 Default: `(\a b -> True)`

 **Note:** If in the list of axis attributes, this attribute is followed by a
 `labelValues` attribute, then this attribute will have no effect.
-}
labelFilter : (Int -> Float -> Bool) -> AxisAttr msg
labelFilter =
    Attributes.labelFilter


{-| Configure the label view specifying a list of label view attributes.

    main =
        plot
            []
            [ xAxis
                [ labelConfigView
                    [ labelFormat (\t -> toString t ++ " s") ]
                ]
            ]
-}
labelConfigView : List LabelViewAttr -> AxisAttr msg
labelConfigView =
    Attributes.labelConfigView


{-| Configure the label view specifying a function returning a list of label view attributes.
 The function will be passed:
 1) An integer representing the amount of ticks away from the origin, the current tick is.
 2) A float value represeting the value of the tick.

    toLabelConfig : Int -> Float -> List TickViewAttr
    toLabelConfig index tick =
        if isOdd index then
            [ labelFormat (\t -> toString t ++ " s") ]
        else
            [ labelFormat (always "") ]

    main =
        plot
            []
            [ xAxis
                [ labelConfigViewFunc toLabelConfig ]
            ]
-}
labelConfigViewFunc : Types.LabelAttrFunc -> AxisAttr msg
labelConfigViewFunc =
    Attributes.labelConfigViewFunc


{-| Add a custom view for rendering your label.

    viewLabel : Float -> Svg.Svg a
    viewLabel tick =
        text_ mySpecialAttributes mySpecialLabelDisplay

    main =
        plot
            []
            [ xAxis [ labelCustomView viewLabel ] ]

 **Note:** If in the list of axis attributes, this attribute is followed by a
 `labelFormat` attribute, then this attribute will have no effect.
-}
labelCustomView : (Float -> Svg.Svg msg) -> AxisAttr msg
labelCustomView =
    Attributes.labelCustomView


{-| Same as `labelCustomView`, except this view is also passed the value being
 the amount of ticks the current tick is away from zero.

    viewLabel : Int -> Float -> Svg.Svg a
    viewLabel fromZero tick =
        let
            attrs =
                if isOdd fromZero then oddAttrs
                else evenAttrs
        in
            text_ attrs labelHtml

    main =
        plot
            []
            [ xAxis [ labelCustomViewIndexed viewLabel ] ]

 **Note:** If in the list of axis attributes, this attribute is followed by a
 `labelFormat` attribute, then this attribute will have no effect.
-}
labelCustomViewIndexed : (Int -> Float -> Svg.Svg msg) -> AxisAttr msg
labelCustomViewIndexed =
    Attributes.labelCustomViewIndexed


{-| This returns an axis element resulting in an x-axis being rendered in your plot.

    main =
        plot [] [ xAxis [] ]
-}
xAxis : List (AxisAttr msg) -> Element msg
xAxis attrs =
    Axis (toConfig axisConfig attrs)


{-| This returns an axis element resulting in an y-axis being rendered in your plot.

    main =
        plot [] [ yAxis [] ]
-}
yAxis : List (AxisAttr msg) -> Element msg
yAxis attrs =
    Axis (toConfig { axisConfig | orientation = Y } attrs)



{-| The type representing an grid configuration.
-}
type alias GridAttr =
    Types.GridConfig -> Types.GridConfig


{-| Adds grid lines where the ticks on the corresponding axis are.

    main =
        plot
            []
            [ verticalGrid [ gridMirrorTicks ]
            , xAxis []
            ]

 **Note:** If in the list of axis attributes, this attribute is followed by a
 `gridValues` attribute, then this attribute will have no effect.
-}
gridMirrorTicks : GridAttr
gridMirrorTicks =
    Attributes.gridMirrorTicks


{-| Specify a list of ticks where you want grid lines drawn.

    plot [] [ verticalGrid [ gridValues [ 1, 2, 4, 8 ] ] ]

 **Note:** If in the list of axis attributes, this attribute is followed by a
 `gridMirrorTicks` attribute, then this attribute will have no effect.
-}
gridValues : List Float -> GridAttr
gridValues =
    Attributes.gridValues


{-| Specify styles for the gridlines.

    plot
        []
        [ verticalGrid
            [ gridMirrorTicks
            , gridStyle myGridStyles
            ]
        ]

 Remember that if you do not specify either `gridMirrorTicks`
 or `gridValues`, then we will default to not showing any grid lines.
-}
gridStyle : Types.Style -> GridAttr
gridStyle =
    Attributes.gridStyle


{-| Specify classes for the grid.

    plot
        []
        [ verticalGrid
            [ gridMirrorTicks
            , gridClasses [ "my-class" ]
            ]
        ]

 Remember that if you do not specify either `gridMirrorTicks`
 or `gridValues`, then we will default to not showing any grid lines.
-}
gridClasses : List String -> GridAttr
gridClasses =
    Attributes.gridClasses


{-| This returns an grid element resulting in vertical grid lines being rendered in your plot.

    main =
        plot [] [ horizontalGrid [] ]
-}
horizontalGrid : List GridAttr -> Element msg
horizontalGrid attrs =
    Grid (toConfig Default.gridConfig attrs)


{-| This returns an axis element resulting in horizontal grid lines being rendered in your plot.

    main =
        plot [] [ verticalGrid [] ]
-}
verticalGrid : List GridAttr -> Element msg
verticalGrid attrs =
    let
        default = Default.gridConfig
    in
        Grid (toConfig { default | orientation = Y } attrs)



type alias AreaAttr =
    Types.AreaConfig -> Types.AreaConfig


{-| Add styles to your area serie.

    main =
        plot
            []
            [ area
                [ areaStyle
                    [ ( "fill", "deeppink" )
                    , ( "stroke", "deeppink" )
                    , ( "opacity", "0.5" ) ]
                    ]
                ]
                areaDataPoints
            ]
-}
areaStyle : Types.Style -> AreaAttr
areaStyle =
    Attributes.areaStyle


{-| This returns an area element resulting in an area serie rendered in your plot.

    main =
        plot [] [ area []  [ ( 0, -2 ), ( 2, 0 ), ( 3, 1 ) ] ]
-}
area : List AreaAttr -> List Types.Point -> Element msg
area attrs points =
    let
        config =
            toConfig Default.areaConfig attrs
    in
        Area { config | points = points }


{-| The type representing a line configuration.
-}
type alias LineAttr =
    Types.LineConfig -> Types.LineConfig


{-| Add styles to your line serie.

    main =
        plot
            []
            [ line
                [ lineStyle [ ( "fill", "deeppink" ) ] ]
                lineDataPoints
            ]
-}
lineStyle : Types.Style -> LineAttr
lineStyle =
    Attributes.lineStyle


{-| This returns a line element resulting in an line serie rendered in your plot.

    main =
        plot [] [ line [] [ ( 0, 1 ), ( 2, 2 ), ( 3, 4 ) ] ]
-}
line : List LineAttr -> List Types.Point -> Element msg
line attrs points =
    let
        config =
            toConfig Default.lineConfig attrs
    in
        Line { config | points = points }


{-| The type representing a tooltip configuration.
-}
type alias TooltipAttr msg =
    Types.TooltipConfig msg -> Types.TooltipConfig msg


{-| -}
tooltipRemoveLine : TooltipAttr msg
tooltipRemoveLine =
    Attributes.tooltipRemoveLine


{-| -}
tooltipCustomView : (Types.TooltipInfo -> Bool -> Svg.Svg msg) -> TooltipAttr msg
tooltipCustomView =
    Attributes.tooltipCustomView


{-| -}
tooltip : List (TooltipAttr msg) -> ( Float, Float ) -> Element msg
tooltip attrs position =
    Tooltip (toConfig Default.tooltipConfig attrs) position


{-| -}
type State
    = State Types.State


{-| -}
initialState : State
initialState =
    State
    { position = Nothing
    , waiting = True
    }



-- UPDATE


{-| -}
type Msg
    = Msg Types.Msg


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update (Msg msg) (State state) =
    let
        ( newState, maybeMsg ) =
            Update.update msg state
    in
        ( State newState, maybeMsg )



