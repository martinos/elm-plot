module Plot.Attributes
    exposing 
        ( plotSize
        , plotPadding
        , plotClasses
        , plotMargin
        , plotStyle
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
        , labelFormatIndexed
        , labelClasses
        , labelDisplace
        , labelStyle
        , labelConfigView
        , labelConfigViewFunc
        , labelCustomView
        , labelCustomViewIndexed
        , gridValues
        , gridClasses
        , gridStyle
        , gridMirrorTicks
        , tooltipCustomView
        , tooltipRemoveLine
        , areaStyle
        , lineStyle
        )


import Plot.Types
    exposing
        ( Element(..)
        , MetaConfig
        , TickViewConfig
        , TickValues
        , TickAttrFunc
        , TickView
        , LabelViewConfig
        , LabelAttrFunc
        , LabelView
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
        , State
        , Msg(..)
        , Orientation(..)
        , Point
        , Style
        )

import Plot.Default as Default

import Svg exposing (Svg)



plotPadding : ( Int, Int ) -> MetaConfig -> MetaConfig
plotPadding ( bottom, top ) config =
    { config | padding = ( toFloat bottom, toFloat top ) }


plotSize : ( Int, Int ) -> MetaConfig -> MetaConfig
plotSize ( width, height ) config =
    { config | size = ( toFloat width, toFloat height ) }


plotMargin : ( Int, Int, Int, Int ) -> MetaConfig -> MetaConfig
plotMargin ( t, r, b, l ) config =
    { config | margin = ( toFloat t, toFloat r, toFloat b, toFloat l ) }


plotStyle : Style -> MetaConfig -> MetaConfig
plotStyle style config =
    let
        metaConfig = Default.metaConfig
    in
        { config | style = metaConfig.style ++ style ++ [ ( "padding", "0" ) ] }


plotClasses : List String -> MetaConfig -> MetaConfig
plotClasses classes config =
    { config | classes = classes }




-- TICK ATTRIBUTES


tickLength : Int -> TickViewConfig -> TickViewConfig
tickLength length config =
    { config | length = length }


tickWidth : Int -> TickViewConfig -> TickViewConfig
tickWidth width config =
    { config | width = width }


tickClasses : List String -> TickViewConfig -> TickViewConfig
tickClasses classes config =
    { config | classes = classes }


tickStyle : Style -> TickViewConfig -> TickViewConfig
tickStyle style config =
    { config | style = style }



-- LABEL ATTRIBUTES


labelDisplace : ( Int, Int ) -> LabelViewConfig -> LabelViewConfig
labelDisplace displace config =
    { config | displace = Just displace }


labelFormat : (Float -> String) -> LabelViewConfig -> LabelViewConfig
labelFormat format config =
    { config | format = always format }


labelClasses : List String -> LabelViewConfig -> LabelViewConfig
labelClasses classes config =
    { config | classes = classes }


labelFormatIndexed : (Int -> Float -> String) -> LabelViewConfig -> LabelViewConfig
labelFormatIndexed format config =
    { config | format = format }


labelStyle : Style -> LabelViewConfig -> LabelViewConfig
labelStyle style config =
    { config | style = style }



-- AXIS ATTRIBUTES


axisStyle : Style -> AxisConfig msg -> AxisConfig msg
axisStyle style config =
    { config | style = style }


axisClasses : List String -> AxisConfig msg -> AxisConfig msg
axisClasses classes config =
    { config | classes = classes }


axisLineStyle : Style -> AxisConfig msg -> AxisConfig msg
axisLineStyle style config =
    { config | axisLineStyle = style }


tickValues : List Float -> AxisConfig msg -> AxisConfig msg
tickValues values config =
    { config | toTickValues = Default.toTickValuesFromList values }


tickDelta : Float -> AxisConfig msg -> AxisConfig msg
tickDelta delta config =
    { config | toTickValues = Default.toTickValuesFromDelta delta }


tickConfigView : List (TickViewConfig -> TickViewConfig) -> AxisConfig msg -> AxisConfig msg
tickConfigView tickAttrs config =
    { config | tickView = Default.toTickView tickAttrs }


tickConfigViewFunc : TickAttrFunc -> AxisConfig msg -> AxisConfig msg
tickConfigViewFunc toTickAttrs config =
    { config | tickView = Default.toTickViewDynamic toTickAttrs }


tickCustomView : (Float -> Svg msg) -> AxisConfig msg -> AxisConfig msg
tickCustomView view config =
    { config | tickView = (\_ _ -> view) }


tickCustomViewIndexed : (Int -> Float -> Svg msg) -> AxisConfig msg -> AxisConfig msg
tickCustomViewIndexed view config =
    { config | tickView = (\_ -> view) }


tickRemoveZero : AxisConfig msg -> AxisConfig msg
tickRemoveZero config =
    { config | axisCrossing = True }


labelValues : List Float -> AxisConfig msg -> AxisConfig msg
labelValues filter config =
    { config | labelValues = LabelCustomValues filter }


labelFilter : (Int -> Float -> Bool) -> AxisConfig msg -> AxisConfig msg
labelFilter filter config =
    { config | labelValues = LabelCustomFilter filter }


labelConfigView : List (LabelViewConfig -> LabelViewConfig) -> AxisConfig msg -> AxisConfig msg
labelConfigView attrs config =
    { config | labelView = Default.toLabelView attrs }


labelConfigViewFunc : LabelAttrFunc -> AxisConfig msg -> AxisConfig msg
labelConfigViewFunc toAttrs config =
    { config | labelView = Default.toLabelViewDynamic toAttrs }


labelCustomView : (Float -> Svg msg) -> AxisConfig msg -> AxisConfig msg
labelCustomView view config =
    { config | labelView = (\_ _ -> view) }


labelCustomViewIndexed : (Int -> Float -> Svg msg) -> AxisConfig msg -> AxisConfig msg
labelCustomViewIndexed view config =
    { config | labelView = (\_ -> view) }



-- GRID ATTRIBUTES


gridMirrorTicks : GridConfig -> GridConfig
gridMirrorTicks config =
    { config | values = GridMirrorTicks }


gridValues : List Float -> GridConfig -> GridConfig
gridValues values config =
    { config | values = GridCustomValues values }


gridStyle : Style -> GridConfig -> GridConfig
gridStyle style config =
    { config | style = style }


gridClasses : List String -> GridConfig -> GridConfig
gridClasses classes config =
    { config | classes = classes }


-- AREA ATTRIBUTES


areaStyle : Style -> AreaConfig -> AreaConfig
areaStyle style config =
    { config | style = style }



-- LINE ATTRIBUTES


lineStyle : Style -> LineConfig -> LineConfig
lineStyle style config =
    { config | style = ( "fill", "transparent" ) :: style }



-- TOOLTIP


tooltipRemoveLine : TooltipConfig msg -> TooltipConfig msg
tooltipRemoveLine config =
    { config | showLine = False }


tooltipCustomView : (TooltipInfo -> Bool -> Svg msg) -> TooltipConfig msg -> TooltipConfig msg
tooltipCustomView view config =
    { config | view = view }


