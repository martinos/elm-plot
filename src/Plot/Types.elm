module Plot.Types
    exposing
        ( Element(..)
        , MetaConfig
        , TickViewConfig
        , TickAttrFunc
        , TickValues
        , TickView
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
        , State
        , Msg(..)
        , Orientation(..)
        , Point
        , Style
        )

import Svg exposing (Svg)
import Html exposing (Html)
import Dom exposing (Error)


type alias Point =
    ( Float, Float )


type alias Style =
    List ( String, String )


type Orientation
    = X
    | Y


type Element msg
    = Axis (AxisConfig msg)
    | Tooltip (TooltipConfig msg) ( Float, Float )
    | Grid GridConfig
    | Line LineConfig
    | Area AreaConfig


type alias MetaConfig =
    { size : ( Float, Float )
    , padding : ( Float, Float )
    , margin : ( Float, Float, Float, Float )
    , classes : List String
    , style : Style
    , id : String
    }



-- TICK TYPES


type alias TickViewConfig =
    { length : Int
    , width : Int
    , style : Style
    , classes : List String
    }


type alias TickView msg =
    Orientation -> Int -> Float -> Svg msg


type alias TickValues =
    AxisScale -> List Float


type alias TickAttrFunc =
    Int -> Float -> List (TickViewConfig -> TickViewConfig)



-- LABEL TYPES


type alias LabelViewConfig =
    { displace : Maybe ( Int, Int )
    , format : Int -> Float -> String
    , style : Style
    , classes : List String
    }


type alias LabelView msg =
    Orientation -> Int -> Float -> Svg msg


type LabelValues
    = LabelCustomValues (List Float)
    | LabelCustomFilter (Int -> Float -> Bool)


type alias LabelAttrFunc =
    Int -> Float -> List (LabelViewConfig -> LabelViewConfig)



-- AXIS TYPES


type alias AxisConfig msg =
    { toTickValues : TickValues
    , tickView : TickView msg
    , labelValues : LabelValues
    , labelView : LabelView msg
    , axisLineStyle : Style
    , axisCrossing : Bool
    , style : Style
    , classes : List String
    , orientation : Orientation
    }



-- GRID TYPES


type GridValues
    = GridMirrorTicks
    | GridCustomValues (List Float)


type alias GridConfig =
    { values : GridValues
    , style : Style
    , classes : List String
    , orientation : Orientation
    }


type alias GridAttr =
    GridConfig -> GridConfig



-- AREA TYPES


type alias AreaConfig =
    { style : Style
    , points : List Point
    }



-- LINE TYPES


type alias LineConfig =
    { style : Style
    , points : List Point
    }



-- TOOLTIP


type alias TooltipConfig msg =
    { view : TooltipInfo -> Bool -> Html msg
    , showLine : Bool
    , lineStyle : Style
    }


type alias TooltipInfo =
    { xValue : Float
    , yValues : List (Maybe Float)
    }



-- MODEL


type alias State =
    { position : Maybe ( Float, Float )
    , waiting : Bool
    }


type Msg
    = Hovering PlotProps ( Float, Float )
    | ReceivePosition (Result Dom.Error ( Float, Float ))
    | ResetPosition


-- CALCULATE SCALES


type alias AxisScale =
    { range : Float
    , lowest : Float
    , highest : Float
    , length : Float
    , offset : Float
    }


type alias PlotProps =
    { scale : AxisScale
    , oppositeScale : AxisScale
    , toSvgCoords : Point -> Point
    , oppositeToSvgCoords : Point -> Point
    , fromSvgCoords : Point -> Point
    , ticks : List Float
    , oppositeTicks : List Float
    , getTooltipInfo : Float -> TooltipInfo
    , toNearestX : Float -> Float
    , id : String
    }


