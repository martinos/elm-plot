module Common exposing (..)

import Plot exposing (..)

scaleX : List ScaleAttribute
scaleX =
    [ length 380
    , marginUpper 20
    , marginLower 20
    , rangeLower (min 0)
    ]


scaleY : List ScaleAttribute
scaleY =
    [ length 300
    , marginUpper 40
    , marginLower 10
    , rangeLower (min 0)
    ]


axisColor : String
axisColor =
    "#949494"


axisColorLight : String
axisColorLight =
    "#e4e4e4"


blueFill : String
blueFill =
    "#e4eeff"


blueStroke : String
blueStroke =
    "#cfd8ea"


skinFill : String
skinFill =
    "#feefe5"


skinStroke : String
skinStroke =
    "#f7e0d2"


pinkFill : String
pinkFill =
    "#fdb9e7"


pinkStroke : String
pinkStroke =
    "#ff9edf"
