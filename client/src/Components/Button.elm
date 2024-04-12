module Components.Button exposing (..)

import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css, href)
import Components.Colour exposing (scheme)

buttonStyle : Style
buttonStyle = Css.batch
    [ padding2 (rem 0.5) (rem 1)
    , backgroundColor scheme.primaryColour, color scheme.textColour
    , borderStyle none, borderRadius (px 2)
    , fontSize (rem 1), textDecoration none
    , cursor pointer
    , hover [ backgroundColor scheme.primaryColourActive ], (pseudoClass "focus-visible") [ backgroundColor scheme.primaryColourActive ]
    ]

btn : List (Attribute msg) -> String -> Html msg
btn attrs txt = Html.button ( css [ buttonStyle ] :: attrs ) [ text txt ]

linkBtn : List (Attribute msg) -> String -> String -> Html msg
linkBtn attrs txt loc = a ( css [ buttonStyle ] :: href loc :: attrs ) [ text txt ]