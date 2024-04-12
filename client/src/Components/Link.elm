module Components.Link exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Components.Colour exposing (scheme)

link : String -> String -> Html msg
link txt loc = a [ href loc, css [ textDecoration none, color scheme.textColour, hover [ color scheme.primaryColour ], (pseudoClass "focus-visible") [ color scheme.primaryColour ] ] ] [ text txt ]