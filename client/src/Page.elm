module Page exposing (view)

import Browser
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Components.Header exposing (..)
import Components.Settings exposing(..)
import Components.Colour exposing(scheme)

view : String -> Html msg -> Browser.Document msg
view title content =
    { title = title
    , body = [ toUnstyled (page content) ]
    }

page : Html msg -> Html msg
page content = div [ css [ fontFamilies [ "sans-serif" ], displayFlex, flexDirection column, minHeight (vh 100), lineHeight (num 1.5) ] ]
    [ pageHeader
    , pageBody content 
    ]

pageBody : Html msg -> Html msg
pageBody content = div [ css [ padding2 (rem 1) (rem 2), flexGrow (num 1), backgroundColor scheme.backgroundColour, color scheme.textColour ] ]
    [ div [ css [ maxWidth contentWidth, property "margin-inline" "auto" ] ] [content]
    ]