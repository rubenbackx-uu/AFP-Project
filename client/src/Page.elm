module Page exposing (view)

import Browser
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Components.Header exposing (..)
import Components.Settings exposing(..)
import Components.Colour exposing(scheme)
import Session exposing (..)

view : String -> Session -> Html msg -> Browser.Document msg
view title session content =
    { title = title
    , body = [ toUnstyled (page session content) ]
    }

page : Session -> Html msg -> Html msg
page session content = div [ css [ fontFamilies [ "sans-serif" ], displayFlex, flexDirection column, minHeight (vh 100), lineHeight (num 1.5) ] ]
    [ pageHeader session
    , pageBody content 
    ]

pageBody : Html msg -> Html msg
pageBody content = div [ css [ padding2 (rem 1) (rem 2), flexGrow (num 1), backgroundColor scheme.backgroundColour, color scheme.textColour ] ]
    [ div [ css [ maxWidth contentWidth, property "margin-inline" "auto" ] ] [content]
    ]