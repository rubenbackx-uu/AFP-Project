module Components.Header exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href)
import Components.Settings exposing (contentWidth)

pageHeader : Html msg
pageHeader = header [ css [ padding2 (rem 2) (rem 2), backgroundColor (rgb 20 20 20), color (rgb 255 255 255) ] ] 
    [ div [ css [ displayFlex, justifyContent spaceBetween, alignItems center, maxWidth contentWidth, property "margin-inline" "auto" ] ]
        [ div [ css [ displayFlex, property "gap" "4rem", alignItems center ] ] 
            [ span [] [ a [ href "/", css [ fontSize (pt 16), textDecoration none, color (rgb 255 255 255) ] ] [ text "Logo" ] ]
            , ul [ css [ listStyle none, displayFlex, property "gap" "2rem", padding (px 0), margin (px 0) ] ]
                (List.map (\(t, l) -> li [] [ a [ href l, css [ textDecoration none, color (rgb 255 255 255), hover [ textDecoration underline ] ] ] [ text t ] ]) links)
            ]
        , a [ href "/profile", css [ textDecoration none, color (rgb 255 255 255), hover [ textDecoration underline ] ] ] [text "User"] 
        ]
    ]

links : List (String, String)
links = 
    [ ("Page 1", "/")
    , ("Page 2", "/")
    ]