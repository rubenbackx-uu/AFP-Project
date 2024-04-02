module Components.Header exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href)
import Components.Settings exposing (contentWidth)
import Components.Colour exposing (scheme)

pageHeader : Html msg
pageHeader = header [ css [ padding2 (rem 2) (rem 2), backgroundColor scheme.headerColour, color scheme.textColour ] ] 
    [ div [ css [ displayFlex, justifyContent spaceBetween, alignItems center, maxWidth contentWidth, property "margin-inline" "auto" ] ]
        [ div [ css [ displayFlex, property "gap" "4rem", alignItems center ] ] 
            [ span [] [ a [ href "/", css [ fontSize (pt 16), textDecoration none, color scheme.textColour ] ] [ text "Home" ] ]
            , ul [ css [ listStyle none, displayFlex, property "gap" "2rem", padding (px 0), margin (px 0) ] ]
                (List.map (\(t, l) -> li [] [ a [ href l, css [ textDecoration none, color scheme.textColour, hover [ textDecoration underline, color scheme.primaryColour ], (pseudoClass "focus-visible") [ textDecoration underline, color scheme.primaryColour ] ] ] [ text t ] ]) links)
            ]
        , a [ href "/profile", css [ textDecoration none, color scheme.textColour, hover [ textDecoration underline, color scheme.primaryColour ], (pseudoClass "focus-visible") [ textDecoration underline, color scheme.primaryColour ] ] ] [text "User"] 
        ]
    ]

links : List (String, String)
links = 
    [ ("Tabs", "/tabs")
    , ("Arists", "/artists")
    , ("Log in", "/login")
    ]