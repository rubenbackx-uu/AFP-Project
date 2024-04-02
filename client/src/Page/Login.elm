module Page.Login exposing (Model, Msg, view, update)

import Browser
import Page
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css, placeholder, type_, name, method, action)
import Components.Header exposing (..)
import Components.Settings exposing (..)
import Components.Colour exposing (scheme)
import Components.Button as Button exposing (..)

-- MODEL

type alias Model = ()


-- UPDATE

type alias Msg = ()

update : Msg -> Model -> (Model, Cmd Msg) 
update _ model = (model, Cmd.none)


-- VIEW

view : Model -> Browser.Document Msg
view _ = Page.view "Log in" content

content : Html Msg
content = div [ ] 
    [ h1 [ css [ color scheme.textColour ] ] [ text "Log in" ]
    , div [] 
        [ form [ ]
            [ div [ css [ property "display" "grid", property "grid-template-columns" "6rem 16rem", property "gap" ".25rem", alignItems center, marginBottom (rem 1) ] ] 
                [ label [] [ text "Username" ]
                , input [ css [ fontSize (rem 1), padding2 (rem 0.25) (rem 0.5) ], placeholder "Username...", type_ "username", name "username" ] []
                , label [] [ text "Password" ]
                , input [ css [ fontSize (rem 1), padding2 (rem 0.25) (rem 0.5) ], placeholder "Password...", type_ "password", name "username" ] []
                ]
            , btn [] "Log in"
            ]
        ]
    ]