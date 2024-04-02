module Page.Error exposing (Model, Msg, view, update)

import Browser
import Page
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Components.Header exposing (..)
import Components.Settings exposing (..)
import Components.Colour exposing (scheme)
import Session exposing (..)

-- MODEL

type alias Model = { session : Session }


-- UPDATE

type alias Msg = ()

update : Msg -> Model -> (Model, Cmd Msg) 
update _ model = (model, Cmd.none)


-- VIEW

view : Model -> Browser.Document Msg
view model = Page.view "Error" model.session content

content : Html Msg
content = div [ ] 
    [ h1 [ css [ color scheme.textColour ] ] [ text "Error" ]
    , p [ css [ color scheme.textColour ] ] [ text "An error occured." ]
    ]
