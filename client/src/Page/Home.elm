module Page.Home exposing (Model, Msg, view, update)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


-- MODEL

type alias Model = ()


-- UPDATE

type alias Msg = ()

update : Msg -> Model -> (Model, Cmd Msg) 
update _ model = (model, Cmd.none)


-- VIEW

view : Model -> Browser.Document Msg
view _ =
    { title = "Home"
    , body =
        [ h1 [] [ text "Home" ]
        , links
        ]
    }

links : Html Msg
links = ul []
    [ li [] [ a [ href "/" ] [ text "Home" ] ]
    , li [] [ a [ href "/profile" ] [ text "Profile" ] ]
    , li [] [ a [ href "/tab/1" ] [ text "Tab 1" ] ]
    , li [] [ a [ href "/tab/2" ] [ text "Tab 2" ] ]
    , li [] [ a [ href "/error" ] [ text "Error" ] ]
    ]
