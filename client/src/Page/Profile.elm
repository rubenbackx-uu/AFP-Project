module Page.Profile exposing (Model, Msg, view, update)

import Browser
import Page
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Components.Header exposing (..)
import Components.Settings exposing (..)
import Session exposing (..)

-- MODEL

type alias Model = { session : Session }


-- UPDATE

type alias Msg = ()

update : Msg -> Model -> (Model, Cmd Msg) 
update _ model = (model, Cmd.none)


-- VIEW

view : Model -> Browser.Document Msg
view model = Page.view "Home" model.session content

content : Html Msg
content = div [] 
    [ h1 [ css [ color (rgb 255 255 255) ] ] [ text "This is the profile page" ]
    , p [ css [ color (rgb 255 255 255) ] ] [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam sit amet ipsum quam. Mauris commodo purus quis maximus viverra. Pellentesque nec velit vel tellus fringilla efficitur. Fusce dignissim metus et volutpat placerat. In mattis dolor id felis feugiat, a rutrum velit auctor. Duis pretium cursus lectus dapibus pulvinar. Aenean mollis ligula vitae neque gravida, eget molestie metus ullamcorper. Nunc ultrices molestie sem, in commodo diam egestas et. In fringilla odio nulla, sit amet lobortis magna eleifend nec. Sed laoreet tristique purus ut maximus. Cras at cursus mi, vitae consequat lorem. Mauris risus lectus, laoreet ut maximus vel, sollicitudin at tellus. Nunc quis magna id massa rutrum tincidunt. In feugiat ornare fermentum. Lorem ipsum dolor sit amet, consectetur adipiscing elit. " ]
    ]
