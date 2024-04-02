module Page.Tab exposing (Model, Msg, view, update)

import Browser
import Page
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (css, href)
import Components.Header exposing (..)
import Components.Settings exposing (..)
import Components.Colour exposing (scheme)
import Components.Link as Link exposing (..)

-- MODEL

type alias Artist =
    { id : Int
    , name : String
    }

type alias Chord =
    { position : Int
    , symbol : String
    }

type alias Line =
    { text : String
    , chords : List Chord
    }

type alias Tab = 
    { id : Int
    , title : String
    , artist : Artist
    , lines : List Line
    }

type alias Model = { tab : Tab }


-- UPDATE

type alias Msg = ()

update : Msg -> Model -> (Model, Cmd Msg) 
update _ model = (model, Cmd.none)


-- VIEW

view : Model -> Browser.Document Msg
view model = Page.view model.tab.title (content model.tab)

content : Tab -> Html Msg
content tab = div [ ] 
    [ h1 [ css [ color scheme.textColour ] ] [ text tab.title ]
    , div [ css [ fontFamily monospace, fontSize (rem 1) ] ] (List.map line tab.lines)
    ]

line : Line -> Html Msg
line l = div [ css [ minHeight (rem 2) ] ] 
    [ div [ css [ position relative, minHeight (rem 1.25) ] ] (List.map (\c -> span [ css [ position absolute, top (px 0), left (ch (toFloat c.position)), color scheme.primaryColour ] ] [ text c.symbol ] ) l.chords)
    , span [] [ text l.text ]
    ]