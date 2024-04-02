module Page.Tab exposing (Model, Msg, view, update, init, toSession)

import Browser
import Page
import Css exposing (..)
import Http
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (css, href)
import Components.Header exposing (..)
import Components.Settings exposing (..)
import Components.Colour exposing (scheme)
import Components.Link as Link exposing (..)
import Json.Decode exposing (..)
import Session exposing (Session)

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

type Model = Loading Session | Failure Session | TabLoaded Tab Session

toSession : Model -> Session
toSession model = case model of
    Loading session -> session
    Failure session -> session
    TabLoaded _ session -> session


-- INIT

init : Int -> Session -> (Model, Cmd Msg)
init id session = (Loading session, fetchTab id session)


-- UPDATE

type Msg = Fetch Int | Fetched (Result Http.Error Tab)

update : Msg -> Model -> (Model, Cmd Msg) 
update msg model = case msg of 
    Fetch id -> (model, fetchTab id (toSession model))
    Fetched result -> case result of
        Ok tab -> (TabLoaded tab (toSession model), Cmd.none)
        Err _ -> (Failure (toSession model), Cmd.none)

-- VIEW

view : Model -> Browser.Document Msg
view model = case model of 
    TabLoaded tab session -> Page.view tab.title session (content tab)
    Loading session -> Page.view "Loading" session (div [] [ text "Loading" ])
    Failure session -> Page.view "Error" session (div [] [ text "Error" ])

content : Tab -> Html Msg
content tab = div [ ] 
    [ h1 [ css [ color scheme.textColour ] ] [ text (tab.title ++ " - " ++ tab.artist.name) ]
    , div [ css [ fontFamily monospace, fontSize (rem 1) ] ] (List.map line tab.lines)
    ]

line : Line -> Html Msg
line l = div [ css [ minHeight (rem 2) ] ] 
    [ div [ css [ position relative, minHeight (rem 1.25) ] ] (List.map (\c -> span [ css [ position absolute, top (px 0), left (ch (toFloat c.position)), color scheme.primaryColour ] ] [ text c.symbol ] ) l.chords)
    , span [] [ text l.text ]
    ]


fetchTab : Int -> Session -> Cmd Msg
fetchTab id session = Http.request 
    { method = "GET"
    , url = "http://localhost:8300/tab/" ++ (String.fromInt id)
    , headers = []
    , expect = Http.expectJson Fetched tabDecoder 
    , body = Http.emptyBody
    , timeout = Nothing
    , tracker = Nothing
    }

tabDecoder : Decoder Tab
tabDecoder = map4 Tab (field "id" Json.Decode.int) (field "title" string) (field "artist" artistDecoder) (field "content" contentDecoder)

artistDecoder : Decoder Artist
artistDecoder = map2 Artist (field "id" Json.Decode.int) (field "name" string)

contentDecoder : Decoder (List Line)
contentDecoder = field "lines" (list lineDecoder)

lineDecoder : Decoder Line
lineDecoder = map2 Line (field "text" string) (field "chords" (list chordDecoder))

chordDecoder : Decoder Chord
chordDecoder = map2 Chord (field "position" Json.Decode.int) (field "symbol" string)
