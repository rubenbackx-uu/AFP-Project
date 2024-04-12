module Page.Home exposing (Model, Msg, view, update, init)

import Browser
import Page
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css, href)
import Components.Header exposing (..)
import Components.Settings exposing (..)
import Components.Colour exposing (scheme)
import Components.Link as Link exposing (..)
import Components.Button exposing (..)
import Session exposing (..)
import Json.Decode exposing (map2, map4, Decoder, field, string, int, list)
import Http

-- MODEL

type alias Artist =
    { id : Int
    , name : String
    }

type alias Tab =
    { id : Int
    , title : String
    , artist : Artist
    , rating : Int
    }

type alias Model = { session : Session, tabs : List Tab }


-- INIT

init : Session -> (Model, Cmd Msg)
init session = ( { session = session, tabs = [] }, fetchTabs session)

-- UPDATE

type Msg = Fetched (Result Http.Error (List Tab))

update : Msg -> Model -> (Model, Cmd Msg) 
update msg model = case msg of 
    Fetched res -> case res of
        Ok tabs -> ( { model | tabs = tabs }, Cmd.none)
        Err _ -> (model, Cmd.none)


-- VIEW

view : Model -> Browser.Document Msg
view model = Page.view "Home" model.session (content model.tabs)

content : List Tab -> Html Msg
content tabs = div [ ] 
    [ h1 [ css [ color scheme.textColour ] ] [ text "Home" ]
    , popularTabs tabs
    ]


tableCell : Style
tableCell = Css.batch 
    [ borderBottom3 (px 1) solid scheme.borderColour
    , padding2 (rem 0.1) (rem 0.25)
    ]

yourTabs : Html Msg
yourTabs = div [ css [ displayFlex, flexDirection column, marginBottom (rem 1) ] ] 
    [ div [ css [ marginBottom (rem 0.5), displayFlex, alignItems center, justifyContent spaceBetween ] ] 
        [ h2 [ css [ margin (px 0), fontWeight normal ] ] [ text "Your tabs" ]
        , linkBtn [] "Create a new tab" "create-tab"
        ]
    , tabsTable
        [ { id = 1, title = "Tab 1", artist = { id = 1, name = "Arist 1" }, rating = 3 }
        , { id = 2, title = "Tab 2", artist = { id = 2, name = "Arist 2" }, rating = 4 }
        ]
    ]

popularTabs : List Tab -> Html Msg
popularTabs tabs = div [ css [ displayFlex, flexDirection column ] ] 
    [ h2 [ css [ marginBottom (rem 0.5), fontWeight normal ] ] [ text "Popular tabs" ]
    , tabsTable tabs
    ]

tabsTable : List Tab -> Html Msg
tabsTable tabs = Html.table [  css [ borderCollapse collapse ] ]
        (tr [] 
            [ th [ css [ textAlign start, tableCell ] ] [ text "Title" ]
            , th [ css [ textAlign start, tableCell ] ] [ text "Artist" ]
            , th [ css [ textAlign start, tableCell ] ] [ text "Rating" ]
            ]
        :: List.map tabRow tabs)

tabRow : Tab -> Html Msg
tabRow tab = tr []
    [ td [ css [ tableCell ] ] [ Link.link tab.title ("/tab/" ++ (String.fromInt tab.id)) ]
    , td [ css [ tableCell ] ] [ Link.link tab.artist.name ("/artist/" ++ (String.fromInt tab.artist.id)) ]
    , td [ css [ tableCell ] ] 
        (List.repeat tab.rating (span [ css [color scheme.primaryColour ] ] [ text "★" ]) ++
            List.repeat (5 - tab.rating) (span [ css [color scheme.textColour] ] [ text "☆" ]))
    ]

fetchTabs : Session -> Cmd Msg
fetchTabs session = Http.request 
    { method = "GET"
    , url = "http://localhost:8300/tabs"
    , headers = []
    , expect = Http.expectJson Fetched (list tabDecoder)
    , body = Http.emptyBody
    , timeout = Nothing
    , tracker = Nothing
    }

tabDecoder : Decoder Tab
tabDecoder = map4 Tab (field "id" Json.Decode.int) (field "title" string) (field "artist" artistDecoder) (field "rating" Json.Decode.int)

artistDecoder : Decoder Artist
artistDecoder = map2 Artist (field "id" Json.Decode.int) (field "name" string)