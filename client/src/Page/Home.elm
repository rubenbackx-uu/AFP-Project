module Page.Home exposing (Model, Msg, view, update)

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

-- MODEL

type alias Model = ()


-- UPDATE

type alias Msg = ()

update : Msg -> Model -> (Model, Cmd Msg) 
update _ model = (model, Cmd.none)


-- VIEW

view : Model -> Browser.Document Msg
view _ = Page.view "Home" content

content : Html Msg
content = div [ ] 
    [ h1 [ css [ color scheme.textColour ] ] [ text "Home" ]
    , yourTabs
    , popularTabs
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

popularTabs : Html Msg
popularTabs = div [ css [ displayFlex, flexDirection column ] ] 
    [ h2 [ css [ marginBottom (rem 0.5), fontWeight normal ] ] [ text "Popular tabs" ]
    , tabsTable
        [ { id = 3, title = "Tab 3", artist = { id = 3, name = "Arist 3" }, rating = 5 }
        , { id = 4, title = "Tab 4", artist = { id = 4, name = "Arist 4" }, rating = 4 }
        ]
    ]

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

tabsTable : List Tab -> Html Msg
tabsTable tabs = Html.table [  css [ borderCollapse collapse ] ]
        (tr [] 
            [ th [ css [ textAlign start, tableCell ] ] [ text "Title" ]
            , th [ css [ textAlign start, tableCell ] ] [ text "Arist" ]
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