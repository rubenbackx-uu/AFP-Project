module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Route exposing (Route)
import Url

import Page.Home as Home

-- MAIN

main : Program () Model Msg
main = Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }


-- MODEL

type Page = OnPage Route | ErrorPage

type alias Model =
    { key : Nav.Key
    , page : Page
    }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key = changeRoute (Route.fromUrl url) key

changeRoute : Maybe Route -> Nav.Key -> ( Model, Cmd Msg )
changeRoute mbRoute key =
    case mbRoute of
        Nothing -> ( { key = key, page = ErrorPage }, Cmd.none )
        Just route -> ( { key = key, page = OnPage route }, Cmd.none )


-- UPDATE

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomeMsg Home.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )
                Browser.External href ->
                    ( model, Nav.load href )
        UrlChanged url -> changeRoute (Route.fromUrl url) model.key
        HomeMsg m -> Home.update m () |> updateWith (\_ -> model) HomeMsg

updateWith : (model -> Model) -> (msg -> Msg) -> (model, Cmd msg) -> (Model, Cmd Msg)
updateWith toModel toMsg (subModel, subCmd) = (toModel subModel, Cmd.map toMsg subCmd)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- VIEW

view : Model -> Browser.Document Msg
view model =
    let 
        viewPage page toMsg = let { title, body } = page in { title = title, body = List.map (Html.map toMsg) body } 
    in
    case model.page of
        ErrorPage -> viewError
        OnPage Route.Home -> viewPage (Home.view ()) HomeMsg
        OnPage Route.Profile -> viewProfile
        OnPage (Route.Tab id) -> viewTab id

viewProfile : Browser.Document Msg
viewProfile =
    { title = "Profile"
    , body = 
        [ h1 [] [ text "Profile" ]
        , links 
        ]
    }

viewTab : Int -> Browser.Document Msg
viewTab id =
    { title = "Tab " ++ (String.fromInt id) 
    , body = 
        [ h1 [] [ text ("Tab " ++ (String.fromInt id)) ] 
        , links
        ]
    }

viewError : Browser.Document Msg
viewError = 
    { title = "Error" 
    , body = 
        [ h1 [] [ text "Error" ] 
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
