module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Debug exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (href)
import Route exposing (Route)
import Url

import Page.Home as Home
import Page.Profile as Profile
import Page.Error as Error
import Page.Tab as Tab
import Page.Login as Login

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
    | ProfileMsg Profile.Msg
    | ErrorMsg Error.Msg
    | TabMsg Tab.Msg
    | LoginMsg Login.Msg

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
        LoginMsg m -> Login.update m () |> updateWith (\_ -> model) LoginMsg
        ProfileMsg m -> Profile.update m () |> updateWith (\_ -> model) ProfileMsg
        ErrorMsg m -> Error.update m () |> updateWith (\_ -> model) ErrorMsg
        TabMsg m -> Tab.update m (tempTab 0) |> updateWith (\_ -> model) TabMsg

tempTab : Int -> Tab.Model
tempTab id = { tab = { id = id, title = "Tab " ++ (String.fromInt id), artist = { id = 1, name = "Artist 1" }, lines = 
    [ { text = "This is some line", chords = [ { position = 3, symbol = "C" }, { position = 6, symbol = "G" } ] }
    , { text = "This is another line", chords = [ { position = 8, symbol = "A#sus4add9" } ] }
    , { text = "", chords = [] }
    , { text = "There are more lines", chords = [ { position = 0, symbol = "C7dim" }, { position = 10, symbol = "D6add9" } ] }
    , { text = "This will be the final line", chords = [ { position = 2, symbol = "A" } ] }
    ] } }

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
        ErrorPage -> viewPage (Error.view ()) ErrorMsg
        OnPage Route.Home -> viewPage (Home.view ()) HomeMsg
        OnPage Route.Login -> viewPage (Login.view ()) LoginMsg
        OnPage Route.Profile -> viewPage (Profile.view ()) ProfileMsg
        OnPage (Route.Tab id) -> viewPage (Tab.view (tempTab id)) TabMsg