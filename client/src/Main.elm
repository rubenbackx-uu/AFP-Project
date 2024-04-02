module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Debug exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (href)
import Route exposing (Route)
import Url
import Session exposing (..)

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

type Model 
    = Tab Tab.Model
    | Login Login.Model
    | Error Error.Model
    | Profile Profile.Model
    | Home Home.Model

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key = changeRoute (Route.fromUrl url) (Error { session = { key = key, auth = Nothing } })

changeRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRoute mbRoute model =
    case mbRoute of
        Just (Route.Tab id) -> Tab.init id (toSession model) |> updateWith Tab TabMsg
        Just Route.Login -> Login.init (toSession model) |> updateWith Login LoginMsg
        Just Route.Home -> Home.init (toSession model) |> updateWith Home HomeMsg
        Just Route.Profile -> ({ session = toSession model }, Cmd.none) |> updateWith Profile ProfileMsg
        Nothing -> ({ session = toSession model }, Cmd.none) |> updateWith Error ErrorMsg

toSession : Model -> Session
toSession model = case model of
    Tab tab -> Tab.toSession tab
    Login login -> Login.toSession login
    Home home -> home.session
    Profile profile -> profile.session
    Error error -> error.session


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
    case (msg, model) of
        (LinkClicked urlRequest, _) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (toSession model).key (Url.toString url) )
                Browser.External href ->
                    ( model, Nav.load href )
        (UrlChanged url, _) -> changeRoute (Route.fromUrl url) model
        (HomeMsg m, Home home) -> Home.update m home |> updateWith Home HomeMsg
        (LoginMsg m, Login login) -> Login.update m login |> updateWith Login LoginMsg
        (ProfileMsg m, Profile profile) -> Profile.update m profile |> updateWith Profile ProfileMsg
        (ErrorMsg m, Error error) -> Error.update m error |> updateWith Error ErrorMsg
        (TabMsg m, Tab tab) -> Tab.update m tab |> updateWith Tab TabMsg
        (_, _) -> (model, Cmd.none)

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
    case model of
        Tab tab -> viewPage (Tab.view tab) TabMsg
        Login login -> viewPage (Login.view login) LoginMsg
        Error error -> viewPage (Error.view error) ErrorMsg
        Profile profile -> viewPage (Profile.view profile) ProfileMsg
        Home home -> viewPage (Home.view home) HomeMsg