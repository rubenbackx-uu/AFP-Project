module Buttons exposing (main)

import Browser
import Html exposing (Html, button, div, span, text)
import Html.Events exposing (onClick)
import Http exposing (header)
import Json.Decode exposing (Decoder, map2, field, int, string, list)
import List exposing (map)

-- MAIN

main : Program () Model Msg
main = Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

-- MODEL

type Model = Loading | Failure | Users (List User)

init : flags -> (Model, Cmd Msg)
init _ = (Loading, Cmd.none)

type alias User = { id : Int, name : String }

-- UPDATE

type Msg = GetUsers | GotUsers (Result Http.Error (List User))

update : Msg -> Model -> (Model, Cmd Msg)
update msg _ = case msg of
   GetUsers -> (Loading, getUsers)
   GotUsers result -> case result of
      Ok users -> (Users users, Cmd.none)
      Err _ -> (Failure, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- VIEW

view : Model -> Html Msg
view model = div [] [ button [ onClick GetUsers ] [ text "Get users" ] , viewUsers model ]

viewUsers : Model -> Html Msg
viewUsers model = case model of
    Loading -> span [] [text "Loading"]
    Failure -> span [] [text "Failure"]
    Users users -> div [] (map viewUser users)

viewUser : User -> Html Msg
viewUser user = 
    div [] [ span [] [ text user.name ], span [] [ text ": " ], span [] [ text (String.fromInt user.id) ] ]

-- HTTP
getUsers : Cmd Msg
getUsers = Http.request 
    { method = "GET"
    , url = "http://localhost:8300/users"
    , headers = [ header "Authorization" "Basic dXNlcjE6dXNlcjE=" ]
    , expect = Http.expectJson GotUsers usersDecoder 
    , body = Http.emptyBody
    , timeout = Nothing
    , tracker = Nothing
    }

usersDecoder : Decoder (List User)
usersDecoder = list (map2 User (field "userId" int) (field "username" string))





