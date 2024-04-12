module Page.Login exposing (Model, Msg, view, update, init, toSession)

import Browser
import Page
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css, placeholder, type_, name, method, action)
import Html.Styled.Events exposing (onInput, onClick)
import Components.Header exposing (..)
import Components.Settings exposing (..)
import Components.Colour exposing (scheme)
import Components.Button exposing (..)
import Http
import Session exposing (..)
import Base64
import Json.Decode exposing (Decoder, map2, field, string, int)
import Browser.Navigation as Nav

-- MODEL

type Status = Success | Failure | None

type alias Model =
    { username : String
    , password : String
    , session : Session
    , status : Status
    }

toSession : Model -> Session
toSession model = model.session

-- INIT

init : Session -> (Model, Cmd Msg)
init session = ({ username = "", password = "", session = session, status = None }, Cmd.none)


-- UPDATE

type Msg 
    = Username String
    | Password String
    | Login
    | LoginResponse (Result Http.Error Auth)

update : Msg -> Model -> (Model, Cmd Msg) 
update msg model = case msg of
    Username username -> ( { model | username = username } , Cmd.none)
    Password password -> ( { model | password = password } , Cmd.none)
    Login -> (model, login model)
    LoginResponse res -> case res of
        Ok auth -> let session = model.session in 
            ({ model | status = Success, session = { session | auth = Just auth } }, Nav.pushUrl model.session.key "/")
        Err _ -> ({ model | status = Failure }, Cmd.none)

-- VIEW

view : Model -> Browser.Document Msg
view model = Page.view "Log in" model.session content

content : Html Msg
content = div [ ] 
    [ h1 [ css [ color scheme.textColour ] ] [ text "Log in" ]
    , div [] 
        [ form [ ]
            [ div [ css [ property "display" "grid", property "grid-template-columns" "6rem 16rem", property "gap" ".25rem", alignItems center, marginBottom (rem 1) ] ] 
                [ label [] [ text "Username" ]
                , input [ onInput Username,  css [ fontSize (rem 1), padding2 (rem 0.25) (rem 0.5) ], placeholder "Username...", type_ "username", name "username" ] []
                , label [] [ text "Password" ]
                , input [ onInput Password, css [ fontSize (rem 1), padding2 (rem 0.25) (rem 0.5) ], placeholder "Password...", type_ "password", name "username" ] []
                ]
            , btn [ onClick Login, type_ "button" ] "Log in"
            ]
        ]
    ]

login : Model -> Cmd Msg
login model = let token = Base64.encode (model.username ++ ":" ++ model.password) in Http.request 
    { method = "POST"
    , url = "http://localhost:8300/login"
    , headers = [ Http.header "Authorization" ("Basic " ++ token) ]
    , expect = Http.expectStringResponse LoginResponse (extractToken token)
    , body = Http.emptyBody
    , timeout = Nothing
    , tracker = Nothing
    }

extractToken : String -> Http.Response String -> Result Http.Error Auth
extractToken token response = case response of 
    Http.BadUrl_ url -> Err (Http.BadUrl url)
    Http.Timeout_ -> Err Http.Timeout
    Http.NetworkError_ -> Err Http.NetworkError
    Http.BadStatus_ meta _ -> Err (Http.BadStatus meta.statusCode)
    Http.GoodStatus_ meta body -> case Json.Decode.decodeString userDecoder body of
        Err _ -> Err (Http.BadStatus 422)
        Ok user -> Ok { token = token, user = user }


userDecoder : Decoder User
userDecoder = map2 User (field "userId" Json.Decode.int) (field "username" string)