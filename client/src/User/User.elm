module User.User exposing (..)
import Json.Decode exposing (Decoder, map3, field, int, string)
import Http
import Html exposing (Html)

type alias Model = Maybe User

type alias User = {
    id: Int,
    username: String,
    jwt: String
    }

type Msg = GotUser (Result Http.Error User)

getUser : Cmd Msg
getUser = Http.get 
    { url = "/api/user",
    expect = Http.expectJson GotUser userDecoder
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of 
        GotUser result -> case result of
            Ok user -> (Just user, Cmd.none)
            Err _ -> (Nothing, Cmd.none)

userDecoder : Decoder User
userDecoder = map3 User
    (field "id" int)
    (field "username" string)
    (field "jwt" string)

viewLogin : Model -> Html Msg
viewLogin model = undefined