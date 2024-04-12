module Session exposing (..)

import Browser.Navigation as Nav

type alias User = { id : Int, username: String }

type alias Auth = { user : User, token : String }

type alias Session = { key : Nav.Key, auth : Maybe Auth }