module Route exposing (Route(..), fromUrl, href, replaceUrl)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser exposing (Parser, (</>), int, map, oneOf, s, string, top, parse)

type Route
    = Home
    | Login
    | Profile
    | Tab Int

routeParser : Parser (Route -> a) a
routeParser =
    oneOf 
        [ map Home top
        , map Login (s "login")
        , map Profile (s "profile")
        , map Tab (s "tab" </> int)
        ]

fromUrl : Url -> Maybe Route
fromUrl url = parse routeParser url

href : Route -> Attribute msg
href target = Attr.href (routeToString target)

replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route = Nav.replaceUrl key (routeToString route)

routeToString : Route -> String
routeToString route = "#/" ++ String.join "/" (routeToPieces route)

routeToPieces : Route -> List String
routeToPieces route = 
    case route of
        Home -> []
        Login -> ["login"]
        Profile -> ["profile"]
        Tab id -> ["tab", String.fromInt id]
