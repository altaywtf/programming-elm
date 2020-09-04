module Routes exposing (Route(..), href, match)

import Html
import Html.Attributes
import Url exposing (Url)
-- START:import.Url.Parser
import Url.Parser as Parser exposing ((</>), Parser)
-- END:import.Url.Parser


type Route
    = Home
    | Account
    -- START:type.Route
    | UserFeed String
    -- END:type.Route


href : Route -> Html.Attribute msg
href route =
    Html.Attributes.href (routeToUrl route)


match : Url -> Maybe Route
match url =
    Parser.parse routes url


routeToUrl : Route -> String
routeToUrl route =
    case route of
        Home ->
            "/"

        Account ->
            "/account"

        -- START:routeToUrl.UserFeed
        UserFeed username ->
            "/user/" ++ username ++ "/feed"
        -- END:routeToUrl.UserFeed


routes : Parser (Route -> a) a
-- START:routes
routes =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Account (Parser.s "account")
        , Parser.map
            UserFeed
            (Parser.s "user" </> Parser.string </> Parser.s "feed")
        ]
-- END:routes
