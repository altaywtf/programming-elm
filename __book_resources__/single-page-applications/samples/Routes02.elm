-- START:module
module Routes exposing (Route(..), href, match)
-- END:module

-- START:import
import Html
import Html.Attributes
-- END:import
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)


type Route
    = Home
    | Account


-- START:href
href : Route -> Html.Attribute msg
href route =
    Html.Attributes.href (routeToUrl route)
-- END:href


match : Url -> Maybe Route
match url =
    Parser.parse routes url


-- START:routeToUrl
routeToUrl : Route -> String
routeToUrl route =
    case route of
        Home ->
            "/"

        Account ->
            "/account"
-- END:routeToUrl


routes : Parser (Route -> a) a
routes =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Account (Parser.s "account")
        ]
