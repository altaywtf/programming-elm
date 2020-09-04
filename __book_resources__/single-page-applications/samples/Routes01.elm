-- START:module
module Routes exposing (Route(..), match)
-- END:module

-- START:import
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)
-- END:import


-- START:type.Route
type Route
    = Home
    | Account
-- END:type.Route


-- START:match
match : Url -> Maybe Route
match url =
    Parser.parse routes url
-- END:match


-- START:routes
routes : Parser (Route -> a) a
routes =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Account (Parser.s "account")
        ]
-- END:routes
