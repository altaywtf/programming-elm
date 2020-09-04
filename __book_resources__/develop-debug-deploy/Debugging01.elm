-- START:modules
module Debugging exposing (main)

import Html exposing (Html, text)
import Json.Decode as Json
import Json.Decode.Pipeline exposing (required)
-- END:modules


-- START:type.alias
type alias Dog =
    { name : String
    , age : Int
    }
-- END:type.alias


-- START:dogDecoder
dogDecoder : Json.Decoder Dog
dogDecoder =
    Json.succeed Dog
        |> required "name" Json.string
        |> required "age" Json.int
-- END:dogDecoder


-- START:jsonDog
jsonDog : String
jsonDog =
    """
    {
      "name": "Tucker",
      "age": 11
    }
    """
-- END:jsonDog


-- START:decodedDog
decodedDog : Result Json.Error Dog
decodedDog =
    Json.decodeString dogDecoder jsonDog
-- END:decodedDog


-- START:viewDog
viewDog : Dog -> Html msg
viewDog dog =
    text <|
        dog.name
            ++ " is "
            ++ String.fromInt dog.age
            ++ " years old."
-- END:viewDog


-- START:main
main : Html msg
main =
    case Debug.log "decodedDog" decodedDog of
        Ok dog ->
            viewDog dog

        Err _ ->
            text "ERROR: Couldn't decode dog."
-- END:main
