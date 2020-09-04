module Debugging exposing (main)

import Html exposing (Html, text)
import Json.Decode as Json
import Json.Decode.Pipeline exposing (required)


-- START:breed
type Breed
    = Sheltie
    | Poodle
-- END:breed


-- START:breedToString
breedToString : Breed -> String
breedToString breed =
    case breed of
        Sheltie ->
            "Sheltie"

        _ ->
            Debug.todo "Handle other breeds in breedToString"
-- END:breedToString


-- START:type.alias.Dog
type alias Dog =
    { name : String
    , age : Int
    , breed : Breed
    }
-- END:type.alias.Dog


-- START:decodeBreed
decodeBreed : String -> Json.Decoder Breed
decodeBreed breed =
    case Debug.log "breed" breed of
        "Sheltie" ->
            Json.succeed Sheltie

        _ ->
            Debug.todo "Handle other breeds in decodeBreed"
-- END:decodeBreed


dogDecoder : Json.Decoder Dog
dogDecoder =
    Json.succeed Dog
        |> required "name" Json.string
        |> required "age" Json.int
        -- START:dogDecoder
        |> required "breed" (Json.string |> Json.andThen decodeBreed)
        -- END:dogDecoder


jsonDog : String
jsonDog =
    """
    -- START:jsonDog
    {
      "name": "Tucker",
      "age": 11,
      "breed": "Sheltie"
    }
    -- END:jsonDog
    """


decodedDog : Result Json.Error Dog
decodedDog =
    Json.decodeString dogDecoder jsonDog


viewDog : Dog -> Html msg
viewDog dog =
    -- START:viewDog
    text <|
        dog.name
            ++ " the "
            ++ breedToString dog.breed
            ++ " is "
            ++ String.fromInt dog.age
            ++ " years old."
    -- END:viewDog


main : Html msg
main =
    case Debug.log "decodedDog" decodedDog of
        Ok dog ->
            viewDog dog

        Err _ ->
            text "ERROR: Couldn't decode dog."
