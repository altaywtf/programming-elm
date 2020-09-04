module Debugging exposing (main)

import Html exposing (Html, text)
import Json.Decode as Json
import Json.Decode.Pipeline exposing (required)


-- START:breed
type Breed
    = Sheltie
    | Poodle
    | Beagle
-- END:breed


breedToString : Breed -> String
-- START:breedToString
breedToString breed =
    case breed of
        Sheltie ->
            "Sheltie"

        Poodle ->
            "Poodle"

        Beagle ->
            "Beagle"
-- END:breedToString


type alias Dog =
    { name : String
    , age : Int
    , breed : Breed
    }


decodeBreed : String -> Json.Decoder Breed
decodeBreed breed =
    -- START:decodeBreed
    case String.toLower breed of
        "sheltie" ->
            Json.succeed Sheltie

        "poodle" ->
            Json.succeed Poodle

        "beagle" ->
            Json.succeed Beagle

        _ ->
            Json.fail ("Unknown breed " ++ breed)
    -- END:decodeBreed


dogDecoder : Json.Decoder Dog
dogDecoder =
    Json.succeed Dog
        |> required "name" Json.string
        |> required "age" Json.int
        |> required "breed" (Json.string |> Json.andThen decodeBreed)


jsonDog : String
jsonDog =
    """
    {
      "name": "Tucker",
      "age": 11,
      "breed": "Sheltie"
    }
    """


decodedDog : Result Json.Error Dog
decodedDog =
    Json.decodeString dogDecoder jsonDog


viewDog : Dog -> Html msg
viewDog dog =
    text <|
        dog.name
            ++ " the "
            ++ breedToString dog.breed
            ++ " is "
            ++ String.fromInt dog.age
            ++ " years old."


main : Html msg
main =
    -- START:main
    case decodedDog of
    -- END:main
        Ok dog ->
            viewDog dog

        Err _ ->
            text "ERROR: Couldn't decode dog."
