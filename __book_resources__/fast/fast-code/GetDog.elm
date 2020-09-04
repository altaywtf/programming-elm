module GetDog exposing (Dog, Trick(..), createDog, getDog, suite)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Dict exposing (Dict)
import Set


type Trick
    = Sit
    | RollOver
    | Speak
    | Fetch
    | Spin


type alias Dog =
    { name : String
    , tricks : List Trick
    }


trickToString : Trick -> String
trickToString trick =
    case trick of
        Sit ->
            "Sit"

        RollOver ->
            "RollOver"

        Speak ->
            "Speak"

        Fetch ->
            "Fetch"

        Spin ->
            "Spin"


uniqueBy : (a -> comparable) -> List a -> List a
uniqueBy toComparable list =
    List.foldr
        (\item ( existing, accum ) ->
            let
                comparableItem =
                    toComparable item
            in
            if Set.member comparableItem existing then
                ( existing, accum )

            else
                ( Set.insert comparableItem existing, item :: accum )
        )
        ( Set.empty, [] )
        list
        |> Tuple.second


createDog : String -> List Trick -> Dog
createDog name tricks =
    Dog name (uniqueBy trickToString tricks)


getDog : Dict String Dog -> String -> List Trick -> ( Dog, Dict String Dog )
getDog dogs name tricks =
    let
        dog =
            Dict.get name dogs
                |> Maybe.withDefault (createDog name tricks)

        newDogs =
            Dict.insert name dog dogs
    in
    ( dog, newDogs )


benchmarkTricks : List Trick
benchmarkTricks =
    [ Sit, RollOver, Speak, Fetch, Spin ]


benchmarkDogs : Dict String Dog
benchmarkDogs =
    Dict.fromList
        [ ( "Tucker", createDog "Tucker" benchmarkTricks ) ]


dogExists : Benchmark
dogExists =
    describe "dog exists" []


suite : Benchmark
suite =
    describe "getDog"
        [ dogExists ]


main : BenchmarkProgram
main =
    program suite
