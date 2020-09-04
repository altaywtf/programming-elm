module GetDog exposing (Dog, Trick(..), createDog, getDog, suite)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Dict exposing (Dict)
import Set


-- START:types
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
-- END:types


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


-- START:uniqueBy
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
-- END:uniqueBy


-- START:createDog
createDog : String -> List Trick -> Dog
createDog name tricks =
    Dog name (uniqueBy trickToString tricks)
-- END:createDog


-- START:getDog
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
-- END:getDog


-- START:withDefaultLazy
withDefaultLazy : (() -> a) -> Maybe a -> a
withDefaultLazy thunk maybe =
    case maybe of
        Just value ->
            value

        Nothing ->
            thunk ()
-- END:withDefaultLazy


getDogLazy : Dict String Dog -> String -> List Trick -> ( Dog, Dict String Dog )
getDogLazy dogs name tricks =
    let
        dog =
            Dict.get name dogs
                -- START:getDogLazy
                |> withDefaultLazy (\() -> createDog name tricks)
                -- END:getDogLazy

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


-- START:benchmark.dogExists
dogExists : Benchmark
dogExists =
    describe "dog exists"
        [ Benchmark.compare "implementations"
            "eager creation"
            (\_ -> getDog benchmarkDogs "Tucker" benchmarkTricks)
            "lazy creation"
            (\_ -> getDogLazy benchmarkDogs "Tucker" benchmarkTricks)
        ]
-- END:benchmark.dogExists


suite : Benchmark
suite =
    describe "getDog"
        [ dogExists ]


main : BenchmarkProgram
main =
    program suite
