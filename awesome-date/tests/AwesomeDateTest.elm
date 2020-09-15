module AwesomeDateTest exposing (suite)

import AwesomeDate as Date exposing (Date)
import Expect
import Test exposing (..)


exampleDate : Date
exampleDate =
    Date.create 2012 6 2


testDateParts : Test
testDateParts =
    describe "date part getters"
        [ test "retrieves the year from a date" <|
            \_ -> Date.year exampleDate |> Expect.equal 2012
        , test "retrieves the month from a date" <|
            \_ -> Date.month exampleDate |> Expect.equal 6
        , test "retrieves the day from a date" <|
            \_ -> Date.day exampleDate |> Expect.equal 2
        ]


suite : Test
suite =
    describe "AwesomeDate"
        [ testDateParts
        ]
