module AppTest exposing (suite)

import App
import AwesomeDate as Date exposing (Date)
import Expect
import Test exposing (..)


selectedDate : Date
selectedDate =
    Date.create 2012 6 2


futureDate : Date
futureDate =
    Date.create 2015 9 21


initialModel : App.Model
initialModel =
    { selectedDate = selectedDate
    , years = Nothing
    , months = Nothing
    , days = Nothing
    }


modelWithDateOffsets : App.Model
modelWithDateOffsets =
    { initialModel
        | years = Just 3
        , months = Just 2
        , days = Just 50
    }


selectDate : Date -> App.Msg
selectDate date =
    App.SelectDate (Just date)


changeDateOffset : App.DateOffsetField -> Int -> App.Msg
changeDateOffset field amount =
    App.ChangeDateOffset field (Just amount)


testUpdate : Test
testUpdate =
    describe "update"
        [ test "selects a date" <|
            \_ ->
                App.update (selectDate futureDate) initialModel
                    |> Tuple.first
                    |> Expect.equal { initialModel | selectedDate = futureDate }
        , test "changes years" <|
            \_ ->
                App.update (changeDateOffset App.Years 3) initialModel
                    |> Tuple.first
                    |> Expect.equal { initialModel | years = Just 3 }
        , test "changes months" <|
            \_ ->
                App.update (changeDateOffset App.Months 3) initialModel
                    |> Tuple.first
                    |> Expect.equal { initialModel | months = Just 3 }
        , test "changes days" <|
            \_ ->
                App.update (changeDateOffset App.Days 3) initialModel
                    |> Tuple.first
                    |> Expect.equal { initialModel | days = Just 3 }
        ]


testView : Test
testView =
    todo "implement view tests"


testEvents : Test
testEvents =
    todo "implement event tests"


suite : Test
suite =
    describe "App"
        [ testUpdate
        , testView
        , testEvents
        ]
