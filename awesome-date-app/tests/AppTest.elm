module AppTest exposing (suite)

import App
import AwesomeDate as Date exposing (Date)
import Expect
import Html.Attributes exposing (type_, value)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, id, tag, text)


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
    describe "view"
        [ test "displays the selected date" <|
            \_ ->
                App.view initialModel
                    |> Query.fromHtml
                    |> Query.find [ tag "input", attribute (type_ "date") ]
                    |> Query.has [ attribute (value "2012-06-02") ]
        , test "displays the weekday" <|
            \_ ->
                App.view initialModel
                    |> Query.fromHtml
                    |> Query.find [ id "info-weekday" ]
                    |> Query.has [ text "Saturday" ]
        , test "displays the days in month" <|
            \_ ->
                App.view initialModel
                    |> Query.fromHtml
                    |> Query.find [ id "info-days" ]
                    |> Query.has [ text "30" ]
        , test "displays the leap year state" <|
            \_ ->
                App.view initialModel
                    |> Query.fromHtml
                    |> Query.find [ id "info-leap-year" ]
                    |> Query.has [ text "Yes" ]
        , test "displays the date part inputs with correct values" <|
            \_ ->
                App.view initialModel
                    |> Query.fromHtml
                    |> Query.find [ id "offset-years" ]
                    |> Query.has [ attribute (value "0") ]
        , test "displays the date part inputs with correct values, when there is an offset" <|
            \_ ->
                App.view modelWithDateOffsets
                    |> Query.fromHtml
                    |> Query.find [ id "offset-years" ]
                    |> Query.has [ attribute (value "3") ]
        ]


testEvents : Test
testEvents =
    describe "events"
        [ test "receives selected date changes" <|
            \_ ->
                App.view initialModel
                    |> Query.fromHtml
                    |> Query.find [ tag "input", attribute (type_ "date") ]
                    |> Event.simulate (Event.input "2015-09-21")
                    |> Event.expect (selectDate futureDate)
        , test "receives year offset changes" <|
            \_ ->
                App.view initialModel
                    |> Query.fromHtml
                    |> Query.find [ tag "input", id "offset-years" ]
                    |> Event.simulate (Event.input "3")
                    |> Event.expect (changeDateOffset App.Years 3)
        ]


suite : Test
suite =
    describe "App"
        [ testUpdate
        , testView
        , testEvents
        ]
