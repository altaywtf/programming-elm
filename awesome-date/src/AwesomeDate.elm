module AwesomeDate exposing (..)


type Date
    = Date { year : Int, month : Int, day : Int }


create : Int -> Int -> Int -> Date
create year_ month_ day_ =
    Date { year = year_, month = month_, day = day_ }


year : Date -> Int
year (Date date) =
    date.year


month : Date -> Int
month (Date date) =
    date.month


day : Date -> Int
day (Date date) =
    date.day


isLeapYear : Int -> Bool
isLeapYear year_ =
    let
        isDivisibleBy n =
            remainderBy n year_ == 0
    in
    isDivisibleBy 4 && not (isDivisibleBy 100) || isDivisibleBy 400


preventInvalidLeapDates : Date -> Date
preventInvalidLeapDates (Date date) =
    if not (isLeapYear date.year) && date.month == 2 && date.day >= 29 then
        Date { date | day = 28 }

    else
        Date date


addYears : Int -> Date -> Date
addYears years (Date date) =
    Date { date | year = date.year + years }
        |> preventInvalidLeapDates


toDateString : Date -> String
toDateString (Date date) =
    [ date.month, date.day, date.year ]
        |> List.map String.fromInt
        |> String.join "/"
