module AwesomeDate exposing
    ( Date
    , Weekday(..)
    , addDays
    , addMonths
    , addYears
    , create
    , day
    , daysInMonth
    , fromISO8601
    , isLeapYear
    , month
    , toDateString
    , toISO8601
    , weekday
    , weekdayToString
    , year
    )


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


toDateString : Date -> String
toDateString (Date date) =
    [ date.month, date.day, date.year ]
        |> List.map String.fromInt
        |> String.join "/"


addYears : Int -> Date -> Date
addYears years (Date date) =
    Date { date | year = date.year + years }
        |> preventInvalidLeapDates


preventInvalidLeapDates : Date -> Date
preventInvalidLeapDates (Date date) =
    if not (isLeapYear date.year) && date.month == 2 && date.day >= 29 then
        Date { date | day = 28 }

    else
        Date date



{- AwesomeDate Helpers -}
{- Adapted from https://github.com/elm-community/elm-time -}
{-
   Copyright (c) 2016, Bogdan Paul Popa
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:
       * Redistributions of source code must retain the above copyright
         notice, this list of conditions and the following disclaimer.
       * Redistributions in binary form must reproduce the above copyright
         notice, this list of conditions and the following disclaimer in the
         documentation and/or other materials provided with the distribution.
       * Neither the name of the <organization> nor the
         names of its contributors may be used to endorse or promote products
         derived from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
   ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
   DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
   (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
   ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}


type Weekday
    = Sunday
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday


addMonths : Int -> Date -> Date
addMonths months (Date date) =
    Date { date | month = date.month + months }
        |> ensureValidDate


addDays : Int -> Date -> Date
addDays days (Date date) =
    (daysFromYearMonthDay date.year date.month date.day + days)
        |> dateFromDays


daysInMonth : Int -> Int -> Int
daysInMonth year_ month_ =
    case month_ of
        2 ->
            if isLeapYear year_ then
                29

            else
                28

        4 ->
            30

        6 ->
            30

        9 ->
            30

        11 ->
            30

        _ ->
            31


toISO8601 : Date -> String
toISO8601 (Date date) =
    [ String.fromInt date.year, padZero date.month, padZero date.day ]
        |> String.join "-"


fromISO8601 : String -> Maybe Date
fromISO8601 input =
    let
        parsed =
            input
                |> String.split "-"
                |> List.map String.toInt
    in
    case parsed of
        [ Just year_, Just month_, Just day_ ] ->
            Just (create year_ month_ day_)

        _ ->
            Nothing


weekday : Date -> Weekday
weekday (Date date) =
    let
        m =
            if date.month == 1 then
                0

            else if date.month == 2 then
                3

            else if date.month == 3 then
                2

            else if date.month == 4 then
                5

            else if date.month == 5 then
                0

            else if date.month == 6 then
                3

            else if date.month == 7 then
                5

            else if date.month == 8 then
                1

            else if date.month == 9 then
                4

            else if date.month == 10 then
                6

            else if date.month == 11 then
                2

            else
                4

        y =
            if date.month < 3 then
                date.year - 1

            else
                date.year

        d =
            modBy 7 (y + y // 4 - y // 100 + y // 400 + m + date.day)
    in
    if d == 0 then
        Sunday

    else if d == 1 then
        Monday

    else if d == 2 then
        Tuesday

    else if d == 3 then
        Wednesday

    else if d == 4 then
        Thursday

    else if d == 5 then
        Friday

    else
        Saturday


ensureValidDate : Date -> Date
ensureValidDate date =
    date
        |> ensureValidMonth
        |> preventInvalidLeapDates


ensureValidMonth : Date -> Date
ensureValidMonth (Date date) =
    if date.month < 1 || date.month > 12 then
        let
            monthOffset =
                date.month - 1

            newMonth =
                modBy 12 monthOffset + 1

            newYear =
                date.year + floor (toFloat monthOffset / 12)
        in
        Date { date | year = newYear, month = newMonth }

    else
        Date date


padZero : Int -> String
padZero =
    String.fromInt >> String.padLeft 2 '0'


daysFromYearMonthDay : Int -> Int -> Int -> Int
daysFromYearMonthDay year_ month_ day_ =
    let
        yds =
            daysFromYear year_

        mds =
            daysFromYearMonth year_ month_

        dds =
            day_ - 1
    in
    yds + mds + dds


daysFromYearMonth : Int -> Int -> Int
daysFromYearMonth year_ month_ =
    let
        go y m acc =
            if m == 0 then
                acc

            else
                go y (m - 1) (acc + daysInMonth y m)
    in
    go year_ (month_ - 1) 0


daysFromYear : Int -> Int
daysFromYear y =
    if y > 0 then
        366
            + ((y - 1) * 365)
            + ((y - 1) // 4)
            - ((y - 1) // 100)
            + ((y - 1) // 400)

    else if y < 0 then
        (y * 365)
            + (y // 4)
            - (y // 100)
            + (y // 400)

    else
        0


yearFromDays : Int -> Int
yearFromDays ds =
    let
        y =
            ds // 365

        d =
            daysFromYear y
    in
    if ds <= d then
        y - 1

    else
        y


dateFromDays : Int -> Date
dateFromDays ds =
    let
        d400 =
            daysFromYear 400

        y400 =
            ds // d400

        d =
            remainderBy d400 ds

        year_ =
            yearFromDays (d + 1)

        leap =
            if isLeapYear year_ then
                (+) 1

            else
                identity

        doy =
            d - daysFromYear year_

        ( month_, day_ ) =
            if doy < 31 then
                ( 1, doy + 1 )

            else if doy < leap 59 then
                ( 2, doy - 31 + 1 )

            else if doy < leap 90 then
                ( 3, doy - leap 59 + 1 )

            else if doy < leap 120 then
                ( 4, doy - leap 90 + 1 )

            else if doy < leap 151 then
                ( 5, doy - leap 120 + 1 )

            else if doy < leap 181 then
                ( 6, doy - leap 151 + 1 )

            else if doy < leap 212 then
                ( 7, doy - leap 181 + 1 )

            else if doy < leap 243 then
                ( 8, doy - leap 212 + 1 )

            else if doy < leap 273 then
                ( 9, doy - leap 243 + 1 )

            else if doy < leap 304 then
                ( 10, doy - leap 273 + 1 )

            else if doy < leap 334 then
                ( 11, doy - leap 304 + 1 )

            else
                ( 12, doy - leap 334 + 1 )
    in
    Date
        { year = year_ + y400 * 400
        , month = month_
        , day = day_
        }


weekdayToString : Weekday -> String
weekdayToString weekday_ =
    case weekday_ of
        Sunday ->
            "Sunday"

        Monday ->
            "Monday"

        Tuesday ->
            "Tuesday"

        Wednesday ->
            "Wednesday"

        Thursday ->
            "Thursday"

        Friday ->
            "Friday"

        Saturday ->
            "Saturday"
