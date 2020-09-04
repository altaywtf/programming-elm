module AwesomeDate exposing (Date, create, day, month, year)


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
