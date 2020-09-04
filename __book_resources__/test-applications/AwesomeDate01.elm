-- START:module
module AwesomeDate exposing (Date, create, year)
-- END:module


-- START:type.Date
type Date
    = Date { year : Int, month : Int, day : Int }
-- END:type.Date


-- START:create
create : Int -> Int -> Int -> Date
create year_ month_ day_ =
    Date { year = year_, month = month_, day = day_ }
-- END:create


-- START:year
year : Date -> Int
year (Date date) =
    date.year
-- END:year
