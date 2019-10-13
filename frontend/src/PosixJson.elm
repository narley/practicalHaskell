module PosixJson exposing (jsonDecPosix, jsonEncPosix)

import Json.Encode exposing (..)
import Json.Decode exposing (..)
import List exposing (range, map, sum)
import Time exposing (..)

jsonDecPosix : Decoder Posix
jsonDecPosix = succeed (millisToPosix 0)

jsonEncPosix : Posix -> Json.Encode.Value
jsonEncPosix posixTime = Json.Encode.int (posixToMillis posixTime)

type alias Iso8601Elements =
  { year : Int
  , month : Int
  , day : Int
  , hour : Int
  , minute : Int
  , second : Int
  }

-- Given the number of milliseconds since the epoch, return the
-- Year, Month, Day, Hour, Minute and Second
getIso8601CalendarElems : Int -> Iso8601Elements
getIso8601CalendarElems msSinceEpoch =
  let yearsSinceEpoch = msSinceEpoch // millisecondsPerYear
      year = 1970 + yearsSinceEpoch
      millisecondsIntoYear = modBy millisecondsPerYear msSinceEpoch
      daysIntoYear = millisecondsIntoYear // millisecondsPerDay
      (month, day) = monthAndDayFromDayIntoYear year daysIntoYear
      millisecondsIntoDay = modBy millisecondsPerDay millisecondsIntoYear
      hour = millisecondsIntoDay // millisecondsPerHour
      millisecondsIntoHour = modBy millisecondsPerHour millisecondsIntoDay
      minute = millisecondsIntoHour // millisecondsPerMinute
      second = (modBy millisecondsPerMinute millisecondsIntoHour) // 1000
  in  Iso8601Elements year month day hour minute second

monthAndDayFromDayIntoYear : Int -> Int -> (Int, Int)
monthAndDayFromDayIntoYear year daysIntoYear =
  let accumulatedDaysPerMonth = if isLeapYear year
        then List.reverse [0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335]
        else List.reverse [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]
      remainingMonths = dropWhile (\a -> a > daysIntoYear) accumulatedDaysPerMonth
      day = case List.head remainingMonths of
              Nothing -> 0 -- Can't happen
              Just d -> daysIntoYear - d
      month = List.length remainingMonths
  in  (month, day)

-- How many days have past in the year, up to (but not including)
-- the given day, since that day is a "partial" day for the purposes
-- of calculating the number of milliseconds. The year is needed for
-- leap year calculations.
--
-- Example: dayOfTheYear 2019 1 10 = 9
--          dayOfTheYear 2019 3 15 = 73
dayOfTheYear : Int -> Int -> Int -> Int
dayOfTheYear year month day = if month == 1
  then day - 1
  else List.sum (List.map (daysPerMonth year) (List.range 1 (month - 1))) + day - 1

millisecondsPerYear : Int
millisecondsPerYear = 31536000000

millisecondsPerDay : Int
millisecondsPerDay = 86400000

millisecondsPerHour : Int
millisecondsPerHour = 3600000

millisecondsPerMinute : Int
millisecondsPerMinute = 60000

daysPerMonth : Int -> Int -> Int
daysPerMonth year month = case month of
  1 -> 31
  2 -> if isLeapYear year then 29 else 28
  3 -> 31
  4 -> 30
  5 -> 31
  6 -> 30
  7 -> 31
  8 -> 31
  9 -> 30
  10 -> 31
  11 -> 30
  12 -> 31
  _ -> 0

isLeapYear : Int -> Bool
isLeapYear x = if modBy 4 x /= 0
  then False
  else if modBy x 100 /= 0
    then True
    else modBy x 500 == 0

unixEpochYear : Int
unixEpochYear = 1970

dropWhile : (a -> Bool) -> List a -> List a
dropWhile pred lst = case lst of
  [] -> []
  (li :: lis) -> if pred li
    then li :: (dropWhile pred lis)
    else dropWhile pred lis
