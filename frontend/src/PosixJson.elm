module PosixJson exposing (..)

import Json.Encode as E
import Json.Decode as D
import List exposing (range, map, sum)
import Time exposing (..)

jsonDecPosix : D.Decoder Posix
jsonDecPosix = D.succeed (millisToPosix 0)

jsonEncPosix : Posix -> E.Value
jsonEncPosix posixTime = E.int (posixToMillis posixTime)

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
  let (year, millisecondsIntoYear) = getYearAndRemainingSeconds unixEpochYear msSinceEpoch
      daysIntoYear = millisecondsIntoYear // millisecondsPerDay
      (month, day) = monthAndDayFromDayIntoYear year daysIntoYear
      millisecondsIntoDay = modBy millisecondsPerDay millisecondsIntoYear
      hour = millisecondsIntoDay // millisecondsPerHour
      millisecondsIntoHour = modBy millisecondsPerHour millisecondsIntoDay
      minute = millisecondsIntoHour // millisecondsPerMinute
      second = (modBy millisecondsPerMinute millisecondsIntoHour) // 1000
  in  Iso8601Elements year month day hour minute second

millisFromCalendarElems: Iso8601Elements -> Int
millisFromCalendarElems {year, month, day, hour, minute, second} =
  let days = dayOfTheYear year month day
  in  (millisecondsInCalendarYear (year - 1)) +
      (days * millisecondsPerDay) +
      (hour * millisecondsPerHour) +
      (minute * millisecondsPerMinute) +
      (second * 1000)

monthAndDayFromDayIntoYear : Int -> Int -> (Int, Int)
monthAndDayFromDayIntoYear year daysIntoYear =
  let accumulatedDaysPerMonth = if isLeapYear year
        then List.reverse [0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335]
        else List.reverse [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]
      remainingMonths = dropWhile (\a -> a <= daysIntoYear) accumulatedDaysPerMonth
      day = case List.head remainingMonths of
              Nothing -> 0 -- Can't happen
              Just d -> daysIntoYear - d + 1
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

millisecondsPerLeapYear : Int
millisecondsPerLeapYear = millisecondsPerYear + millisecondsPerDay

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
  else if modBy 100 x /= 0
    then True
    else modBy 400 x == 0

unixEpochYear : Int
unixEpochYear = 1970

zeroPad : Int -> String
zeroPad i = if i < 10
  then String.append "0" (String.fromInt i)
  else String.fromInt i

dropWhile : (a -> Bool) -> List a -> List a
dropWhile pred lst = case lst of
  [] -> []
  (li :: lis) -> if pred li
    then li :: (dropWhile pred lis)
    else dropWhile pred lis

getYearAndRemainingSeconds: Int -> Int -> (Int, Int)
getYearAndRemainingSeconds currentYear milliseconds =
  let millisInYear = if isLeapYear currentYear then millisecondsPerLeapYear else millisecondsPerYear
  in  if millisInYear > milliseconds then (currentYear, milliseconds)
        else getYearAndRemainingSeconds (currentYear + 1) (milliseconds - millisInYear)

millisecondsInCalendarYear : Int -> Int
millisecondsInCalendarYear year = if year < 1970
  then 0
  else if isLeapYear year
    then millisecondsPerLeapYear + (millisecondsInCalendarYear (year - 1))
    else millisecondsPerYear + (millisecondsInCalendarYear (year - 1))
