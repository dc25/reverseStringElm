import Html exposing (Html, Attribute, text, div, input)
import Html.Attributes exposing (placeholder, value, style)
import Html.Events exposing (on, targetValue)
import Signal exposing (Address)
import StartApp.Simple exposing (start)
import String exposing (toInt)
import Maybe exposing (withDefault)

months : List String
months= ["January ", "February ", "March ", "April ", "May ", "June ", "July ", "August ", "September ", "October ", "November ", "December "]

isLeap : Int -> Bool
isLeap year = (year % 400) == 0 || ( (year % 4) == 0 && (year % 100) /= 0 )

daysInMonth : Int -> List Int
daysInMonth year = 
  let febDays = if isLeap(year) then 29 else 28
  in [31, febDays, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

-- Sunday through Saturday are represented by 0 through 6.
firstDayOfYear : Int -> Int
firstDayOfYear year =
   let firstYear = 1601
       delta = year - firstYear
       daysBeforeThisYear = 365  * delta + delta // 4 - delta // 100 + delta // 400
       firstDayOf1601 = 1 -- Monday 
   in (firstDayOf1601 + daysBeforeThisYear) % 7

lastFridayOfMonth : Int -> List Int
lastFridayOfMonth year =
  let daysBeforeMonth -- number of days in year before this month
      = List.scanl (+) 0 (daysInMonth year)

      -- number of days in year before next month
      daysBeforeNextMonth 
      = daysBeforeMonth
      |> List.tail
      |> Maybe.withDefault []

      -- index of the first day of the last week of the month
      -- The "index" of a day is how many days into the year that day is.
      lastWeekIndex 
      = daysBeforeNextMonth
      |> List.map ((+) -7)

      -- day of week of first day of the last week of the month 
      firstDayOfLastWeekInMonth 
      = lastWeekIndex 
      |> List.map ((+) (firstDayOfYear year))
      |> List.map (\dayIndex -> dayIndex % 7)

      -- days until Friday from the first day of the last week of the month
      fridayOffset -- 
      = firstDayOfLastWeekInMonth 
      |> List.map (\day -> (12-day) % 7)

      -- index of the last friday of the month
      indexOfLastFriday 
      = List.map2 (+) lastWeekIndex fridayOffset

      -- "zero offset date" of the last friday of the month
      dateOfLastFriday
      = List.map2 (-) indexOfLastFriday daysBeforeMonth

      -- date of the last friday of the month.
      lastFridays
      = List.map ((+) 1) dateOfLastFriday

  in lastFridays

errString : String
errString = "Only years after 1752 are valid."

lastFridays : String -> List String
lastFridays yearString = 
  case toInt yearString of 
    Ok year -> if (year < 1753) 
                  then [errString] 
                  else lastFridayOfMonth year
                       |> List.map toString 
                       |> List.map2 (++) months 
                       |> List.map (\s -> s ++ ", " ++ yearString)
    Err _ -> [errString]

main = start { model = "", view = view, update = update }

update newStr oldStr = newStr

view : Address String -> String -> Html
view address yearString =
  div []
    ([ input
        [ placeholder "Enter a year."
        , value yearString
        , on "input" targetValue (Signal.message address)
        , myStyle
        ]
        []
     ] ++ (lastFridays yearString
           |> List.map (\date -> div [ myStyle ] [ text date ]) ))

myStyle : Attribute
myStyle =
  style
    [ ("width", "100%")
    , ("height", "20px")
    , ("padding", "5px 0 0 5px")
    , ("font-size", "1em")
    , ("text-align", "left")
    ]
