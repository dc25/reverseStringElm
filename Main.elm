import Html exposing (Html, Attribute, text, toElement, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import String exposing (..)
import Maybe exposing (withDefault)

months : List String
months= ["January ", "February ", "March ", "April ", "May ", "June ", "July ", "August ", "September ", "October ", "November ", "December "]

isLeap : Int -> Bool
isLeap year = (year % 400) == 0 || ( (year % 4) == 0 && (year % 100) /= 0 )

daysInMonth : Int -> List Int
daysInMonth year = 
  let febDays = if isLeap(year) then 29 else 28
  in [31, febDays, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

firstDayOfYear : Int -> Int
firstDayOfYear year =
   let firstYear = 1601
       delta = year - firstYear
       daysBeforeThisYear = 365  * delta + delta // 4 - delta // 100 + delta // 400
       firstDayOf1601 = 1
   in (firstDayOf1601 + daysBeforeThisYear) % 7

lastFridayOfMonth : Int -> List Int
lastFridayOfMonth year =
  let daysBeforeMonth 
      = List.scanl (+) 0 (daysInMonth year)

      daysBeforeNextMonth 
      = daysBeforeMonth
      |> List.tail
      |> Maybe.withDefault []

      lastWeekIndex 
      = daysBeforeNextMonth
      |> List.map ((+) -7)

      firstDayOfLastWeekInMonth 
      = lastWeekIndex 
      |> List.map ((+) (firstDayOfYear year))
      |> List.map (\dayIndex -> dayIndex % 7)

      fridayOffset
      = firstDayOfLastWeekInMonth 
      |> List.map (\day -> (12-day) % 7)

      indexOfLastFriday 
      = List.map2 (+) lastWeekIndex fridayOffset

      dateOfLastFriday
      = List.map2 (-) indexOfLastFriday daysBeforeMonth

      offsetFridayDate
      = List.map ((+) 1) dateOfLastFriday

  in offsetFridayDate

errString : String
errString = "Only years after 1752 are valid."

lastFridays : String -> List String
lastFridays string = 
  case toInt string of 
      Ok year -> if (year < 1753) then [errString] else List.map2 (++) months (List.map toString (lastFridayOfMonth year))
      Err _ -> [errString]
                             
main =
  StartApp.start { model = "", view = view, update = update }

update newStr oldStr =
  newStr

view : Address String -> String -> Html
view address string =
  div []
    ([ input
        [ placeholder "Enter a year."
        , value string
        , on "input" targetValue (Signal.message address)
        , myStyle
        ]
        []
     ] ++ List.map (\date -> div [ myStyle ] [ text date ]) (lastFridays string)   )


myStyle : Attribute
myStyle =
  style
    [ ("width", "100%")
    , ("height", "20px")
    , ("padding", "5px 0")
    , ("font-size", "1em")
    , ("text-align", "center")
    ]
