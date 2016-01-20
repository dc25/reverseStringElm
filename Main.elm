import Html exposing (Html, Attribute, text, toElement, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import String


main =
  StartApp.start { model = "", view = view, update = update }


update newStr oldStr =
  newStr

lastFridays : String -> List String
lastFridays string = 
  [String.reverse string, string]

view : Address String -> String -> Html
view address string =
  div []
    ([ input
        [ placeholder "Text to reverse"
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
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
