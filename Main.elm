import Html exposing (Html, Attribute, text, div, input)
import Html.Attributes exposing (placeholder, value, style)
import Html.Events exposing (on, targetValue)
import Signal exposing (Address)
import StartApp.Simple exposing (start)
import String exposing (reverse)

main = start { model = "", view = view, update = update }

update newStr oldStr = newStr

view : Address String -> String -> Html
view address forwards =
  div []
    ([ input
        [ placeholder "Enter a string to reverse."
        , value forwards
        , on "input" targetValue (Signal.message address)
        , myStyle
        ]
        []
     ] ++ 
     [ div [ myStyle] [text (reverse forwards)]
     ])

myStyle : Attribute
myStyle =
  style
    [ ("width", "100%")
    , ("height", "20px")
    , ("padding", "5px 0 0 5px")
    , ("font-size", "1em")
    , ("text-align", "left")
    ]
