import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import List
import Arithmetic

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }

model : Model
model =
  Model (List.map factorCount (List.range 1 1000))

type alias Model =
  { 
    doorSwitchCount : List Int
  }

type Msg
  = Calculate String

factorCount : Int -> Int
factorCount num =
  List.length (Arithmetic.primeFactors num)

addOpen : Int -> Int -> Int
addOpen bef num =
  num + (bef + 1) % 2

toIntOrNought: String -> Int
toIntOrNought number = 
  String.toInt number |> Result.toMaybe |> Maybe.withDefault 0

update : Msg -> Model -> Model
update msg model =
  case msg of
    Calculate num ->
      { model | doorSwitchCount = List.map factorCount (List.range 1 (toIntOrNought num)) }    

getAddOpenString : List Int -> String 
getAddOpenString doorSwitches =
  toString (List.foldl addOpen 0 doorSwitches)

getTotalString : List Int -> String
getTotalString doorSwitches = 
  toString (List.foldl (+) 0 doorSwitches)

maxDoorSwitch : List Int -> String
maxDoorSwitch doorSwitches = 
  List.maximum doorSwitches |> Maybe.withDefault 0 |> toString

view : Model -> Html Msg
view model =
  div []
    [ div[][ input[type_ "number", onInput Calculate, placeholder "doorCount" ][] ]
    , div[][ text("total doors: " ++ toString (List.length model.doorSwitchCount)) ]
    , div[][text("open doors: " ++ getAddOpenString model.doorSwitchCount)]
    , div[][text("highest number of door switches: " ++ maxDoorSwitch model.doorSwitchCount)]
    , div[][text("total number of door switches: " ++ getTotalString model.doorSwitchCount)]
    ]
