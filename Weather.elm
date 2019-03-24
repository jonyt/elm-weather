module Weather exposing (..)

import Browser exposing (sandbox)
import Html exposing (..)
import Html.Attributes exposing (href, placeholder, value)
import Html.Events.Extra exposing (onEnter)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (string, int, list, Decoder)

type alias Model = {
    textInput : String
    , allInputs : List String
  }
type Msg = Change String |
  EnterPressed |
  Add 

init : Model
init = {
    textInput = ""
    , allInputs = [] 
  }

view : Model -> Html Msg
view model =
    div []
        [ 
          listView model
          , inputView model
          , buttonView model
          , validationView model
        ]

listView : Model -> Html Msg
listView model = 
  ul []
    (List.map (\e -> li [] [ text e ]) model.allInputs)
    
inputView : Model -> Html Msg
inputView model = 
  input [ placeholder "Enter text", onInput Change, onEnter EnterPressed, value model.textInput ] []


buttonView : Model -> Html Msg
buttonView model = 
  button [ onClick Add ] [ text "Get data from server" ]

validationView : Model -> Html Msg
validationView model =
  if String.isEmpty <| String.trim model.textInput then
    div [] [ text "Cannot submit an empty string" ]
  else
    text ""

onSubmit : Model -> Model
onSubmit model = 
  if not (String.isEmpty <| String.trim model.textInput) then
    { model | allInputs = model.textInput :: model.allInputs, textInput = "" }
  else
    model

update : Msg -> Model -> Model
update msg model = 
  case msg of 
    Add -> 
      onSubmit model
    EnterPressed ->
      onSubmit model 
    Change content -> 
      { model | textInput = content }

main =
    sandbox
        { init = init 
        , view = view
        , update = update
        }
