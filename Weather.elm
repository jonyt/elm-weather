module Weather exposing (..)

import Browser exposing (element)
import Html exposing (..)
import Html.Attributes exposing (href, placeholder, value)
import Html.Events.Extra exposing (onEnter)
import Html.Events exposing (onClick, onInput)
import Http
import Url.Builder exposing (crossOrigin, string)
import Json.Decode exposing (string, Decoder, field, at)
import Regex exposing (..)

type alias Model = {
    textInput : String
    , allInputs : List String
    , error : String
  }
type Msg = Change String |
  EnterPressed |
  Add |
  GotWeather (Result Http.Error String)

init : Flags -> (Model, Cmd Msg)
init flags = ({
    textInput = ""
    , allInputs = []
    , error = ""
  }, Cmd.none)

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
  div [] [ text model.error ]

onSubmit : Model -> (Model, Cmd Msg)
onSubmit model =
  if (String.isEmpty <| String.trim <| model.textInput) then
    ({model | error = "Cannot submit empty string"}, Cmd.none)
  else if (not <| isCoordinates model.textInput) then
    ({model | error = "Can only submit coordinates, e.g. 1.456,2.123"}, Cmd.none)
  else
    (model, getWeather model.textInput)

isCoordinates : String -> Bool
isCoordinates text =
  text |> Regex.contains (Maybe.withDefault Regex.never (Regex.fromString "^\\d+\\.\\d+,\\d+\\.\\d+$"))

getWeather : String -> Cmd Msg
getWeather coordinates =
  Http.get
    { url = weatherUrl coordinates
    , expect = Http.expectJson GotWeather currentWeatherSummary
    }

weatherUrl : String -> String
weatherUrl coordinates =
  Url.Builder.crossOrigin "https://api.darksky.net" ["forecast", "7c107880c337f12942675ef11a4afa81", coordinates] []

currentWeatherSummary : Decoder String
currentWeatherSummary =
  field "currently" (field "summary" string)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Add ->
      onSubmit model
    EnterPressed ->
      onSubmit model
    Change content ->
      ({ model | textInput = content, error = "" }, Cmd.none)
    GotWeather result ->
      case result of
        Ok weatherSummary ->
          ({ model | allInputs = weatherSummary :: model.allInputs, textInput = "" }, Cmd.none)
        Err httpError ->
          (model, Cmd.none)

-- TODO: delete this
type alias Flags = {}

main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
