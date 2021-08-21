module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { running : Bool
    , timeInput : String
    }


init : Model
init =
    { running = False
    , timeInput = "60"
    }



-- UPDATE


type Msg
    = TimeInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        TimeInput input ->
            { model | timeInput = input }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ value model.timeInput, onInput TimeInput ] []
        , startButton model.timeInput
        ]


startButton : String -> Html Msg
startButton timeInput =
    case String.toInt timeInput of
        Just _ ->
            button [] [ text "start" ]

        Nothing ->
            text ""
