module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Process
import Task



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { running : Bool
    , timeInput : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { running = False
      , timeInput = "60"
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = TimeInput String
    | StartTimer


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeInput input ->
            ( { model | timeInput = input }, Cmd.none )

        StartTimer ->
            ( model, delay 5000 <| TimeInput "4" )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text "工事中 startを押すと5秒後に4になる" ]
        , input [ value model.timeInput, onInput TimeInput ] []
        , startButton model.timeInput
        ]


startButton : String -> Html Msg
startButton timeInput =
    case String.toInt timeInput of
        Just _ ->
            button [ onClick StartTimer ] [ text "start" ]

        Nothing ->
            text ""
