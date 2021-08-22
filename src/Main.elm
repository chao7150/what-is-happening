module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, p, ruby, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Process
import Task
import Time



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { running : Bool
    , timeInput : String
    , remainingTimer : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { running = False
      , timeInput = "60"
      , remainingTimer = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = TimeInput String
    | StartTimer
    | Tick Time.Posix


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
            case String.toInt model.timeInput of
                Just int ->
                    ( { model | running = True, remainingTimer = int }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Tick _ ->
            if model.running then
                if model.remainingTimer > 0 then
                    ( { model | remainingTimer = model.remainingTimer - 1 }, Cmd.none )

                else
                    ( { model | running = False }, Cmd.none )

            else
                ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text "工事中 startを押すと指定秒数後に停止する" ]
        , p []
            [ text
                (if model.running then
                    "計測中"

                 else
                    "停止中"
                )
            ]
        , input [ value model.timeInput, onInput TimeInput ] []
        , if not model.running then
            startButton model.timeInput

          else
            text ""
        ]


startButton : String -> Html Msg
startButton timeInput =
    case String.toInt timeInput of
        Just _ ->
            button [ onClick StartTimer ] [ text "start" ]

        Nothing ->
            text ""
