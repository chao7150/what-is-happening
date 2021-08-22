module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Process
import Random
import Task
import Time



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { running : Bool
    , minTimeInput : String
    , maxTimeInput : String
    , remainingTimer : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { running = False
      , minTimeInput = ""
      , maxTimeInput = ""
      , remainingTimer = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = MinTimeInput String
    | MaxTimeInput String
    | StartTimer
    | NewFace Int
    | StopTimer
    | Tick Time.Posix


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MinTimeInput input ->
            ( { model | minTimeInput = input }, Cmd.none )

        MaxTimeInput input ->
            ( { model | maxTimeInput = input }, Cmd.none )

        StartTimer ->
            case String.toInt model.minTimeInput of
                Just min ->
                    case String.toInt model.maxTimeInput of
                        Just max ->
                            ( model, Random.generate NewFace (Random.int min max) )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NewFace newFace ->
            ( { model | running = True, remainingTimer = newFace }, Cmd.none )

        StopTimer ->
            ( { model | running = False }, Cmd.none )

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
        [ p []
            [ text
                (if model.running then
                    "計測中"

                 else
                    "停止中"
                )
            ]
        , input [ value model.minTimeInput, placeholder "min", onInput MinTimeInput ] []
        , input [ value model.maxTimeInput, placeholder "max", onInput MaxTimeInput ] []
        , if not model.running then
            startButton model.minTimeInput model.maxTimeInput

          else
            stopButton
        ]


startButton : String -> String -> Html Msg
startButton min max =
    case String.toInt min of
        Just _ ->
            case String.toInt max of
                Just _ ->
                    button [ onClick StartTimer ] [ text "start" ]

                Nothing ->
                    text ""

        Nothing ->
            text ""


stopButton : Html Msg
stopButton =
    button [ onClick StopTimer ] [ text "stop" ]
