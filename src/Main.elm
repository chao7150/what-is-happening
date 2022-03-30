port module Main exposing (..)

import Audio exposing (Audio, AudioCmd, AudioData)
import Html exposing (Html, button, div, input, li, p, pre, text, time, ul)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Json.Encode
import Process
import Random
import Task
import Time



-- MAIN


main : Platform.Program () (Audio.Model Msg Model) (Audio.Msg Msg)
main =
    Audio.elementWithAudio { init = init, update = update, view = view, subscriptions = subscriptions, audio = audio, audioPort = { toJS = audioPortToJS, fromJS = audioPortFromJS } }



-- MODEL


type alias Sample =
    { time : Time.Posix, comment : String }


type SoundState
    = NotPlaying
    | Playing Time.Posix


type alias LoadedModel_ =
    { running : Bool
    , minTimeInput : String
    , maxTimeInput : String
    , startTimestamp : Time.Posix
    , upTimer : Int
    , durationMs : Int
    , samples : List Sample
    , zone : Time.Zone
    , sound : Audio.Source
    , soundState : SoundState
    }


type Model
    = LoadingModel
    | LoadedModel LoadedModel_
    | LoadFailedModel


port audioPortToJS : Json.Encode.Value -> Cmd msg


port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg


audio : AudioData -> Model -> Audio
audio _ model =
    case model of
        LoadedModel loadedModel ->
            case loadedModel.soundState of
                NotPlaying ->
                    Audio.silence

                Playing time ->
                    Audio.audio loadedModel.sound time

        _ ->
            Audio.silence


setSystemTime : Cmd Msg
setSystemTime =
    Task.perform SetSystemTime <| Time.here


delay : Float -> msg -> Cmd msg
delay time msg =
    Task.perform (\_ -> msg) (Process.sleep time)


init : () -> ( Model, Cmd Msg, AudioCmd Msg )
init _ =
    ( LoadingModel
    , Cmd.none
    , Audio.loadAudio SoundLoaded "./sin440.wav"
    )



-- UPDATE


type Msg
    = SoundLoaded (Result Audio.LoadError Audio.Source)
    | SetSystemTime Time.Zone
    | MinTimeInput String
    | MaxTimeInput String
    | SampleInput Time.Posix String
    | StartTimer
    | NewFace Int
    | GetStartTimestamp Time.Posix
    | StopTimer
    | Tick Time.Posix
    | PressedPlay
    | PressedPlayAndGotTime Time.Posix
    | StopAudio


update : AudioData -> Msg -> Model -> ( Model, Cmd Msg, AudioCmd Msg )
update _ msg model =
    case model of
        LoadingModel ->
            case msg of
                SoundLoaded result ->
                    case result of
                        Ok sound ->
                            ( LoadedModel { running = False, minTimeInput = "", maxTimeInput = "", startTimestamp = Time.millisToPosix 0, upTimer = 0, durationMs = 0, samples = [], zone = Time.utc, sound = sound, soundState = NotPlaying }, setSystemTime, Audio.cmdNone )

                        Err _ ->
                            ( LoadFailedModel, Cmd.none, Audio.cmdNone )

                _ ->
                    ( model, Cmd.none, Audio.cmdNone )

        LoadFailedModel ->
            ( model, Cmd.none, Audio.cmdNone )

        LoadedModel loadedModel ->
            case msg of
                SetSystemTime zone ->
                    ( LoadedModel { loadedModel | zone = zone }, Cmd.none, Audio.cmdNone )

                MinTimeInput input ->
                    ( LoadedModel { loadedModel | minTimeInput = input }, Cmd.none, Audio.cmdNone )

                MaxTimeInput input ->
                    ( LoadedModel { loadedModel | maxTimeInput = input }, Cmd.none, Audio.cmdNone )

                SampleInput time input ->
                    ( LoadedModel
                        { loadedModel
                            | samples =
                                List.map
                                    (\sample ->
                                        if sample.time == time then
                                            { sample | comment = input }

                                        else
                                            sample
                                    )
                                    loadedModel.samples
                        }
                    , Cmd.none
                    , Audio.cmdNone
                    )

                StartTimer ->
                    case String.toInt loadedModel.minTimeInput of
                        Just min ->
                            case String.toInt loadedModel.maxTimeInput of
                                Just max ->
                                    ( LoadedModel loadedModel, Random.generate NewFace (Random.int min max), Audio.cmdNone )

                                Nothing ->
                                    ( LoadedModel loadedModel, Cmd.none, Audio.cmdNone )

                        Nothing ->
                            ( LoadedModel loadedModel, Cmd.none, Audio.cmdNone )

                NewFace newFace ->
                    ( LoadedModel { loadedModel | upTimer = 0, durationMs = newFace * 1000 }, Task.perform GetStartTimestamp Time.now, Audio.cmdNone )

                GetStartTimestamp timestamp ->
                    ( LoadedModel { loadedModel | running = True, startTimestamp = timestamp }, Cmd.none, Audio.cmdNone )

                StopTimer ->
                    ( LoadedModel { loadedModel | running = False }, Cmd.none, Audio.cmdNone )

                PressedPlay ->
                    ( LoadedModel loadedModel, Task.perform PressedPlayAndGotTime Time.now, Audio.cmdNone )

                PressedPlayAndGotTime time ->
                    ( LoadedModel { loadedModel | soundState = Playing time }, delay 3000 StopAudio, Audio.cmdNone )

                StopAudio ->
                    ( LoadedModel { loadedModel | soundState = NotPlaying }, Cmd.none, Audio.cmdNone )

                Tick now ->
                    if loadedModel.running then
                        if Time.posixToMillis now - Time.posixToMillis loadedModel.startTimestamp < loadedModel.durationMs then
                            ( LoadedModel { loadedModel | upTimer = (Time.posixToMillis now - Time.posixToMillis loadedModel.startTimestamp) // 1000 }, Cmd.none, Audio.cmdNone )

                        else
                            ( LoadedModel { loadedModel | running = False, samples = List.append loadedModel.samples [ { time = now, comment = "" } ] }, Task.perform PressedPlayAndGotTime Time.now, Audio.cmdNone )

                    else
                        ( model, Cmd.none, Audio.cmdNone )

                -- unreachable
                SoundLoaded _ ->
                    ( model, Cmd.none, Audio.cmdNone )



-- SUBSCRIPTIONS


subscriptions : AudioData -> Model -> Sub Msg
subscriptions _ model =
    Time.every 1000 Tick



-- VIEW


view : AudioData -> Model -> Html Msg
view _ model =
    case model of
        LoadingModel ->
            p [] [ text "loading" ]

        LoadFailedModel ->
            p [] [ text "load failed" ]

        LoadedModel loadedModel ->
            div []
                [ p [] [ text "minとmaxに設定した秒数の間でランダムなタイミングで音が鳴ります。鳴った時刻をリストで記録します。" ]
                , p []
                    [ text
                        (if loadedModel.running then
                            "計測中"

                         else
                            "停止中"
                        )
                    ]
                , p [] [ text (String.fromInt loadedModel.upTimer) ]
                , input [ value loadedModel.minTimeInput, placeholder "min", onInput MinTimeInput ] []
                , input [ value loadedModel.maxTimeInput, placeholder "max", onInput MaxTimeInput ] []
                , if not loadedModel.running then
                    startButton loadedModel.minTimeInput loadedModel.maxTimeInput

                  else
                    stopButton
                , sampleList loadedModel.samples
                    loadedModel.zone
                , pre
                    []
                    [ text <| String.join "\n" <| List.map (\sample -> formatTime sample.time loadedModel.zone ++ "," ++ sample.comment) loadedModel.samples ]
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


sampleList : List Sample -> Time.Zone -> Html Msg
sampleList samples zone =
    ul [] (List.map (\sample -> li [] [ time [] [ text <| formatTime sample.time zone ], input [ placeholder "What is happening?", value sample.comment, onInput (SampleInput sample.time) ] [] ]) samples)


formatTime : Time.Posix -> Time.Zone -> String
formatTime posix zone =
    (String.padLeft 2 '0' <| String.fromInt <| Time.toHour zone posix) ++ ":" ++ (String.padLeft 2 '0' <| String.fromInt <| Time.toMinute zone posix) ++ ":" ++ (String.padLeft 2 '0' <| String.fromInt <| Time.toSecond zone posix)
