module PentaManager exposing (..)

import Array exposing (..)
import Browser
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Task
import Time


type RacePhase
    = ShortDistanceRunning
    | Shooting
    | Running


type LaserRunPhase
    = NotStarted
    | Race RacePhase
    | Finished


type TargetAttempt
    = Success Time.Posix
    | Fail Time.Posix


type alias TargetSummary =
    { startTime : Maybe Time.Posix
    , attempts : List TargetAttempt
    }


type alias RunTime =
    Time.Posix


type alias ShortDistanceTime =
    Time.Posix


type alias SeriesSummary =
    { shortDistanceTime : Maybe ShortDistanceTime
    , shootSummary : Array TargetSummary
    , runTime : Maybe RunTime
    }


type SeriesId
    = SeriesId Int


type TargetId
    = TargetId Int


type alias Model =
    { zone : Time.Zone
    , currentTime : Time.Posix
    , currentSeriesStartTime : Maybe Time.Posix
    , currentPhaseStartTime : Maybe Time.Posix
    , currentSeries : Maybe SeriesId
    , currentPhase : LaserRunPhase
    , currentTarget : Maybe TargetId
    , startTime : Maybe Time.Posix
    , finishTime : Maybe Time.Posix
    , numberOfTargets : Int
    , numberOfSeries : Int
    , maxTimeShootingInSeconds : Int
    , fullSummary : Array SeriesSummary
    }


initializeATargetSummary =
    { startTime = Nothing
    , attempts = []
    }


initializeAShootSummary : Int -> Array TargetSummary
initializeAShootSummary numberOfTargets =
    let
        aTargetSummary =
            initializeATargetSummary
    in
    Array.initialize numberOfTargets (always aTargetSummary)


initializeASeriesSummary : Int -> SeriesSummary
initializeASeriesSummary numberOfTargets =
    { shortDistanceTime = Nothing
    , shootSummary = initializeAShootSummary numberOfTargets
    , runTime = Nothing
    }


initializeFullSummary : Int -> Int -> Array SeriesSummary
initializeFullSummary numberOfSeries numberOfTargets =
    let
        aSeriesSummary =
            initializeASeriesSummary numberOfTargets
    in
    Array.initialize numberOfSeries (always aSeriesSummary)


initializeModel : Int -> Int -> Int -> Model
initializeModel theNumberOfSeries theNumberOfTargets maxTimeShootingInSeconds =
    { zone = Time.utc
    , currentTime = Time.millisToPosix 0
    , currentSeriesStartTime = Nothing
    , currentPhaseStartTime = Nothing
    , currentSeries = Nothing
    , currentPhase = NotStarted
    , currentTarget = Nothing
    , startTime = Nothing
    , finishTime = Nothing
    , numberOfTargets = theNumberOfTargets
    , numberOfSeries = theNumberOfSeries
    , fullSummary = initializeFullSummary theNumberOfSeries theNumberOfTargets
    , maxTimeShootingInSeconds = maxTimeShootingInSeconds
    }


initialModel : Model
initialModel =
    initializeModel 3 5 50


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.none
    )



--UPDATE


type Msg
    = Tick Time.Posix
    | MainButtonClicked
    | SecondaryButtonClicked
    | Restart


stillShooting : Time.Posix -> Time.Posix -> Int -> Int -> Int -> TargetAttempt -> Bool
stillShooting shootingStartTime currentTime maxTimeShootingInSeconds numberOftargets currentTargetNumber currentTargetAttempt =
    let
        isNotLastTarget =
            currentTargetNumber < numberOftargets

        ( attemptFailed, attemptTime ) =
            case currentTargetAttempt of
                Success currentAttemptTime ->
                    ( False, currentAttemptTime )

                Fail currentAttemptTime ->
                    ( True, currentAttemptTime )

        arePendingTargets =
            isNotLastTarget || attemptFailed

        maxTimeShootingInMillis =
            maxTimeShootingInSeconds * 1000

        timeSpentShooting =
            deltaTimeBetween attemptTime shootingStartTime

        timeNotConsumed =
            Time.posixToMillis timeSpentShooting < maxTimeShootingInMillis
    in
    arePendingTargets && timeNotConsumed


grabCurrentTimeFrom : Model -> Time.Posix
grabCurrentTimeFrom model =
    model.currentTime


processMainButton : Model -> Model
processMainButton model =
    case model.currentPhase of
        Finished ->
            model

        NotStarted ->
            { model
                | startTime = Just (grabCurrentTimeFrom model)
                , currentSeriesStartTime = Just (grabCurrentTimeFrom model)
                , currentPhaseStartTime = Just (grabCurrentTimeFrom model)
                , currentPhase = Race ShortDistanceRunning
                , currentSeries = Just (SeriesId 1)
            }

        Race racePhase ->
            case model.currentSeries of
                Nothing ->
                    Debug.log "Application corrupted. Current series is not known in race phase when green button is clicked. Application will be finished!"
                        initialModel

                Just (SeriesId currentSeries) ->
                    let
                        seriesSummary =
                            Array.get (currentSeries - 1) model.fullSummary
                    in
                    case seriesSummary of
                        Nothing ->
                            Debug.log "Application corrupted. Summary of Series has not been properly initialized when green button is clicked. Application will be finished!"
                                initialModel

                        Just currentSeriesSummary ->
                            case racePhase of
                                ShortDistanceRunning ->
                                    let
                                        currentTarget =
                                            1

                                        newTargetSummary =
                                            { startTime = Just (grabCurrentTimeFrom model)
                                            , attempts = Success (grabCurrentTimeFrom model) :: []
                                            }

                                        newNextTargetSummary =
                                            { startTime = Just (grabCurrentTimeFrom model)
                                            , attempts = []
                                            }

                                        newShootSummary =
                                            Array.set currentTarget newNextTargetSummary currentSeriesSummary.shootSummary

                                        newSeriesSummary =
                                            { currentSeriesSummary
                                                | shortDistanceTime = Just (grabCurrentTimeFrom model)
                                                , shootSummary = Array.set (currentTarget - 1) newTargetSummary newShootSummary
                                            }
                                    in
                                    { model
                                        | fullSummary = Array.set (currentSeries - 1) newSeriesSummary model.fullSummary
                                        , currentTarget = Just (TargetId (currentTarget + 1))
                                        , currentPhaseStartTime = Just (grabCurrentTimeFrom model)
                                        , currentPhase = Race Shooting
                                    }

                                Shooting ->
                                    case model.currentPhaseStartTime of
                                        Nothing ->
                                            Debug.log "Application corrupted. Current phase start time is not known in shooting phase when green button is clicked. Application will be finished!"
                                                initialModel

                                        Just currentPhaseStartTime ->
                                            case model.currentTarget of
                                                Nothing ->
                                                    Debug.log "Application corrupted. Current target is not known in shooting phase when green button is clicked. Application will be finished!"
                                                        initialModel

                                                Just (TargetId currentTarget) ->
                                                    let
                                                        targetSummary =
                                                            Array.get (currentTarget - 1) currentSeriesSummary.shootSummary

                                                        nextTargetSummary =
                                                            if currentTarget < model.numberOfTargets then
                                                                Array.get currentTarget currentSeriesSummary.shootSummary

                                                            else
                                                                Nothing
                                                    in
                                                    case targetSummary of
                                                        Nothing ->
                                                            Debug.log "Application corrupted. Current target summary is not known in shooting phase when green button is clicked. Application will be finished!"
                                                                initialModel

                                                        Just currentTargetSummary ->
                                                            let
                                                                newTargetAttempt =
                                                                    Success (grabCurrentTimeFrom model)

                                                                newTargetSummary =
                                                                    case currentTargetSummary.startTime of
                                                                        Nothing ->
                                                                            Debug.log "Application corrupted. Start time of current target summary has not been properly initialized in shooting phase when green button is clicked."
                                                                                currentTargetSummary

                                                                        Just _ ->
                                                                            { currentTargetSummary
                                                                                | attempts = newTargetAttempt :: currentTargetSummary.attempts
                                                                            }

                                                                newNextTargetSummary =
                                                                    case nextTargetSummary of
                                                                        -- Nothing branch will never be used
                                                                        Nothing ->
                                                                            initializeATargetSummary

                                                                        Just theNextTargetSummary ->
                                                                            { theNextTargetSummary
                                                                                | startTime = Just (grabCurrentTimeFrom model)
                                                                            }

                                                                newSeriesSummary =
                                                                    case nextTargetSummary of
                                                                        Nothing ->
                                                                            { currentSeriesSummary
                                                                                | shootSummary = Array.set (currentTarget - 1) newTargetSummary currentSeriesSummary.shootSummary
                                                                            }

                                                                        Just _ ->
                                                                            let
                                                                                newShootSummary =
                                                                                    Array.set currentTarget newNextTargetSummary currentSeriesSummary.shootSummary
                                                                            in
                                                                            { currentSeriesSummary
                                                                                | shootSummary = Array.set (currentTarget - 1) newTargetSummary newShootSummary
                                                                            }
                                                            in
                                                            if stillShooting currentPhaseStartTime model.currentTime model.maxTimeShootingInSeconds model.numberOfTargets currentTarget newTargetAttempt then
                                                                { model
                                                                    | fullSummary = Array.set (currentSeries - 1) newSeriesSummary model.fullSummary
                                                                    , currentTarget = Just (TargetId (currentTarget + 1))
                                                                }

                                                            else
                                                                { model
                                                                    | fullSummary = Array.set (currentSeries - 1) newSeriesSummary model.fullSummary
                                                                    , currentTarget = Nothing
                                                                    , currentPhaseStartTime = Just (grabCurrentTimeFrom model)
                                                                    , currentPhase = Race Running
                                                                }

                                Running ->
                                    let
                                        newSeriesSummary =
                                            { currentSeriesSummary | runTime = Just (grabCurrentTimeFrom model) }

                                        raceNotFinished =
                                            currentSeries < model.numberOfSeries
                                    in
                                    if raceNotFinished then
                                        { model
                                            | fullSummary = Array.set (currentSeries - 1) newSeriesSummary model.fullSummary
                                            , currentSeries = Just (SeriesId (currentSeries + 1))
                                            , currentSeriesStartTime = Just (grabCurrentTimeFrom model)
                                            , currentPhaseStartTime = Just (grabCurrentTimeFrom model)
                                            , currentPhase = Race ShortDistanceRunning
                                        }

                                    else
                                        Debug.log "INFO: Final model: "
                                            { model
                                                | fullSummary = Array.set (currentSeries - 1) newSeriesSummary model.fullSummary
                                                , currentSeries = Nothing

                                                --, currentSeriesStartTime = Nothing
                                                --, currentPhaseStartTime = Nothing
                                                , currentPhase = Finished
                                                , finishTime = Just (grabCurrentTimeFrom model)
                                            }


processSecondaryButton : Model -> Model
processSecondaryButton model =
    case model.currentPhase of
        NotStarted ->
            model

        Finished ->
            model

        Race racePhase ->
            case model.currentSeries of
                Nothing ->
                    Debug.log "Application corrupted. Current series is not known in race phase when red button is clicked. Application will be finished!"
                        initialModel

                Just (SeriesId currentSeries) ->
                    let
                        seriesSummary =
                            Array.get (currentSeries - 1) model.fullSummary
                    in
                    case seriesSummary of
                        Nothing ->
                            Debug.log "Application corrupted. Summary of Series has not been properly initialized when red button is clicked. Application will be finished!"
                                initialModel

                        Just currentSeriesSummary ->
                            case racePhase of
                                Running ->
                                    model

                                ShortDistanceRunning ->
                                    let
                                        currentTarget =
                                            1

                                        newTargetSummary =
                                            { startTime = Just (grabCurrentTimeFrom model)
                                            , attempts = Fail (grabCurrentTimeFrom model) :: []
                                            }

                                        newSeriesSummary =
                                            { currentSeriesSummary
                                                | shortDistanceTime = Just (grabCurrentTimeFrom model)
                                                , shootSummary = Array.set (currentTarget - 1) newTargetSummary currentSeriesSummary.shootSummary
                                            }
                                    in
                                    { model
                                        | fullSummary = Array.set (currentSeries - 1) newSeriesSummary model.fullSummary
                                        , currentTarget = Just (TargetId currentTarget)
                                        , currentPhaseStartTime = Just (grabCurrentTimeFrom model)
                                        , currentPhase = Race Shooting
                                    }

                                Shooting ->
                                    case model.currentPhaseStartTime of
                                        Nothing ->
                                            Debug.log "Application corrupted. Current phase start time is not known in shooting phase when red button is clicked. Application will be finished!"
                                                initialModel

                                        Just currentPhaseStartTime ->
                                            case model.currentTarget of
                                                Nothing ->
                                                    Debug.log "Application corrupted. Current target is not known in shooting phase when red button is clicked. Application will be finished!"
                                                        initialModel

                                                Just (TargetId currentTarget) ->
                                                    let
                                                        targetSummary =
                                                            Array.get (currentTarget - 1) currentSeriesSummary.shootSummary
                                                    in
                                                    case targetSummary of
                                                        Nothing ->
                                                            Debug.log "Application corrupted. Current target summary is not known in shooting phase when red button is clicked. Application will be finished!"
                                                                initialModel

                                                        Just currentTargetSummary ->
                                                            let
                                                                newTargetAttempt =
                                                                    Fail (grabCurrentTimeFrom model)

                                                                newTargetSummary =
                                                                    case currentTargetSummary.startTime of
                                                                        Nothing ->
                                                                            { currentTargetSummary
                                                                                | startTime = Just (grabCurrentTimeFrom model)
                                                                                , attempts = newTargetAttempt :: currentTargetSummary.attempts
                                                                            }

                                                                        Just _ ->
                                                                            { currentTargetSummary
                                                                                | attempts = newTargetAttempt :: currentTargetSummary.attempts
                                                                            }

                                                                newSeriesSummary =
                                                                    { currentSeriesSummary
                                                                        | shootSummary = Array.set (currentTarget - 1) newTargetSummary currentSeriesSummary.shootSummary
                                                                    }
                                                            in
                                                            if stillShooting currentPhaseStartTime model.currentTime model.maxTimeShootingInSeconds model.numberOfTargets currentTarget newTargetAttempt then
                                                                { model
                                                                    | fullSummary = Array.set (currentSeries - 1) newSeriesSummary model.fullSummary
                                                                }

                                                            else
                                                                { model
                                                                    | fullSummary = Array.set (currentSeries - 1) newSeriesSummary model.fullSummary
                                                                    , currentTarget = Nothing
                                                                    , currentPhaseStartTime = Just (grabCurrentTimeFrom model)
                                                                    , currentPhase = Race Running
                                                                }


restartCounters : Model -> Model
restartCounters model =
    initialModel


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Tick newTime ->
            let
                sameModel =
                    ( model.currentPhase, model.currentTarget, model.currentPhaseStartTime )

                ( nextPhase, nextTarget, nextPhaseStartTime ) =
                    case model.currentPhase of
                        Race currentRacePhase ->
                            case currentRacePhase of
                                Shooting ->
                                    let
                                        (SeriesId currentSeries) =
                                            Maybe.withDefault (SeriesId 1) model.currentSeries

                                        currentSeriesSummary =
                                            Array.get (currentSeries - 1) model.fullSummary
                                    in
                                    case currentSeriesSummary of
                                        Nothing ->
                                            Debug.log "Application corrupted. In Shooting phase current series summary is not properly set." sameModel

                                        Just seriesSummary ->
                                            let
                                                firstTargetIndex =
                                                    1

                                                firstTargetSummary =
                                                    Array.get (firstTargetIndex - 1) seriesSummary.shootSummary
                                            in
                                            case firstTargetSummary of
                                                Nothing ->
                                                    sameModel

                                                Just targetSummary ->
                                                    case targetSummary.startTime of
                                                        Nothing ->
                                                            Debug.log "Application corrupted. In Shooting phase first target summary is not properly set." sameModel

                                                        Just currentTargetTime ->
                                                            let
                                                                timeSpentShooting =
                                                                    deltaTimeBetween model.currentTime currentTargetTime

                                                                maxTimeShootingInMillis =
                                                                    model.maxTimeShootingInSeconds * 1000

                                                                timeNotConsumed =
                                                                    Time.posixToMillis timeSpentShooting < maxTimeShootingInMillis
                                                            in
                                                            if timeNotConsumed then
                                                                sameModel

                                                            else
                                                                ( Race Running, Nothing, Just (grabCurrentTimeFrom model) )

                                _ ->
                                    sameModel

                        _ ->
                            sameModel
            in
            ( { model
                | currentTime = newTime
                , currentPhase = nextPhase
                , currentTarget = nextTarget
                , currentPhaseStartTime = nextPhaseStartTime
              }
            , Cmd.none
            )

        MainButtonClicked ->
            ( processMainButton model
            , Cmd.none
            )

        SecondaryButtonClicked ->
            ( processSecondaryButton model
            , Cmd.none
            )

        Restart ->
            ( restartCounters model
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1 Tick


timetoString : Bool -> Bool -> Bool -> Bool -> Time.Zone -> Time.Posix -> String
timetoString includeHour includeMinute includeSecond includeMillisecond zone time =
    let
        hour =
            if includeHour then
                time
                    |> Time.toHour zone
                    |> String.fromInt
                    |> String.padLeft 2 '0'
                    |> (++) ":"

            else
                ""

        minute =
            if includeMinute then
                (time
                    |> Time.toMinute zone
                    |> String.fromInt
                    |> String.padLeft 2 '0'
                )
                    ++ ":"

            else
                ""

        second =
            if includeSecond then
                (time
                    |> Time.toSecond zone
                    |> String.fromInt
                    |> String.padLeft 2 '0'
                )
                    ++ ":"

            else
                ""

        millisecond =
            if includeMillisecond then
                time
                    |> Time.toMillis zone
                    |> String.fromInt
                    |> String.padLeft 3 '0'

            else
                ""
    in
    String.concat [ hour, minute, second, millisecond ]


viewSVGTimer : String -> String -> String -> Time.Zone -> Time.Posix -> List (Svg Msg)
viewSVGTimer cssClass pos_x pos_y zone time =
    let
        timeText =
            timetoString False True True True zone time
    in
    [ text_ [ x pos_x, y pos_y, Svg.Attributes.class cssClass ]
        [ Svg.text timeText
        ]
    ]


deltaTimeBetween : Time.Posix -> Time.Posix -> Time.Posix
deltaTimeBetween now previous =
    let
        nowInt =
            Time.posixToMillis now

        previousInt =
            Time.posixToMillis previous

        deltaInt =
            nowInt - previousInt

        delta =
            Time.millisToPosix deltaInt
    in
    delta


currentTimeSVGView : String -> String -> String -> Model -> List (Svg Msg)
currentTimeSVGView cssClass pos_x pos_y model =
    case model.currentPhase of
        NotStarted ->
            viewSVGTimer cssClass pos_x pos_y model.zone (Time.millisToPosix 0)

        Finished ->
            let
                theFinishTime =
                    case model.finishTime of
                        Nothing ->
                            Debug.log "Application corrupted. Finish time is not known when state is Finished." (Time.millisToPosix 0)

                        Just finishTime ->
                            finishTime

                theStartTime =
                    case model.startTime of
                        Nothing ->
                            Debug.log "Application corrupted. Start time is not known when state is Finished." (Time.millisToPosix 0)

                        Just startTime ->
                            startTime
            in
            viewSVGTimer cssClass pos_x pos_y model.zone (deltaTimeBetween theFinishTime theStartTime)

        _ ->
            let
                theStartTime =
                    case model.startTime of
                        Nothing ->
                            Debug.log "Application corrupted. Start time is not known when state is other than Not Started or Finished." (Time.millisToPosix 0)

                        Just startTime ->
                            startTime
            in
            viewSVGTimer cssClass pos_x pos_y model.zone (deltaTimeBetween model.currentTime theStartTime)


currentSeriesTimeSVGView : String -> String -> String -> Model -> List (Svg Msg)
currentSeriesTimeSVGView cssClass pos_x pos_y model =
    case model.currentPhase of
        NotStarted ->
            viewSVGTimer cssClass pos_x pos_y model.zone (Time.millisToPosix 0)

        Finished ->
            let
                theFinishTime =
                    case model.finishTime of
                        Nothing ->
                            Debug.log "Application corrupted. Finish time is not known when state is Finished." (Time.millisToPosix 0)

                        Just finishTime ->
                            finishTime

                theCurrentSeriesStartTime =
                    case model.currentSeriesStartTime of
                        Nothing ->
                            Debug.log "Application corrupted. Current series start time time is not known when state is Finished." (Time.millisToPosix 0)

                        Just currentSeriesStartTime ->
                            currentSeriesStartTime
            in
            viewSVGTimer cssClass pos_x pos_y model.zone (deltaTimeBetween theFinishTime theCurrentSeriesStartTime)

        _ ->
            let
                theCurrentSeriesStartTime =
                    case model.currentSeriesStartTime of
                        Nothing ->
                            Debug.log "Application corrupted. Current series start time time is not known when state is other than Not Started or Finished." (Time.millisToPosix 0)

                        Just currentSeriesStartTime ->
                            currentSeriesStartTime
            in
            viewSVGTimer cssClass pos_x pos_y model.zone (deltaTimeBetween model.currentTime theCurrentSeriesStartTime)


currentPhaseTimeSVGView : String -> String -> String -> Model -> List (Svg Msg)
currentPhaseTimeSVGView cssClass pos_x pos_y model =
    case model.currentPhase of
        NotStarted ->
            viewSVGTimer cssClass pos_x pos_y model.zone (Time.millisToPosix 0)

        Finished ->
            let
                theFinishTime =
                    case model.finishTime of
                        Nothing ->
                            Debug.log "Application corrupted. Finish time is not known when state is Finished." (Time.millisToPosix 0)

                        Just finishTime ->
                            finishTime

                theCurrentPhaseStartTime =
                    case model.currentPhaseStartTime of
                        Nothing ->
                            Debug.log "Application corrupted. Current phase start time time is not known when state is Finished." (Time.millisToPosix 0)

                        Just currentPhaseStartTime ->
                            currentPhaseStartTime
            in
            viewSVGTimer cssClass pos_x pos_y model.zone (deltaTimeBetween theFinishTime theCurrentPhaseStartTime)

        _ ->
            let
                theCurrentPhaseStartTime =
                    case model.currentPhaseStartTime of
                        Nothing ->
                            Debug.log "Application corrupted. Current phase start time time is not known when state is other than Not Started or Finished." (Time.millisToPosix 0)

                        Just currentPhaseStartTime ->
                            currentPhaseStartTime
            in
            viewSVGTimer cssClass pos_x pos_y model.zone (deltaTimeBetween model.currentTime theCurrentPhaseStartTime)


shootingButtonsSVGView : Int -> Int -> Int -> Int -> List (Svg Msg)
shootingButtonsSVGView width y_coordinate hitButton_radius missButton_radius =
    let
        right_padding =
            0.1

        top_padding =
            0.7

        missButton_diameter =
            missButton_radius * 2

        missButton_size =
            toFloat missButton_diameter + toFloat missButton_diameter * top_padding

        missButton_center_y =
            toFloat y_coordinate + missButton_size / 2

        missButton_center_x =
            toFloat width - (missButton_size * right_padding) - toFloat missButton_radius

        hitButton_diameter =
            missButton_radius * 2

        hitButton_size =
            toFloat hitButton_diameter + toFloat missButton_diameter * top_padding

        hitButton_center_y =
            --(toFloat y_coordinate + missButton_size) + hitButton_size / 2
            toFloat y_coordinate + missButton_size + toFloat hitButton_radius

        hitButton_center_x =
            toFloat width - (hitButton_size * right_padding) - toFloat hitButton_radius
    in
    [ circle
        [ cx (String.fromFloat missButton_center_x)
        , cy (String.fromFloat missButton_center_y)
        , r (String.fromInt missButton_radius)
        , Svg.Attributes.class "secondaryButton"
        , Svg.Events.onClick SecondaryButtonClicked
        ]
        []
    ]
        ++ [ circle
                [ cx (String.fromFloat hitButton_center_x)
                , cy (String.fromFloat hitButton_center_y)
                , r (String.fromInt hitButton_radius)
                , Svg.Attributes.class "mainButton"
                , Svg.Events.onClick MainButtonClicked
                ]
                []
           ]


targetView : TargetSummary -> Int -> Maybe TargetId -> Int -> Int -> Float -> Time.Zone -> Time.Posix -> List (Svg Msg)
targetView targetSummary targetNumber currentTarget radius center_y center_x zone currentTime =
    let
        target_center_x =
            String.fromFloat center_x

        target_center_y =
            String.fromInt center_y

        target_radius =
            String.fromInt radius

        shootIntentPadding =
            0.1

        shootIntentTime_center_x =
            target_center_x

        shootIntentTime_center_y =
            String.fromFloat ((toFloat center_y + toFloat radius) + ((toFloat center_y + toFloat radius) * shootIntentPadding))
    in
    case currentTarget of
        Nothing ->
            case targetSummary.startTime of
                Nothing ->
                    [ circle [ cx target_center_x, cy target_center_y, r target_radius, Svg.Attributes.class "targetCircleGrey" ] [] ]

                Just targetStartTime ->
                    case targetSummary.attempts of
                        [] ->
                            [ circle [ cx target_center_x, cy target_center_y, r target_radius, Svg.Attributes.class "targetCircleGrey" ] [] ]

                        lastTargetAttempt :: _ ->
                            let
                                numberOfAttempts =
                                    List.length targetSummary.attempts

                                ( classToBeApplied, lastTargetAttemptTime ) =
                                    case lastTargetAttempt of
                                        Success time ->
                                            ( "targetCircleGreen", time )

                                        Fail time ->
                                            ( "targetCircleRed", time )

                                deltaTime =
                                    deltaTimeBetween lastTargetAttemptTime targetStartTime

                                timeText =
                                    timetoString False False True True zone deltaTime
                            in
                            [ circle [ cx target_center_x, cy target_center_y, r target_radius, Svg.Attributes.class classToBeApplied ] [] ]
                                ++ [ text_ [ x target_center_x, y target_center_y, Svg.Attributes.class "shootIntentNumber" ] [ Svg.text (String.fromInt numberOfAttempts) ] ]
                                ++ [ text_ [ x shootIntentTime_center_x, y shootIntentTime_center_y, Svg.Attributes.class "shootIntentTime" ] [ Svg.text timeText ] ]

        Just (TargetId currentTargetNumber) ->
            case targetSummary.startTime of
                Nothing ->
                    [ circle [ cx target_center_x, cy target_center_y, r target_radius, Svg.Attributes.class "targetCircleGrey" ] [] ]

                Just targetStartTime ->
                    case targetSummary.attempts of
                        [] ->
                            let
                                deltaTime =
                                    deltaTimeBetween currentTime targetStartTime

                                timeText =
                                    timetoString False False True True zone deltaTime
                            in
                            [ circle [ cx target_center_x, cy target_center_y, r target_radius, Svg.Attributes.class "targetCircleWhite" ] [] ]
                                ++ [ text_ [ x shootIntentTime_center_x, y shootIntentTime_center_y, Svg.Attributes.class "shootIntentTime" ] [ Svg.text timeText ] ]

                        lastTargetAttempt :: _ ->
                            let
                                numberOfAttempts =
                                    List.length targetSummary.attempts

                                ( classToBeApplied, lastTargetAttemptTime ) =
                                    case lastTargetAttempt of
                                        Success time ->
                                            ( "targetCircleGreen", time )

                                        Fail time ->
                                            ( "targetCircleRed", time )

                                deltaTime =
                                    if targetNumber == currentTargetNumber then
                                        deltaTimeBetween currentTime targetStartTime

                                    else
                                        deltaTimeBetween lastTargetAttemptTime targetStartTime

                                timeText =
                                    timetoString False False True True zone deltaTime
                            in
                            [ circle [ cx target_center_x, cy target_center_y, r target_radius, Svg.Attributes.class classToBeApplied ] [] ]
                                ++ [ text_ [ x target_center_x, y target_center_y, Svg.Attributes.class "shootIntentNumber" ] [ Svg.text (String.fromInt numberOfAttempts) ] ]
                                ++ [ text_ [ x shootIntentTime_center_x, y shootIntentTime_center_y, Svg.Attributes.class "shootIntentTime" ] [ Svg.text timeText ] ]


multiTargetRowSVGView : Int -> Int -> Int -> Model -> List (Svg Msg)
multiTargetRowSVGView width y_coordinate radius model =
    let
        width_per_target =
            toFloat width / toFloat model.numberOfTargets

        center_y =
            y_coordinate + radius

        calculate_center_x_for_target =
            \target_number -> width_per_target * toFloat target_number + (width_per_target / 2)
    in
    case model.currentSeries of
        Nothing ->
            List.range 1 model.numberOfTargets
                |> List.map
                    (\target_number ->
                        targetView initializeATargetSummary target_number model.currentTarget radius center_y (calculate_center_x_for_target (target_number - 1)) model.zone model.currentTime
                    )
                |> List.concat

        Just (SeriesId currentSeries) ->
            let
                seriesSummary =
                    Array.get (currentSeries - 1) model.fullSummary
            in
            case seriesSummary of
                Nothing ->
                    List.range 1 model.numberOfTargets
                        |> List.map
                            (\target_number ->
                                targetView initializeATargetSummary target_number model.currentTarget radius center_y (calculate_center_x_for_target (target_number - 1)) model.zone model.currentTime
                            )
                        |> List.concat

                Just currentSeriesSummary ->
                    let
                        targetSummaryFor =
                            \target_number_index -> Array.get (target_number_index - 1) currentSeriesSummary.shootSummary
                    in
                    List.foldl (++)
                        []
                        (List.range 1 model.numberOfTargets
                            |> List.map
                                (\target_number ->
                                    case targetSummaryFor target_number of
                                        Nothing ->
                                            Debug.log "Application corrupted. One of the targets cannot be drawn because of a lack of information." []

                                        Just targetSummary ->
                                            targetView targetSummary target_number model.currentTarget radius center_y (calculate_center_x_for_target (target_number - 1)) model.zone model.currentTime
                                )
                        )


getCurrentPhase : Model -> String
getCurrentPhase model =
    case model.currentPhase of
        NotStarted ->
            "Not Started"

        Race racePhase ->
            case racePhase of
                ShortDistanceRunning ->
                    "Short Distance Running"

                Shooting ->
                    "Shooting"

                Running ->
                    "Running"

        Finished ->
            "Finished"


view : Model -> Html Msg
view model =
    let
        frame_width =
            320

        frame_height =
            500

        numberOfTargets =
            model.numberOfTargets

        currentTimeXPosition =
            "10%"

        currentTimeYPosition =
            "18"

        currentSeriesTimeXPosition =
            currentTimeXPosition

        currentSeriesTimeYPosition =
            "50"

        currentPhaseTimeXPosition =
            currentTimeXPosition

        currentPhaseTimeYPosition =
            "75"

        multiTargetRow_y_origin =
            100

        target_radius =
            25

        shootingButtons_y_origin =
            multiTargetRow_y_origin + target_radius * 2

        hitButton_radius =
            75

        missButton_radius =
            50
    in
    div []
        [ svg
            [ Svg.Attributes.width (String.fromInt frame_width)
            , Svg.Attributes.height (String.fromInt frame_height)
            , viewBox ("0 0 " ++ String.fromInt frame_width ++ " " ++ String.fromInt frame_height)
            ]
            (currentTimeSVGView "currentTime" currentTimeXPosition currentTimeYPosition model
                ++ currentSeriesTimeSVGView "currentSeriesTime" currentSeriesTimeXPosition currentSeriesTimeYPosition model
                ++ currentPhaseTimeSVGView "currentPhaseTime" currentPhaseTimeXPosition currentPhaseTimeYPosition model
                ++ multiTargetRowSVGView frame_width multiTargetRow_y_origin target_radius model
                ++ shootingButtonsSVGView frame_width shootingButtons_y_origin hitButton_radius missButton_radius
            )
        , button [ Html.Events.onClick Restart ] [ Html.text "Restart" ]
        , div [] [ Html.text (getCurrentPhase model) ]
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
