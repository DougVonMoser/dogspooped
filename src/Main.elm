module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Time exposing (Posix)
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, class, autofocus)
import Html.Styled.Events exposing (onClick, onInput)
import Json.Decode
import Task
import DateFormat
import Array
import Char


type MealStatus
    = HaveNot
    | Half
    | Eated


type alias Occurence =
    { posix : Posix
    , pickerOpenForMe : Bool
    }


type alias Dog =
    { poops : List Occurence
    , pees : List Occurence
    , name : String
    }


type TimeAdjust
    = NotInProgress
    | InProgress
        { occurence : Occurence
        , inputValue : String
        , pointer : Pointer
        , pointed : Pointer
        }


type Pointer
    = FindingPositionThingies
    | GotOriginElement Dom.Element


type alias MiniModel =
    { dogs : List Dog
    , zone : Time.Zone
    , allergied : Bool
    , breakfast : MealStatus
    , dinner : MealStatus
    , timeAdjust : TimeAdjust
    }


type Model
    = GettingTimeZone
    | TimeZoneLoaded MiniModel


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    ( GettingTimeZone, Task.perform GotTimeZone Time.here )


initDogs =
    [ { poops = []
      , pees = []
      , name = "Oakley"
      }
    , { poops = []
      , pees = []
      , name = "Easton"
      }
    ]


type Msg
    = JustPooped Dog
    | JustPeed Dog
    | JustAllergied
    | PeeTime Dog Posix
    | PoopTime Dog Posix
    | GotTimeZone Time.Zone
    | JustBreakfasted
    | JustDinnered
    | ShowATimePicker Occurence
    | AdjustmentEvent String
    | CloseAndUpdateTime
    | FoundOccurenceEl (Result Dom.Error Dom.Element)
    | FoundTimeAdjustEl (Result Dom.Error Dom.Element)
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg bigmodel =
    case bigmodel of
        GettingTimeZone ->
            case msg of
                GotTimeZone zone ->
                    ( TimeZoneLoaded
                        { zone = zone
                        , dogs = initDogs
                        , allergied = False
                        , breakfast = HaveNot
                        , dinner = HaveNot
                        , timeAdjust = NotInProgress
                        }
                    , Cmd.none
                    )

                _ ->
                    ( bigmodel, Cmd.none )

        TimeZoneLoaded model ->
            case msg of
                PeeTime dog posix ->
                    let
                        newMods =
                            List.map
                                (\x ->
                                    if x == dog then
                                        let
                                            newOccurennce =
                                                { posix = posix
                                                , pickerOpenForMe = False
                                                }
                                        in
                                            { x | pees = (newOccurennce :: x.pees) }
                                    else
                                        x
                                )
                                model.dogs
                    in
                        ( TimeZoneLoaded { model | dogs = newMods }, Cmd.none )

                PoopTime dog posix ->
                    let
                        newMods =
                            List.map
                                (\x ->
                                    if x == dog then
                                        let
                                            newOccurennce =
                                                { posix = posix
                                                , pickerOpenForMe = False
                                                }
                                        in
                                            { x | poops = (newOccurennce :: x.poops) }
                                    else
                                        x
                                )
                                model.dogs
                    in
                        ( TimeZoneLoaded { model | dogs = newMods }, Cmd.none )

                JustPeed dog ->
                    ( bigmodel, Task.perform (PeeTime dog) Time.now )

                JustPooped dog ->
                    ( bigmodel, Task.perform (PoopTime dog) Time.now )

                JustAllergied ->
                    ( TimeZoneLoaded { model | allergied = True }, Cmd.none )

                JustBreakfasted ->
                    let
                        newStatus =
                            case model.breakfast of
                                HaveNot ->
                                    Half

                                _ ->
                                    Eated
                    in
                        ( TimeZoneLoaded { model | breakfast = newStatus }, Cmd.none )

                JustDinnered ->
                    let
                        newStatus =
                            case model.dinner of
                                HaveNot ->
                                    Half

                                _ ->
                                    Eated
                    in
                        ( TimeZoneLoaded { model | dinner = newStatus }, Cmd.none )

                ShowATimePicker occurence ->
                    let
                        newTimeAdjust =
                            InProgress
                                { occurence = occurence
                                , inputValue = ""
                                , pointer = FindingPositionThingies
                                , pointed = FindingPositionThingies
                                }
                    in
                        ( TimeZoneLoaded { model | timeAdjust = newTimeAdjust }
                        , Cmd.batch
                            [ Task.attempt (\x -> Noop) (Dom.focus "input-adjust")
                            , Task.attempt (FoundOccurenceEl) (Dom.getElement "test")
                            , Task.attempt (FoundTimeAdjustEl) (Dom.getElement "input-adjust")
                            ]
                        )

                AdjustmentEvent rawUserTotalInput ->
                    case model.timeAdjust of
                        InProgress progressRecord ->
                            case groomInput progressRecord.inputValue rawUserTotalInput of
                                KeepEditing groomedInput ->
                                    ( TimeZoneLoaded
                                        { model
                                            | timeAdjust =
                                                InProgress
                                                    { progressRecord | inputValue = groomedInput }
                                        }
                                    , Cmd.none
                                    )

                                -- implement escape, goodAndDoneAdjustment
                                _ ->
                                    ( bigmodel, Cmd.none )

                        _ ->
                            ( bigmodel, Cmd.none )

                FoundOccurenceEl elementFindResult ->
                    case elementFindResult of
                        Ok element ->
                            case model.timeAdjust of
                                InProgress progressRecord ->
                                    ( TimeZoneLoaded
                                        { model
                                            | timeAdjust =
                                                InProgress
                                                    { progressRecord | pointer = GotOriginElement element }
                                        }
                                    , Cmd.none
                                    )

                                _ ->
                                    ( bigmodel, Cmd.none )

                        _ ->
                            ( bigmodel, Cmd.none )

                FoundTimeAdjustEl elementFindResult ->
                    case elementFindResult of
                        Ok element ->
                            case model.timeAdjust of
                                InProgress progressRecord ->
                                    ( TimeZoneLoaded
                                        { model
                                            | timeAdjust =
                                                InProgress
                                                    { progressRecord | pointed = GotOriginElement element }
                                        }
                                    , Cmd.none
                                    )

                                _ ->
                                    ( bigmodel, Cmd.none )

                        _ ->
                            ( bigmodel, Cmd.none )

                CloseAndUpdateTime ->
                    case model.timeAdjust of
                        InProgress progressRecord ->
                            case parseInputToPosix progressRecord.inputValue model.zone of
                                Just updatedPosix ->
                                    let
                                        newDogs =
                                            adjustDogOccurence progressRecord.occurence model.dogs updatedPosix
                                    in
                                        ( TimeZoneLoaded { model | timeAdjust = NotInProgress, dogs = newDogs }, Cmd.none )

                                Nothing ->
                                    ( TimeZoneLoaded { model | timeAdjust = NotInProgress }, Cmd.none )

                        NotInProgress ->
                            ( TimeZoneLoaded { model | timeAdjust = NotInProgress }, Cmd.none )

                _ ->
                    ( bigmodel, Cmd.none )


type ActionAfterInput
    = KeepEditing String
    | Escape
    | GoodAndDoneAdjustment String


type MagicToDoOnString
    = DoNothing
    | DeleteChar
    | AddColon
    | IgnoreLastChar
    | DeemFinished
    | MoveColonCuzTenElevenTwelve


groomInput : String -> String -> ActionAfterInput
groomInput existingInput userAddedInput =
    let
        pipeline userAdded =
            ignoreNonDigits userAdded
                |> Maybe.map addAColonMaybe
                |> Maybe.andThen restrictFiveDigits
    in
        case pipeline userAddedInput of
            Just updatedInput ->
                KeepEditing updatedInput

            Nothing ->
                KeepEditing existingInput


restrictFiveDigits : String -> Maybe String
restrictFiveDigits stringy =
    if String.length stringy <= 5 then
        Just stringy
    else
        Nothing


addAColonMaybe : String -> String
addAColonMaybe stringy =
    if String.length stringy == 2 then
        stringy ++ ":"
    else
        stringy


ignoreNonDigits : String -> Maybe String
ignoreNonDigits raw =
    String.toList raw
        |> List.reverse
        |> Debug.log "omg!"
        |> List.head
        |> Maybe.map Char.isDigit
        |> Maybe.andThen
            (\x ->
                if x == True then
                    Just raw
                else
                    Nothing
            )


adjustDogOccurence : Occurence -> List Dog -> Posix -> List Dog
adjustDogOccurence occurence oldDogs newPosix =
    List.map
        (\dog ->
            let
                newPoops =
                    List.map
                        (\occ ->
                            if occ == occurence then
                                { occ | posix = newPosix }
                            else
                                occ
                        )
                        dog.poops

                newPees =
                    List.map
                        (\occ ->
                            if occ == occurence then
                                { occ | posix = newPosix }
                            else
                                occ
                        )
                        dog.pees
            in
                { dog | poops = newPoops, pees = newPees }
        )
        oldDogs


parseInputToPosix : String -> Time.Zone -> Maybe Posix
parseInputToPosix inputValue timeZone =
    let
        timeArray =
            String.split ":" inputValue
                |> Array.fromList

        hours =
            timeArray
                |> Array.get 0
                |> Maybe.andThen String.toInt
                |> Maybe.andThen
                    (\x ->
                        if x <= 24 then
                            Just x
                        else
                            Nothing
                    )

        minutes =
            timeArray
                |> Array.get 1
                |> Maybe.andThen String.toInt
                |> Maybe.andThen
                    (\x ->
                        if x <= 59 then
                            Just x
                        else
                            Nothing
                    )
    in
        case ( hours, minutes ) of
            ( Just h, Just m ) ->
                Just ((((h + 5) * 60) + m) * 60 * 1000 |> Time.millisToPosix)

            ( _, _ ) ->
                Nothing



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        GettingTimeZone ->
            text ""

        TimeZoneLoaded minimodel ->
            let
                testPointer =
                    case minimodel.timeAdjust of
                        NotInProgress ->
                            [ text "" ]

                        InProgress progressRecord ->
                            case ( progressRecord.pointer, progressRecord.pointed ) of
                                ( GotOriginElement occurenceSourceEl, GotOriginElement targetWord ) ->
                                    generatePointerElement occurenceSourceEl targetWord

                                ( _, _ ) ->
                                    [ text "" ]

                timePicker =
                    case minimodel.timeAdjust of
                        NotInProgress ->
                            text ""

                        InProgress progressRecord ->
                            div
                                [ css (inputContainerStyle ++ [ zIndex (int 1) ])
                                ]
                                [ div [ css [ zIndex (int 4) ] ]
                                    [ input
                                        [ css [ fontSize (px 80), width (px 250) ]
                                        , Html.Styled.Attributes.value progressRecord.inputValue
                                        , Html.Styled.Attributes.id "input-adjust"
                                        , onInput AdjustmentEvent
                                        ]
                                        []
                                    , button
                                        [ css
                                            [ backgroundColor (rgb 255 255 255)
                                            ]
                                        , onClick CloseAndUpdateTime
                                        ]
                                        [ text "update" ]
                                    ]
                                ]
            in
                div [ css [ width (vw 100), height (vh 100), overflow hidden ] ]
                    (List.append
                        [ div []
                            [ viewAllergy minimodel
                            , viewMeal JustBreakfasted minimodel.breakfast "🍳"
                            , viewMeal JustDinnered minimodel.dinner "🍔"
                            ]
                        , div []
                            (List.map (dogView minimodel.zone) minimodel.dogs)
                        , timePicker
                        ]
                        testPointer
                    )


findTanDegrees : Float -> Float -> Float
findTanDegrees opp adj =
    let
        radians =
            atan (opp / adj)
    in
        (radians * 180) / pi



-- (x1,y1) -> (y2,y2)-> Html.Msg


type alias Points =
    ( ( Float, Float ), ( Float, Float ) )


generateLineFromPoints : Points -> Html Msg
generateLineFromPoints ( ( x1, y1 ), ( x2, y2 ) ) =
    let
        adjacent =
            y2 - y1

        oppositte =
            x2 - x1

        hypotenuse =
            sqrt ((adjacent ^ 2) + (oppositte ^ 2))

        degrees =
            findTanDegrees oppositte adjacent
    in
        div
            [ class "point"
            , css
                [ borderLeft3 (px 5) dashed (rgb 11 14 17)
                , position absolute
                , top (px y1)
                , left (px x1)
                , height (px hypotenuse)
                , transform (rotate (deg -degrees))
                ]
            ]
            []


generatePointerElement : Dom.Element -> Dom.Element -> List (Html Msg)
generatePointerElement pointer pointed =
    let
        line1 =
            let
                x1 =
                    pointer.element.x

                x2 =
                    pointed.element.x

                y1 =
                    pointer.element.y + pointer.element.height

                y2 =
                    pointed.element.y + pointed.element.height
            in
                generateLineFromPoints ( ( x1, y1 ), ( x2, y2 ) )

        line2 =
            let
                x1 =
                    pointer.element.x

                y1 =
                    pointer.element.y

                x2 =
                    pointed.element.x

                y2 =
                    pointed.element.y
            in
                generateLineFromPoints ( ( x1, y1 ), ( x2, y2 ) )

        line3 =
            let
                x1 =
                    pointer.element.x + pointer.element.width

                y1 =
                    pointer.element.y + pointer.element.height

                x2 =
                    pointed.element.x + pointed.element.width

                y2 =
                    pointed.element.y + pointed.element.height
            in
                generateLineFromPoints ( ( x1, y1 ), ( x2, y2 ) )

        line4 =
            let
                x1 =
                    pointer.element.x + pointer.element.width

                y1 =
                    pointer.element.y

                x2 =
                    pointed.element.x + pointer.element.width

                y2 =
                    pointed.element.y
            in
                generateLineFromPoints ( ( x1, y1 ), ( x2, y2 ) )
    in
        [ line1, line2, line3, line4 ]


largeFont =
    fontSize (px 40)


viewHour : Time.Zone -> Posix -> String
viewHour =
    DateFormat.format
        [ DateFormat.hourNumber
        , DateFormat.text ":"
        , DateFormat.minuteFixed
        , DateFormat.text " "
        , DateFormat.amPmLowercase
        , DateFormat.text " "
        ]


viewTimeStamps : Time.Zone -> String -> List Occurence -> List (Html Msg)
viewTimeStamps zone wasteAction wastes =
    let
        spanner waste stringy =
            span [ Html.Styled.Attributes.id "test", css [ margin (px 8) ], onClick (ShowATimePicker waste) ]
                [ text stringy ]

        occurrenceToTimeStamp waste =
            let
                spannylol =
                    (.posix >> (viewHour zone) >> (++) wasteAction >> (spanner waste)) waste
            in
                spannylol
    in
        List.map occurrenceToTimeStamp wastes


viewWaste : Time.Zone -> Msg -> String -> List Occurence -> Html Msg
viewWaste zone msg wasteAction wastes =
    div [ class wasteAction, css [ largeFont, paddingLeft (px 16) ] ]
        [ wasteButton msg wasteAction
        , span [] (viewTimeStamps zone wasteAction wastes)
        ]


wasteButton msg wasteAction =
    button [ onClick msg, css [ largeFont, marginRight (px 16) ] ] [ " " ++ wasteAction ++ " " |> text ]


viewAllergy dog =
    if dog.allergied then
        div []
            [ wasteButton Noop "💊"
            , text "allergied"
            ]
    else
        wasteButton JustAllergied "💊"


dogView : Time.Zone -> Dog -> Html Msg
dogView zone dog =
    div [ css [ margin (px 16), padding4 zero zero (px 16) (px 16) ] ]
        [ h1 [] [ text dog.name ]
        , (viewWaste zone) (JustPooped dog) "💩" dog.poops
        , (viewWaste zone) (JustPeed dog) "🍋" dog.pees
        ]



-- Beckfast 🍳
-- Dinner 🍔


viewMeal : Msg -> MealStatus -> String -> Html Msg
viewMeal msg mealStatus icon =
    let
        statusHtml =
            case mealStatus of
                HaveNot ->
                    text "have not eaten"

                Half ->
                    text "half"

                Eated ->
                    text "all eated"
    in
        div [] [ wasteButton msg icon, statusHtml ]


inputContainerStyle =
    [ position absolute
    , backgroundColor (rgba 6 6 6 0.5)
    , top zero
    , left zero
    , width (pct 100)
    , height (pct 100)
    , border3 (px 2) solid (rgb 12 12 12)
    , displayFlex
    , justifyContent center
    , alignItems center
    ]


main =
    Browser.element
        { init = init
        , subscriptions = (\x -> Sub.none)
        , view = view >> toUnstyled
        , update = update
        }
