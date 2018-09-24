module Main exposing (..)

import Browser
import Time exposing (Posix)
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, class)
import Html.Styled.Events exposing (onClick)
import Json.Decode
import Task
import DateFormat
import TimePicker exposing (TimeEvent(..), TimePicker, Msg(..))


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


type alias MiniModel =
    { dogs : List Dog
    , zone : Time.Zone
    , allergied : Bool
    , breakfast : MealStatus
    , dinner : MealStatus
    , timePicker : Maybe TimePicker
    }


type Model
    = GettingTimeZone
    | TimeZoneLoaded MiniModel


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


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    ( GettingTimeZone, Task.perform GotTimeZone Time.here )


type Msg
    = JustPooped Dog
    | JustPeed Dog
    | JustAllergied
    | PeeTime Dog Posix
    | PoopTime Dog Posix
    | GotTimeZone Time.Zone
    | JustBreakfasted
    | JustDinnered
    | GotTimePickerMsg TimePicker.Msg
    | ShowATimePicker Occurence
    | CloseAndUpdateTime
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
                        , timePicker = Nothing -- TimePicker.init Nothing
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

                                Half ->
                                    Eated

                                Eated ->
                                    Eated
                    in
                        ( TimeZoneLoaded { model | breakfast = newStatus }, Cmd.none )

                JustDinnered ->
                    let
                        newStatus =
                            case model.dinner of
                                HaveNot ->
                                    Half

                                Half ->
                                    Eated

                                Eated ->
                                    Eated
                    in
                        ( TimeZoneLoaded { model | dinner = newStatus }, Cmd.none )

                ShowATimePicker occurence ->
                    -- let
                    --     timePickerSettings =
                    --         let
                    --             default =
                    --                 TimePicker.defaultSettings
                    --         in
                    --             { default
                    --                 | showSeconds = False
                    --                 , minuteStep = 15
                    --             }
                    -- in
                    let
                        tp =
                            TimePicker.init Nothing

                        ( updatedTp, timeEvent ) =
                            TimePicker.update TimePicker.defaultSettings TimePicker.NoOp tp
                    in
                        ( TimeZoneLoaded { model | timePicker = Just updatedTp }, Cmd.none )

                CloseAndUpdateTime ->
                    ( TimeZoneLoaded { model | timePicker = Nothing }, Cmd.none )

                GotTimePickerMsg m ->
                    case model.timePicker of
                        Just tp ->
                            let
                                ( updatedPicker, timeEvent ) =
                                    TimePicker.update TimePicker.defaultSettings m tp
                            in
                                ( TimeZoneLoaded { model | timePicker = Just updatedPicker }, Cmd.none )

                        Nothing ->
                            ( bigmodel, Cmd.none )

                _ ->
                    ( bigmodel, Cmd.none )



-- VIEW


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
            span [ css [ margin (px 8) ], onClick (ShowATimePicker waste) ]
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


view : Model -> Html Msg
view model =
    case model of
        GettingTimeZone ->
            text ""

        TimeZoneLoaded minimodel ->
            case minimodel.timePicker of
                Just tp ->
                    div []
                        [ input [ Html.Styled.Attributes.value "9:58AM" ] []
                        , button [ onClick CloseAndUpdateTime ] [ text "X" ]
                        ]

                Nothing ->
                    div []
                        [ div []
                            [ viewAllergy minimodel
                            , viewMeal JustBreakfasted minimodel.breakfast "🍳"
                            , viewMeal JustDinnered minimodel.dinner "🍔"
                            ]
                        , div [] (List.map (dogView minimodel.zone) minimodel.dogs)
                        , div [ class "default-time-picker" ]
                            []
                        ]


main =
    Browser.element
        { init = init
        , subscriptions = (\x -> Sub.none)
        , view = view >> toUnstyled
        , update = update
        }



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
