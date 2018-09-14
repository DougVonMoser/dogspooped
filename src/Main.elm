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


type MealStatus
    = HaveNot
    | Half
    | Eated


type alias Dog =
    { poops : List Posix
    , pees : List Posix
    , name : String
    }


type alias MiniModel =
    { dogs : List Dog
    , zone : Time.Zone
    , allergied : Bool
    , breakfast : MealStatus
    , dinner : MealStatus
    }


type Model
    = GettingTimeZone
    | TimeZoneLoaded MiniModel


initDogs =
    [ { poops = []
      , pees = []

      --   , allergied = False
      --   , breakfast = HaveNot
      --   , dinner = HaveNot
      , name = "Oakley"
      }
    , { poops = []
      , pees = []

      --   , allergied = False
      --   , breakfast = HaveNot
      --   , dinner = HaveNot
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
                                        { x | pees = (posix :: x.pees) }
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
                                        { x | poops = (posix :: x.poops) }
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

                GotTimeZone zone ->
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


viewTimeStamps zone wasteAction wastes =
    let
        spanner stringy =
            span [ css [ margin (px 8) ] ] [ text stringy ]

        mapper =
            ((viewHour zone) >> (++) wasteAction >> spanner)
    in
        List.map mapper wastes


viewWaste : Time.Zone -> Msg -> String -> List Posix -> Html Msg
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
            div []
                [ div []
                    [ h1 []
                        [ text "Needs"
                        ]
                    , viewAllergy minimodel
                    , viewMeal JustBreakfasted minimodel.breakfast "🍳"
                    , viewMeal JustDinnered minimodel.dinner "🍔"
                    ]
                , div [] (List.map (dogView minimodel.zone) minimodel.dogs)
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
