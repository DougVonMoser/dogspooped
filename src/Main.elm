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
    , allergied : Bool
    , breakfast : MealStatus
    , dinner : MealStatus
    , name : String
    }


type alias MiniModel =
    { dogs : List Dog
    , zone : Time.Zone
    }


type Model
    = GettingTimeZone
    | TimeZoneLoaded MiniModel


initModelish =
    [ { poops = []
      , pees = []
      , allergied = False
      , breakfast = HaveNot
      , dinner = HaveNot
      , name = "Oakley"
      }
    , { poops = []
      , pees = []
      , allergied = False
      , breakfast = HaveNot
      , dinner = HaveNot
      , name = "Easton"
      }
    ]


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    ( GettingTimeZone, Task.perform GotTimeZone Time.here )


type Msg
    = JustPooped Dog
    | JustPeed Dog
    | PeeTime Dog Posix
    | PoopTime Dog Posix
    | GotTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg bigmodel =
    case bigmodel of
        GettingTimeZone ->
            case msg of
                GotTimeZone zone ->
                    ( TimeZoneLoaded { zone = zone, dogs = initModelish }, Cmd.none )

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

                GotTimeZone zone ->
                    ( bigmodel, Cmd.none )


viewHour : Time.Zone -> Posix -> String
viewHour =
    DateFormat.format
        [ DateFormat.text " "
        , DateFormat.hourNumber
        , DateFormat.text ":"
        , DateFormat.minuteFixed
        , DateFormat.text " "
        , DateFormat.amPmLowercase
        ]


viewWaste : Time.Zone -> Msg -> String -> List Posix -> Html Msg
viewWaste zone msg wasteAction wastes =
    div [ class wasteAction, css [ paddingLeft (px 16) ] ]
        [ button [ onClick msg, css [ marginRight (px 16) ] ] [ " " ++ wasteAction ++ " " |> text ]
        , span [] [ text wasteAction ]
        , span [] (List.map ((viewHour zone) >> text) wastes)
        ]


dogView : Time.Zone -> Dog -> Html Msg
dogView zone dog =
    div [ css [ border3 (px 1) dashed (rgb 12 12 12), margin (px 16) ] ]
        [ text dog.name
        , (viewWaste zone) (JustPooped dog) "💩" dog.poops
        , (viewWaste zone) (JustPeed dog) "🍋" dog.pees
        ]


view : Model -> Html Msg
view model =
    case model of
        GettingTimeZone ->
            text ""

        TimeZoneLoaded minimodel ->
            List.map (dogView minimodel.zone) minimodel.dogs
                |> div []


main =
    Browser.element
        { init = init
        , subscriptions = (\x -> Sub.none)
        , view = view >> toUnstyled
        , update = update
        }
