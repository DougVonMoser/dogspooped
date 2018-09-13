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


type alias Model =
    List Dog


initModel : Model
initModel =
    [ { poops = [ Time.millisToPosix 1536847616243 ]
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


init : Json.Decode.Value -> ( Model, Cmd msg )
init flags =
    ( initModel, Cmd.none )


type Msg
    = JustPooped Dog
    | JustPeed Dog
    | PeeTime Dog Posix
    | PoopTime Dog Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
                        model
            in
                ( newMods, Cmd.none )

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
                        model
            in
                ( newMods, Cmd.none )

        JustPeed dog ->
            ( model, Task.perform (PeeTime dog) Time.now )

        JustPooped dog ->
            ( model, Task.perform (PoopTime dog) Time.now )


viewHour : Posix -> String
viewHour posix =
    let
        hours =
            Time.toHour Time.utc posix
                |> String.fromInt

        minutes =
            Time.toMinute Time.utc posix
                |> String.fromInt
    in
        " " ++ hours ++ ":" ++ minutes ++ " "


viewWaste : Msg -> String -> List Posix -> Html Msg
viewWaste msg wasteAction wastes =
    div [ class wasteAction, css [ paddingLeft (px 16) ] ]
        [ button [ onClick msg ] []
        , span [] [ text wasteAction ]
        , span [] (List.map (viewHour >> text) wastes)
        ]


dogView : Dog -> Html Msg
dogView dog =
    div [ css [ border3 (px 1) dashed (rgb 12 12 12), margin (px 16) ] ]
        [ text dog.name
        , viewWaste (JustPooped dog) "poops" dog.poops
        , viewWaste (JustPeed dog) "pees" dog.pees
        ]


view : Model -> Html Msg
view model =
    List.map dogView model
        |> div []


main =
    Browser.element
        { init = init
        , subscriptions = (\x -> Sub.none)
        , view = view >> toUnstyled
        , update = update
        }
