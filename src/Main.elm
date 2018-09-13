module Main exposing (..)

import Browser
import Time exposing (Posix)
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, class)
import Html.Styled.Events exposing (onClick)
import Json.Decode


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                JustPeed dog ->
                    List.map
                        (\x ->
                            if x == dog then
                                { x | pees = (Time.millisToPosix 1536847616243 :: x.pees) }
                            else
                                x
                        )
                        model

                JustPooped dog ->
                    List.map
                        (\x ->
                            if x == dog then
                                { x | poops = (Time.millisToPosix 1536847616243 :: x.poops) }
                            else
                                x
                        )
                        model
    in
        ( newModel, Cmd.none )


viewHour : Posix -> String
viewHour posix =
    Time.toHour Time.utc posix
        |> String.fromInt


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
