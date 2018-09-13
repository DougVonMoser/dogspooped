module Main exposing (..)

import Browser
import Time exposing (Posix)
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, class)
import Html.Styled.Events exposing (onClick)


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


type Msg
    = Msg


update msg model =
    []


viewHour : Posix -> String
viewHour posix =
    Time.toHour Time.utc posix
        |> String.fromInt


viewWaste : String -> List Posix -> Html Msg
viewWaste wasteAction wastes =
    div [ class wasteAction, css [ paddingLeft (px 16) ] ]
        [ span [] [ text wasteAction ]
        , span [] (List.map (viewHour >> text) wastes)
        ]


dogView : Dog -> Html Msg
dogView dog =
    div [ css [ border3 (px 1) dashed (rgb 12 12 12), margin (px 16) ] ]
        [ text dog.name
        , viewWaste "poops" dog.poops
        , viewWaste "pees" dog.pees
        ]


view : Model -> Html Msg
view model =
    List.map dogView model
        |> div []


main =
    Browser.sandbox
        { init = initModel
        , view = view >> toUnstyled
        , update = update
        }
