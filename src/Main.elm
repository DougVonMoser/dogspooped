module Main exposing (..)

import Browser
import Html exposing (text)


type MealStatus
    = HaveNot
    | Half
    | Eated


type alias Dog =
    { poops : List Int
    , pees : List Int
    , allergied : Bool
    , breakfast : MealStatus
    , dinner : MealStatus
    , name : String
    }


type alias Model =
    List Dog


initModel : Model
initModel =
    []


update msg model =
    []


view model =
    text "working!"


main =
    Browser.sandbox
        { init = initModel
        , view = view
        , update = update
        }
