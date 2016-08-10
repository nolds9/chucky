module Main exposing (..)

import Html exposing (..)
import Html.App as App


-- import Html.Events exposing (..)
-- import Html.Attributes exposing (..)
-- import String
-- Model


type alias Model =
    String


initModel : Model
initModel =
    "World"



-- Update


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



-- View


view : Model -> Html Msg
view model =
    text ("Hello " ++ model)


main : Program Never
main =
    App.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }
