module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (..)


-- import String
-- Model


type alias Model =
    { quote : String }


init : ( Model, Cmd Msg )
init =
    ( Model "", Cmd.none )



-- Update


type Msg
    = GetQuote


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetQuote ->
            ( { model | quote = model.quote ++ "A quote!" }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h2 [ class "text-center" ] [ text "Chucky Says" ]
        , p [ class "text-center" ]
            [ button [ class "btn btn-success", onClick GetQuote ]
                [ text "Get some Wisdom!" ]
            ]
        , blockquote []
            [ p [] [ text model.quote ]
            ]
        ]


main : Program Never
main =
    App.program
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
