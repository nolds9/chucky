module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Task exposing (Task)


-- import String
-- Model


type alias Model =
    { quote : String }


init : ( Model, Cmd Msg )
init =
    ( Model "", fetchRandomQuoteCmd )



-- Update


type Msg
    = GetQuote
    | FetchQuoteSucess String
    | HttpError Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetQuote ->
            ( model, fetchRandomQuoteCmd )

        FetchQuoteSucess newQuote ->
            ( { model | quote = newQuote }, Cmd.none )

        HttpError _ ->
            ( model, Cmd.none )


api : String
api =
    "http://localhost:3001/"


randomQuoteUrl : String
randomQuoteUrl =
    api ++ "api/random-quote"


fetchRandomQuote : Platform.Task Http.Error String
fetchRandomQuote =
    Http.getString randomQuoteUrl


fetchRandomQuoteCmd : Cmd Msg
fetchRandomQuoteCmd =
    Task.perform HttpError FetchQuoteSucess fetchRandomQuote



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
