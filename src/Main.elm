module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Task exposing (Task)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import String


-- import String
-- Model


type alias Model =
    { username : String
    , password : String
    , token : String
    , quote : String
    , errorMsg : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" "" "" "" "", fetchRandomQuoteCmd )



-- Update


type Msg
    = GetQuote
    | FetchQuoteSucess String
    | HttpError Http.Error
    | AuthError Http.Error
    | GetTokenSuccess String
    | ClickRegisterUser
    | SetUsername String
    | SetPassword String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetQuote ->
            ( model, fetchRandomQuoteCmd )

        FetchQuoteSucess newQuote ->
            ( { model | quote = newQuote }, Cmd.none )

        HttpError _ ->
            ( model, Cmd.none )

        AuthError error ->
            ( { model | errorMsg = (toString error) }, Cmd.none )

        SetUsername username ->
            ( { model | username = username }, Cmd.none )

        SetPassword password ->
            ( { model | password = password }, Cmd.none )

        ClickRegisterUser ->
            ( model, authUserCmd model registerUrl )

        GetTokenSuccess newToken ->
            ( { model | token = newToken, errorMsg = "" } |> Debug.log "got new token"
            , Cmd.none
            )



-- _ ->
--     ( model, Cmd.none )


api : String
api =
    "http://localhost:3001/"


registerUrl : String
registerUrl =
    api ++ "users"


randomQuoteUrl : String
randomQuoteUrl =
    api ++ "api/random-quote"


userEncoder : Model -> Encode.Value
userEncoder model =
    Encode.object
        [ ( "username", Encode.string model.username )
        , ( "password", Encode.string model.password )
        ]


authUser : Model -> String -> Task Http.Error String
authUser model apiUrl =
    { verb = "Post"
    , headers = [ ( "Content-Type", "application/json" ) ]
    , url = apiUrl
    , body = Http.string <| Encode.encode 0 <| userEncoder model
    }
        |> Http.send Http.defaultSettings
        |> Http.fromJson tokenDecoder


authUserCmd : Model -> String -> Cmd Msg
authUserCmd model apiUrl =
    Task.perform AuthError GetTokenSuccess <| authUser model apiUrl


tokenDecoder : Decoder String
tokenDecoder =
    "id_token" := Decode.string


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
        , div [ class "jumbotron text-left" ]
            [ authBoxView model ]
        ]


authBoxView : Model -> Html Msg
authBoxView model =
    let
        loggedIn : Bool
        loggedIn =
            (String.length model.token) > 0

        greeting : String
        greeting =
            "Hello, " ++ model.username ++ "!"

        showError : String
        showError =
            if String.isEmpty model.errorMsg then
                "hidden"
            else
                ""
    in
        if loggedIn then
            div [ id "greeting" ]
                [ h3 [ class "text-center" ] [ text greeting ]
                , p [ class "text-center" ] [ text "You have are granted access to infinite wisdom." ]
                ]
        else
            div [ id "form" ]
                [ h2 [ class "text-center" ] [ text "Log In or Register" ]
                , p [ class "help-block" ] [ text "If you already have an account, please Log In. Otherwise, enter your desired username and password and Register." ]
                , div [ class showError ]
                    [ div [ class "alert alert-danger" ] [ text model.errorMsg ]
                    ]
                , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                        [ label [ for "username" ] [ text "Username:" ]
                        , input [ id "username", type' "text", class "form-control", Html.Attributes.value model.username, onInput SetUsername ] []
                        ]
                    ]
                , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                        [ label [ for "password" ] [ text "Password:" ]
                        , input [ id "password", type' "password", class "form-control", Html.Attributes.value model.password, onInput SetPassword ] []
                        ]
                    ]
                , div [ class "text-center" ]
                    [ button [ class "btn btn-link", onClick ClickRegisterUser ] [ text "Register" ] ]
                ]


main : Program Never
main =
    App.program
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
