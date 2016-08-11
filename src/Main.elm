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
import Http.Decorators


-- import String
-- Model


type alias Model =
    { username : String
    , password : String
    , token : String
    , quote : String
    , errorMsg : String
    , protectedQuote : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" "" "" "" "" "", fetchRandomQuoteCmd )



-- Update


type Msg
    = GetQuote
    | GetProtectedQuote
    | FetchQuoteSucess String
    | FetchProtectedQuoteSuccess String
    | HttpError Http.Error
    | AuthError Http.Error
    | GetTokenSuccess String
    | ClickRegisterUser
    | SetUsername String
    | SetPassword String
    | ClickLogIn
    | LogOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetQuote ->
            ( model, fetchRandomQuoteCmd )

        GetProtectedQuote ->
            ( model, fetchRandomProtectedQuoteCmd model )

        FetchQuoteSucess newQuote ->
            ( { model | quote = newQuote }, Cmd.none )

        FetchProtectedQuoteSuccess newQuote ->
            ( { model | protectedQuote = newQuote }, Cmd.none )

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

        ClickLogIn ->
            ( model, authUserCmd model loginUrl )

        LogOut ->
            ( { model
                | username = ""
                , password = ""
                , protectedQuote = ""
                , token = ""
                , errorMsg = ""
              }
            , Cmd.none
            )



-- _ ->
--     ( model, Cmd.none )


api : String
api =
    "http://localhost:3001/"


protectedQuoteUrl : String
protectedQuoteUrl =
    api ++ "api/protected/random-quote"


registerUrl : String
registerUrl =
    api ++ "users"


loginUrl : String
loginUrl =
    api ++ "sessions/create"


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


fetchRandomProtectedQuote : Model -> Task Http.Error String
fetchRandomProtectedQuote model =
    { verb = "GET"
    , headers = [ ( "Authorization", "Bearer " ++ model.token ) ]
    , url = protectedQuoteUrl
    , body = Http.empty
    }
        |> Http.send Http.defaultSettings
        |> Http.Decorators.interpretStatus
        |> Task.map responseText


fetchRandomProtectedQuoteCmd : Model -> Cmd Msg
fetchRandomProtectedQuoteCmd model =
    Task.perform HttpError FetchProtectedQuoteSuccess <| fetchRandomProtectedQuote model


fetchRandomQuoteCmd : Cmd Msg
fetchRandomQuoteCmd =
    Task.perform HttpError FetchQuoteSucess fetchRandomQuote


responseText : Http.Response -> String
responseText response =
    case response.value of
        Http.Text t ->
            t

        _ ->
            ""



-- View


view : Model -> Html Msg
view model =
    let
        loggedIn : Bool
        loggedIn =
            (String.length model.token) > 0
    in
        div [ class "container" ]
            [ h2 [ class "text-center" ] [ text "Chucky Says" ]
            , p [ class "text-center" ]
                [ button [ class "btn btn-success", onClick GetQuote ]
                    [ text "Get some Wisdom!" ]
                ]
            , blockquote []
                [ p [ class "text-center" ] [ text model.quote ]
                ]
            , div [ class "jumbotron text-left" ]
                [ authBoxView model loggedIn ]
            , div []
                [ h2 [ class "text-center" ] [ text "Protected Wisdom" ]
                , protectedQuoteView model loggedIn
                ]
            ]


authBoxView : Model -> Bool -> Html Msg
authBoxView model loggedIn =
    let
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
                , p [ class "text-center" ] [ button [ class "btn btn-danger", onClick LogOut ] [ text "Log Out" ] ]
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
                    [ button [ class "btn btn-primary", onClick ClickLogIn ] [ text "Log In" ]
                    , button [ class "btn btn-link", onClick ClickRegisterUser ] [ text "Register" ]
                    ]
                ]


protectedQuoteView : Model -> Bool -> Html Msg
protectedQuoteView model loggedIn =
    let
        hideIfNoProtectedQuote : String
        hideIfNoProtectedQuote =
            if String.isEmpty model.protectedQuote then
                "hidden"
            else
                ""
    in
        if loggedIn then
            div []
                [ p [ class "text-center" ]
                    [ button [ class "btn btn-info", onClick GetProtectedQuote ]
                        [ text "Get a Protected Quote!" ]
                    ]
                , blockquote [ class hideIfNoProtectedQuote ]
                    [ p [] [ text model.protectedQuote ] ]
                ]
        else
            p [ class "text-center" ] [ text "Please log in or register to see protected quote." ]


main : Program Never
main =
    App.program
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
