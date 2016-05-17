module ForgotPassword exposing (main)

import Json.Decode
import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Process
import Random
import String
import Task exposing (Task)
import Time exposing (millisecond)
import Validate
import Debounce exposing (..)


type alias Model =
    { email : String
    , emailState : Validate.State {}
    , username : String
    , usernameDebouncer : Debouncer String
    , usernameState : Validate.State { seed : Random.Seed }
    , message : Maybe String
    , state : Validate.State {}
    }


type Msg
    = NoOp
    | SetEmail String
    | SetUsername String
    | DebounceUsername (DebouncerMsg String)
    | BeginSubmit
    | EndSubmit (Model -> Model)
    | SetValidateState (Model -> Model)


init : ( Model, Cmd Msg )
init =
    (,)
        { email = ""
        , emailState = { error = Nothing }
        , username = ""
        , usernameDebouncer = initDebouncer
        , usernameState = { seed = Random.initialSeed 0, error = Nothing }
        , message = Nothing
        , state = { error = Nothing }
        }
        Cmd.none


validateEmail : Validate.Validator Model
validateEmail =
    Validate.validator .email
        .emailState
        (\state model -> { model | emailState = state })
        --  sync validators
        [ Validate.email " is invalid" ]
        -- async validators
        []


checkUsernameExists : Validate.AsyncValidator { seed : Random.Seed } String
checkUsernameExists state value =
    let
        latencyRng =
            Random.float 25 250

        -- simulate latency delay
        ( latency, newSeed ) =
            Random.step latencyRng state.seed
    in
        Process.sleep latency
            `Task.andThen` \_ ->
                            Task.succeed
                                { state
                                    | seed = newSeed
                                    , error =
                                        if Validate.isNotEmpty value && value /= "test" then
                                            Just (" \"" ++ value ++ "\" is not registered. (" ++ (toString latency) ++ "ms)")
                                        else
                                            Nothing
                                }


validateUsername : Validate.Validator Model
validateUsername =
    Validate.validator .username
        .usernameState
        (\state model -> { model | usernameState = state })
        --  sync validators
        []
        -- async validators
        [ checkUsernameExists ]


validateUsernameOrEmail : Validate.Validator Model
validateUsernameOrEmail =
    Validate.validator (\model -> ( model.username, model.email ))
        .state
        (\state model -> { model | state = state })
        --  sync validators
        [ Validate.basic (\( username, email ) -> not ((String.isEmpty username) && (String.isEmpty email)))
            "Please enter your email or username."
        ]
        -- async validators
        []


validateModel : Validate.Validator Model
validateModel =
    Validate.combine
        <| [ validateEmail
           , validateUsername
           , validateUsernameOrEmail
           ]


modelValidateStates : List (Model -> { error : Maybe String })
modelValidateStates =
    [ .emailState
      -- extract error because the usernameState contains other fields
      -- causing the Model -> { error: Maybe String } to not match
    , \model -> { error = model.usernameState.error }
    , .state
    ]


isValidModel : Model -> Bool
isValidModel =
    Validate.isValidModel modelValidateStates


isInvalidModel : Model -> Bool
isInvalidModel =
    Validate.isInvalidModel modelValidateStates


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetEmail value ->
            Validate.combine
                [ validateEmail
                , validateUsernameOrEmail
                ]
                { model | email = value }
                |> Validate.toCmd SetValidateState

        SetUsername value ->
            Validate.combine
                [ validateUsername
                , validateUsernameOrEmail
                ]
                { model | username = value }
                |> Validate.toCmd SetValidateState

        DebounceUsername value ->
            let
                -- set the username on the model,
                -- otherwise causes laggy behaviour for on "input"
                -- due to setting the attribute: value model.username
                -- on the input
                s =
                    case value of
                        Bounce v ->
                            v

                        _ ->
                            model.username

                ( newDebouncer, eff ) =
                    updateDebouncer (500 * millisecond)
                        SetUsername
                        value
                        model.usernameDebouncer
            in
                ( { model | username = s, usernameDebouncer = newDebouncer }
                , Cmd.map (handleDebouncerResults DebounceUsername) eff
                )

        BeginSubmit ->
            validateModel model
                |> Validate.toCmd EndSubmit

        EndSubmit setValidateState ->
            let
                newModel =
                    setValidateState model
            in
                if isValidModel newModel then
                    ( { newModel | message = Just "Submit: Valid" }, Cmd.none )
                else
                    ( { newModel | message = Just "Submit: Invalid" }, Cmd.none )

        SetValidateState setValidateState ->
            ( setValidateState model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


onChangeEvent : List (Maybe String) -> String
onChangeEvent errors =
    if List.all (\error -> error == Nothing) errors then
        "blur"
    else
        "input"


view : Model -> Html Msg
view model =
    Html.form [ novalidate True ]
        [ node "link" [ href "./forgot-password.css", rel "stylesheet" ] []
        , h1 [] [ text "Forgot password" ]
        , p [] [ text "Please enter your email or username below to reset your password." ]
        , div
            [ class
                <| "field"
                ++ (if Validate.isValid model.emailState then
                        ""
                    else
                        " error"
                   )
            ]
            [ label [] [ text <| "Email" ++ (Maybe.withDefault "" model.emailState.error) ]
            , input
                [ type' "email"
                  -- , autofocus True -- removed to demonstrate submit Validate 'invalid' state
                , placeholder "Enter your email address"
                , value model.email
                , on (onChangeEvent [ model.emailState.error, model.state.error ]) (Json.Decode.map SetEmail targetValue)
                ]
                []
            ]
        , div [ style [ ( "margin-bottom", "0.5em" ) ] ] [ text "or" ]
        , div
            [ class
                <| "field"
                ++ (if Validate.isValid model.usernameState then
                        ""
                    else
                        " error"
                   )
            ]
            [ label [] [ text <| "Username" ++ (Maybe.withDefault "" model.usernameState.error) ]
            , input
                [ type' "text"
                , placeholder "Enter your username"
                , value model.username
                , on (onChangeEvent [ model.usernameState.error, model.state.error ])
                    (targetValue `Json.Decode.andThen` \s -> Json.Decode.succeed (DebounceUsername <| Bounce s))
                ]
                []
            ]
        , div [ class "field error" ] [ label [] [ text <| Maybe.withDefault "" model.state.error ] ]
        , button
            [ type' "button"
            , class "btn"
            , disabled <| model.message /= Nothing && (isInvalidModel model)
            , onClick BeginSubmit
            ]
            [ text "Reset password" ]
        , div [] [ text <| Maybe.withDefault "" model.message ]
        ]


main : Program Never
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
