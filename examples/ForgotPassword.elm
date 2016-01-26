module ForgotPassword where

import Validation

import Effects exposing (Effects,Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal.Time exposing (settledAfter)
import StartApp

import Random
import String
import Task exposing (Task)
import Time exposing (millisecond)

type alias Model =
  { email : String
  , emailState : Validation.State {}
  , username : String
  , usernameState : Validation.State { seed : Random.Seed }
  , message : Maybe String
  , state : Validation.State {}
  }

type Action
  = NoOp
  | SetEmail String
  | SetUsername String
  | BeginSubmit
  | EndSubmit (Model -> Model)
  | SetValidationState (Model -> Model)

init : (Model, Effects Action)
init =
  (,)
  { email = ""
  , emailState = { error = Nothing }
  , username = ""
  , usernameState = { seed = Random.initialSeed 0, error = Nothing }
  , message = Nothing
  , state = { error = Nothing }
  }
  Effects.none

validateEmail : (Model -> (Model, Maybe (Task Never (Model -> Model))))
validateEmail =
  Validation.validate
  .email .emailState (\state model -> {model | emailState = state })
  [ Validation.email " is invalid" ] -- sync validation
  []

-- note: throttling/debounce is done on the setUsernameMailbox.signal
checkUsernameExists =
  Validation.asyncValidate <|
  \oldState value ->
    let
      latencyRng = Random.float 25 250 -- simulate latency delay
      (latency, newSeed) = Random.generate latencyRng oldState.seed
    in
      Task.andThen (Task.sleep latency) <|
        \_ ->
          Task.succeed
          { oldState
          | seed = newSeed
          , error =
              if ((not (String.isEmpty value)) && value /= "test")
                then Just (" \"" ++ value ++ "\" is not registered. " ++ (toString latency))
                else Nothing
          }

validateUsername : (Model -> (Model, Maybe (Task Never (Model -> Model))))
validateUsername =
  Validation.validate
  .username .usernameState (\state model -> {model | usernameState = state })
  []
  [ checkUsernameExists ] -- async validation

validateUsernameOrEmail : (Model -> (Model, Maybe (Task Never (Model -> Model))))
validateUsernameOrEmail =
  Validation.validate
  (\model -> (model.username, model.email))
  .state
  (\state model -> { model | state = state })
  [ Validation.syncValidate
    (\(username, email) -> not ((String.isEmpty username) && (String.isEmpty email)))
    "Please enter your email or username."
  ]
  []

validateModel : (Model -> (Model, Maybe (Task Never (Model -> Model))))
validateModel =
  Validation.combine <|
  [ validateEmail
  , validateUsername
  , validateUsernameOrEmail
  ]

modelValidationStates : List (Model -> { error : Maybe String })
modelValidationStates =
  [ .emailState
  , \model -> { error = model.usernameState.error }
  , .state
  ]

isValidModel : Model -> Bool
isValidModel = Validation.isValidModel modelValidationStates

isInvalidModel : Model -> Bool
isInvalidModel = Validation.isInvalidModel modelValidationStates

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    SetEmail value ->
      Validation.combine
      [ validateEmail
      , validateUsernameOrEmail
      ] {model | email = value}
      |> Validation.toEffects SetValidationState
    SetUsername value ->
      Validation.combine
      [ validateUsername
      , validateUsernameOrEmail
      ] {model | username = value}
      |> Validation.toEffects SetValidationState
    BeginSubmit ->
      validateModel model
      |> Validation.toEffects EndSubmit
    EndSubmit setValidateState ->
      let
        newModel = setValidateState model
      in
        if isValidModel newModel
          then
            ({ newModel | message = Just "Submit: Valid" }, Effects.none)
          else
            ({ newModel | message = Just "Submit: Invalid" }, Effects.none)
    SetValidationState setValidateState ->
      (setValidateState model, Effects.none)
    NoOp ->
      (model, Effects.none)

onChangeEvent : List (Maybe String) -> String
onChangeEvent errors =
  if List.all (\error -> error == Nothing) errors
    then "blur"
    else "input"

view : Signal.Address Action -> Model -> Html
view address model =
  Html.form [ novalidate True ]
  [ node "link" [ href "./forgot-password.css", rel "stylesheet" ] []
  , h1 [] [ text "Forgot password" ]
  , p [] [ text "Please enter your email or username below to reset your password." ]
  , div
    [ class <| "field" ++ (if model.emailState.error == Nothing then "" else " error") ]
    [ label [] [ text <| "Email" ++ (Maybe.withDefault "" model.emailState.error) ]
    , input
      [ type' "email"
      -- , autofocus True -- removed to demonstrate submit validation 'invalid' state
      , placeholder "Enter your email address"
      , value model.email
      , on (onChangeEvent [model.emailState.error, model.state.error]) targetValue (Signal.message address << SetEmail)
      ] []
    ]
  , div [ style [("margin-bottom","0.5em")]] [ text "or" ]
  , div
    [ class <| "field" ++ (if model.usernameState.error == Nothing then "" else " error") ]
    [ label [] [ text <| "Username" ++ (Maybe.withDefault "" model.usernameState.error) ]
    , input
      [ type' "text"
      , placeholder "Enter your username"
      , value model.username
      , on (onChangeEvent [model.usernameState.error, model.state.error]) targetValue (Signal.message setUsernameMailbox.address << SetUsername)
      ] []
    ]
  , div [ class "field error" ] [ label [] [ text <| Maybe.withDefault "" model.state.error ] ]
  , button
    [ type' "button"
    , class "btn"
    , disabled <| model.message /= Nothing && (isInvalidModel model)
    , onClick address BeginSubmit
    ]
    [ text "Reset password" ]
  , div [] [ text <| Maybe.withDefault "" model.message ]
  ]

-- start app boilerplate

setUsernameMailbox : Signal.Mailbox Action
setUsernameMailbox = Signal.mailbox NoOp

app : StartApp.App Model
app =
  StartApp.start
  { init = init
  , update = update
  , view = view
  , inputs =
    [ settledAfter (250 *millisecond) setUsernameMailbox.signal
    ]
  }

main : Signal Html
main =
  app.html

port tasks : Signal (Task Never ())
port tasks =
  app.tasks

port title : String
port title = "shelakel/elm-validate examples"
