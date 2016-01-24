module ForgotPassword where

import Validation

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects exposing (Effects,Never)
import Task exposing (Task)
import String
import Random
import StartApp

-- curry Validation.validate to capture set validation state action
validate = Validation.validate (\transform -> SetValidationState (\model -> (transform model, Effects.none)))

type alias Model =
  { email : String
  , emailState : Validation.State {}
  , username : String
  , usernameState : Validation.State { seed : Random.Seed }
  , state : Validation.State {}
  }

type Action
  = NoOp
  | SetEmail String
  | SetUsername String
  | Submit
  | SetValidationState (Model -> (Model, Effects Action))

init : (Model, Effects Action)
init =
  (,)
  { email = ""
  , emailState = { error = Nothing }
  , username = ""
  , usernameState = { seed = Random.initialSeed 0, error = Nothing }
  , state = { error = Nothing }
  }
  Effects.none

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

validateUsernameOrEmail =
  validate
  (\model -> (model.username, model.email))
  .state
  (\state model -> { model | state = state })
  [ Validation.syncValidate
    (\(username, email) -> not ((String.isEmpty username) && (String.isEmpty email)))
    "Please enter your email or username."
  ]
  []

validateModel : (Model -> (Model, Effects Action))
validateModel =
  Validation.combine <|
  [ validateEmail
  , validateUsername
  , validateUsernameOrEmail
  ]

validateEmail =
  validate .email .emailState (\state model -> {model | emailState = state })
  [ Validation.email " is invalid" ] -- sync validation
  []

-- todo: implement throttling
checkUsernameExists =
  Validation.asyncValidate <|
  \oldState value ->
    -- simulate latency delay
    let
      latencyRng = Random.float 25 250
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

validateUsername =
  validate .username .usernameState (\state model -> {model | usernameState = state })
  []
  [ checkUsernameExists ] -- async validation

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    SetEmail value ->
      Validation.combine
      [ validateEmail
      , validateUsernameOrEmail
      ] {model | email = value}
    SetUsername value ->
      Validation.combine
      [ validateUsername
      , validateUsernameOrEmail
      ] {model | username = value}
    SetValidationState transform ->
      transform model
    Submit ->
      (model, Effects.none)
    NoOp ->
      (model, Effects.none)

onChangeEvent : (Validation.State state) -> String
onChangeEvent state =
  if state.error == Nothing
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
      , autofocus True
      , placeholder "Enter your email address"
      , value model.email
      , on (onChangeEvent model.emailState) targetValue (Signal.message address << SetEmail)
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
      , on (onChangeEvent model.usernameState) targetValue (Signal.message address << SetUsername)
      ] []
    ]
  , div [ class "field error" ] [ label [] [ text <| Maybe.withDefault "" model.state.error ] ]
  , button
    [ type' "button"
    , class "btn"
    , disabled <| isInvalidModel model
    , onClick address Submit
    ]
    [ text "Reset password" ]
  ]

-- start app boilerplate

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = [actions.signal]
    }

main : Signal Html
main =
  app.html

port tasks : Signal (Task Never ())
port tasks =
  app.tasks

port title : String
port title = "shelakel/elm-validate examples"
