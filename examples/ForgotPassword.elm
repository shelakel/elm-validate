module ForgotPassword where

import Validation

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects exposing (Effects,Never)
import Task exposing (Task)
import StartApp

-- curry Validation.validate to capture set validation state action
validate = Validation.validate (\transform -> SetValidationState (\model -> (transform model, Effects.none)))

type alias Model =
  { email : String
  , emailState : Validation.State {}
  }

type Action
  = NoOp
  | SetEmail String
  | SetValidationState (Model -> (Model, Effects Action))

init : (Model, Effects Action)
init =
  (,)
  { email = ""
  , emailState = { error = Nothing }
  }
  Effects.none

validateEmail =
  validate .email .emailState (\state model -> {model | emailState = state })
  [ Validation.notEmpty " is required"
  , Validation.email " is invalid"
  ] []

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    SetEmail value ->
      validateEmail {model | email = value}
    SetValidationState transform ->
      transform model
    NoOp ->
      (model, Effects.none)

onChangeEvent : (Validation.State state) -> String
onChangeEvent state =
  if state.error == Nothing
    then "blur"
    else "input"

submitDisabled : Model -> Bool
submitDisabled model = model.emailState.error /= Nothing

view : Signal.Address Action -> Model -> Html
view address model =
  Html.form [ novalidate True ]
  [ node "link" [ href "./forgot-password.css", rel "stylesheet" ] []
  , h1 [] [ text "Forgot password" ]
  , p [] [ text "Please enter your email below to reset your password." ]
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
  , button
    [ type' "button"
    , class "btn"
    , disabled <| submitDisabled model
    , Html.Attributes.title (if submitDisabled model then "Please correct the errors on the form" else "")
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
