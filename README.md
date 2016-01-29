# elm-validate

Validation library for elm supporting sync and async validation with state.

## Current version

3.0.0 (WIP)

## Examples

See [examples](https://github.com/shelakel/elm-validate/tree/master/examples)

```elm
import Validation

type alias Model =
  { email : String
  , emailState : Validation.State {}
  }

type Action
  = NoOp
  | SetEmail String
  | SetValidationState (Model -> Model)

init : (Model, Effects Action)
init =
  (,)
  { email = ""
  , emailState = { error = Nothing }
  }
  Effects.none

validateEmail : Validation.Validator
validateEmail =
    Validation.validator
        .email
        .emailState
        (\state model -> { model | emailState = state })
        -- sync validation
        [ Validation.email "Email is invalid"
        , Validation.basic (\state value -> value /= "test@test") "Email can't be test@test"
        ]
        -- async validation
        []

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    SetEmail value ->
        validateEmail { model | email = value }
          |> Validation.toEffects SetValidationState

    SetValidationState setValidateState ->
        ( setValidateState model, Effects.none )

    NoOp ->
      (model, Effects.none)
```

## Issues

Please open a ticket.

## License

See LICENSE (MIT).
