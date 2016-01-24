# elm-validate

Validation library for elm supporting sync and async validation with state.

## Current version

1.0.0

## Examples

See [examples](https://github.com/shelakel/elm-validate/tree/master/examples)

```elm
import Validation

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
```

## Issues

Please open a ticket.

## License

See LICENSE (MIT).
