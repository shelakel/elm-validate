# elm-validate

Validation library for elm supporting sync and async validation with state.

## Current version

2.1.0 (WIP)

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

validateEmail : Model -> ( Model, Maybe (Task Never (Model -> Model)) )
validateEmail =
    Validation.validate
        .email
        .emailState
        (\state model -> { model | emailState = state })
        [ Validation.email " is invalid" ]
        -- sync validation
        []

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    SetEmail value ->
      Validation.combine
          validateEmail
          { model | email = value }
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
