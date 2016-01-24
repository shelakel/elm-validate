module Validation
  ( State
  , isNotEmpty, notEmpty
  , isStringLengthBetween, stringLengthBetween
  , isValidEmail, email
  , syncValidate, asyncValidate, validate, combine
  ) where

{-| Validation library for elm supporting sync and async validation with state.
# Validation state
@docs State
# Validation functions
@docs isNotEmpty, isStringLengthBetween, isValidEmail
# Validators
@docs notEmpty, stringLengthBetween, email, syncValidate, asyncValidate
# Validation
@docs validate, combine
-}

import Task exposing (Task)
import Effects exposing (Effects,Never)
import Regex
import String

{-| State holds validation state.
-}
type alias State state = { state | error : Maybe String }

{-| isNotEmpty returns True when the given String value is not empty.
-}
isNotEmpty : String -> Bool
isNotEmpty = \value -> not <| String.isEmpty value

{-| notEmpty validates that the given String value is not empty.
-}
notEmpty :
  String -> (model -> String) -> (model -> State state) -> (model -> State state)
notEmpty = syncValidate isNotEmpty

{-| isStringLengthBetween returns True when the length of the String value
is between minLength and maxLength inclusive.
-}
isStringLengthBetween : Int -> Int -> String -> Bool
isStringLengthBetween minLength maxLength =
  \value ->
    let len = String.length value
    in len >= minLength && len <= maxLength

{-| stringLengthBetween validates that the given String value length is between
minlength and maxLength inclusive.
-}
stringLengthBetween : Int -> Int ->
  String -> (model -> String) -> (model -> State state) -> (model -> State state)
stringLengthBetween minLength maxLength =
  syncValidate <| isStringLengthBetween minLength maxLength

{-| reEmail is an email validation Regex.
-}
reEmail : Regex.Regex
reEmail = Regex.regex
 "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
 |> Regex.caseInsensitive

{-| isValidEmail returns True when the given String value is a valid email address.
-}
isValidEmail : String -> Bool
isValidEmail = Regex.contains reEmail

{-| email validates that the given String value is a valid email address.
-}
email :
    String -> (model -> String) -> (model -> State state) -> (model -> State state)
email = syncValidate (\value -> String.isEmpty value || (Regex.contains reEmail) value)

{-| regex validates that the given String value matches the Regex.
-}
regex : Regex.Regex ->
    String -> (model -> String) -> (model -> State state) -> (model -> State state)
regex re = syncValidate (Regex.contains re)

-- validation helpers

{-| syncValidate creates a new synchronous validation function
given a validation function (value -> Bool) for use in validate.
-}
syncValidate : (value -> Bool) ->
  String -> (model -> value) -> (model -> State state) -> (model -> State state)
syncValidate isValid errorMessage getValue getState =
  \model ->
    let oldState = getState model
    in {oldState | error = (if (not <| isValid <| getValue model) then Just errorMessage else Nothing)}

{-| asyncValidate creates a new asynchronous validation function
given a validation function (oldState -> value -> Task Never newState)
for use in validate.
-}
asyncValidate : ((State state) -> value -> Task Never (State state)) ->
  (model -> value) -> (model -> State state) -> (model -> Task Never (State state))
asyncValidate validate' =
  \getValue getState model -> validate' (getState model) (getValue model)

-- validation runner

{-| validate validates a model with synchronous and asynchronous validation
functions.

Asynchronous validation is done on the value at the point of validation and
a single effect (update validation state) is returned for chained validations.

The value is first validated using chained synchronous validation functions (first failure returned),
thereafter if the value is valid, asynchronous validation functions are chained (first failure returned).

validate produces an effect for asynchronous validation to update validation state.
-}
validate : ((model -> model) -> action) -- set validation state action
        -> (model -> value) -- get value
        -> (model -> State state) -- get state
        -> (State state -> model -> model) -- set state
        -> List ((model -> value) -> (model -> State state) -> (model -> State state)) -- validate sync
        -> List ((model -> value) -> (model -> State state) -> (model -> Task Never (State state))) -- validate async
        -> (model -> (model, Effects action))
validate setStateAction getValue getState setState syncValidators asyncValidators =
  \model ->
    let
      prepareValidator = List.map (\fn -> fn getValue getState)
      validateSync  = prepareValidator syncValidators
      validateAsync = prepareValidator asyncValidators
      runValidateSync =
        (\validate' model'' ->
          if (getState model'').error == Nothing
            then setState (validate' model'') model''
            else model''
        )
      oldState = getState model
      model' = -- validate synchronously after resetting state to valid
        List.foldl
          runValidateSync
          (setState {oldState | error = Nothing} model)
          validateSync
    in
      if (getState model').error == Nothing && List.length validateAsync > 0
        then
          let
            runValidateAsync =
              (\next curr ->
                \model1 -> Task.andThen (curr model1)
                  (\newState ->
                    if newState.error == Nothing
                      then (next (setState newState model1))
                      else Task.succeed newState
                  )
              )
            validateAsync' = List.foldl
              runValidateAsync
              (Maybe.withDefault
                (\model1 -> Task.succeed <| getState model1)
                (List.head validateAsync)
              )
              (Maybe.withDefault [] (List.tail validateAsync))
          in
            ( model'
            , validateAsync' model' |> Effects.task |> Effects.map
              (\newState -> setStateAction <| setState newState)
            )
        else
          (model', Effects.none)

{-| combine combines one or more validation functions.
-}
combine : List (model -> (model, Effects action))
             -> (model -> (model, Effects action))
combine validators =
  \model ->
    List.foldl
    (\validator (model', action) ->
      let (newModel, newAction) = validator model'
      in (newModel, Effects.batch [action, newAction])
    )
    (model |> Maybe.withDefault
      (\model' -> (model', Effects.none))
      (List.head validators)
    )
    (Maybe.withDefault [] (List.tail validators))
