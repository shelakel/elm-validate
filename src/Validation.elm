module Validation (State, SyncValidator, AsyncValidator, Validator, isNotEmpty, notEmpty, isStringLengthBetween, stringLengthBetween, isValidEmail, email, basic, isValid, isInvalid, isValidModel, isInvalidModel, listModelErrors, combine, validator, toEffects) where

{-| Validation library for elm supporting sync and async validation with state.
# Validation state
@docs State
# Validation functions
@docs isNotEmpty, isStringLengthBetween, isValidEmail
# Validators
@docs notEmpty, stringLengthBetween, email, basic, SyncValidator, AsyncValidator
# Validation
@docs combine, validator, Validator
# Validation helpers
@docs isValid, isInvalid, isValidModel, isInvalidModel, listModelErrors, toEffects
-}

import Effects exposing (Effects, Never)
import Regex
import String
import Task exposing (Task)


{-| State holds validation state.
-}
type alias State state =
    { state | error : Maybe String }


{-| SyncValidator does synchronous validation.
-}
type alias SyncValidator state value =
    State state -> value -> State state


{-| AsyncValidator does asynchronous validation.
-}
type alias AsyncValidator state value =
    State state -> value -> Task Never (State state)


{-| Validator takes a model, performs synchronous and asynchronous validation
and procudes a model with updated validation state and possibly a deffered model transform
to update asynchronous validation state.
-}
type alias Validator model =
    model -> ( model, Maybe (Task Never (model -> model)) )


{-| reEmail is an email validation Regex.
-}
reEmail : Regex.Regex
reEmail =
    Regex.regex
        "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
        |> Regex.caseInsensitive



-- validation functions


{-| isNotEmpty returns True when the given String value is not empty.
-}
isNotEmpty : String -> Bool
isNotEmpty =
    not << String.isEmpty


{-| isStringLengthBetween returns True when the length of the String value
is between minLength and maxLength inclusive.
-}
isStringLengthBetween : Int -> Int -> String -> Bool
isStringLengthBetween minLength maxLength value =
    let
        len = String.length value
    in
        len >= minLength && len <= maxLength


{-| isValidEmail returns True when the given String value is a valid email address.
-}
isValidEmail : String -> Bool
isValidEmail =
    Regex.contains reEmail



-- basic validators


{-| notEmpty validates that the given String value is not empty.
-}
notEmpty : String -> State state -> String -> State state
notEmpty =
    basic isNotEmpty


{-| stringLengthBetween validates that the given String value length is between
minlength and maxLength inclusive.
-}
stringLengthBetween : Int -> Int -> String -> State state -> String -> State state
stringLengthBetween minLength maxLength =
    basic <| isStringLengthBetween minLength maxLength


{-| regex validates that the given String value matches the Regex.
-}
regex : Regex.Regex -> String -> State state -> String -> State state
regex re =
    basic (Regex.contains re)


{-| email validates that the given String value is a valid email address.
-}
email : String -> State state -> String -> State state
email =
    basic
        <| \x -> String.isEmpty x || isValidEmail x


{-| basic does value based, synchronous validation. \
-}
basic : (value -> Bool) -> String -> SyncValidator state value
basic isValid errorMessage oldState value =
    if isValid value then
        oldState
    else
        { oldState | error = Just errorMessage }



-- helper functions


{-| isValid tests if the validation state has no error.
-}
isValid : State state -> Bool
isValid state =
    state.error == Nothing


{-| isInvalid tests if the validation state has an error.
-}
isInvalid : State state -> Bool
isInvalid =
    not << isValid


{-| isValidModel, given a model, tests if all the validation states are valid.
-}
isValidModel : List (model -> State state) -> model -> Bool
isValidModel states model =
    List.map (\getState -> getState model) states
        |> List.all isValid


{-| isInvalidModel, given a model, tests if any validation states are invalid.
-}
isInvalidModel : List (model -> State state) -> model -> Bool
isInvalidModel states =
    not << isValidModel states


{-| listModelErrors, given a model, extracts a list of errors for invalid validation states.
-}
listModelErrors : List (model -> State state) -> model -> Maybe (List String)
listModelErrors states model =
    let
        errors =
            List.filterMap
                (\state -> state.error)
                (List.map (\getState -> getState model) states)
    in
        if List.length errors > 0 then
            Just errors
        else
            Nothing



-- validation


{-| validator creates a validator to validate a model using synchronous and asynchronous validation functions.

Order of validation:
  1. Synchronous validation is done in sequence and returns on the first error encountered.
  2. If synchronous validation deemed the value invalid, then the new validation state is returned and no asynchronous validation is done.
  3. Otherwise, asynchronous validation is done in sequence on the value at the point of validation, starting with the new validation state, and returns on the first error encountered.
  - When asynchronous validation needs to be done, the previous error will be kept to avoid flashing of valid status when used with debouncing. E.g. checking if a username is available
-}
validator : (model -> value) -> (model -> State state) -> (State state -> model -> model) -> List (SyncValidator state value) -> List (AsyncValidator state value) -> Validator model
validator getValue getState setState syncValidators asyncValidators model =
    let
        state = getState model

        value = getValue model

        state' = validateSync value syncValidators { state | error = Nothing }
    in
        if isValid state' && List.length asyncValidators > 0 then
            let
                -- retain the old error when validating async to avoid the flash
                -- when previous async validation state was invalid,
                -- the current synchronous validation state is invalid
                -- and the next async validation state is invalid
                state'' = { state' | error = state.error }
            in
                ( setState state'' model
                , Just <| Task.map setState (validateAsync value asyncValidators (Task.succeed state'))
                )
        else
            ( setState state' model
            , Nothing
            )


validateSync : value -> List (SyncValidator state value) -> State state -> State state
validateSync value validators state =
    case validators of
        [] ->
            state

        validate' :: validators' ->
            let
                state' = validate' state value
            in
                if isValid state' then
                    validateSync value validators' state'
                else
                    state'


validateAsync : value -> List (AsyncValidator state value) -> Task Never (State state) -> Task Never (State state)
validateAsync value validators ts =
    case validators of
        [] ->
            ts

        validate' :: validators' ->
            ts
                `Task.andThen` \state ->
                                if isValid state then
                                    validateAsync value validators' (validate' state value)
                                else
                                    Task.succeed state


{-| combine combines one or more validation functions and runs
async validations in sequence.
-}
combine : List (Validator model) -> Validator model
combine validators model =
    combineHelper validators model []


combineHelper : List (Validator model) -> model -> List (Task Never (model -> model)) -> ( model, Maybe (Task Never (model -> model)) )
combineHelper validators model tasks =
    case validators of
        [] ->
            let
                ts =
                    if List.length tasks > 0 then
                        Just
                            <| (Task.sequence tasks)
                            `Task.andThen` \transforms ->
                                            Task.succeed
                                                <| \model' ->
                                                    List.foldr
                                                        (\t m -> t m)
                                                        model'
                                                        transforms
                    else
                        Nothing
            in
                ( model, ts )

        validate' :: validators' ->
            let
                ( model', task ) = validate' model
            in
                combineHelper validators' model'
                    <| case task of
                        Just ts ->
                            ts :: tasks

                        Nothing ->
                            tasks


{-| toEffects transforms a validation result to a (model, Effects action),
mapping the model transform to an action.
-}
toEffects : ((model -> model) -> action) -> ( model, Maybe (Task Never (model -> model)) ) -> ( model, Effects action )
toEffects transformAction ( model, maybeTask ) =
    ( model
    , case maybeTask of
        Just task ->
            Task.map transformAction task |> Effects.task

        Nothing ->
            Effects.none
    )
