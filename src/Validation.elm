module Validation (State, isNotEmpty, notEmpty, isStringLengthBetween, stringLengthBetween, isValidEmail, email, syncValidate, asyncValidate, isValid, isInvalid, isValidModel, isInvalidModel, listModelErrors, combine, validate, toEffects) where

{-| Validation library for elm supporting sync and async validation with state.
# Validation state
@docs State
# Validation functions
@docs isNotEmpty, isStringLengthBetween, isValidEmail
# Validators
@docs notEmpty, stringLengthBetween, email, syncValidate, asyncValidate
# Validation
@docs combine, validate
# Validation helpers
@docs isValid, isInvalid, isValidModel, isInvalidModel, listModelErrors, toEffects
-}

import Task exposing (Task)
import Effects exposing (Effects, Never)
import Regex
import String


{-| State holds validation state.
-}
type alias State state =
    { state | error : Maybe String }


{-| isNotEmpty returns True when the given String value is not empty.
-}
isNotEmpty : String -> Bool
isNotEmpty x =
    not (String.isEmpty x)


{-| notEmpty validates that the given String value is not empty.
-}
notEmpty : String -> (model -> String) -> (model -> State state) -> model -> State state
notEmpty =
    syncValidate isNotEmpty


{-| isStringLengthBetween returns True when the length of the String value
is between minLength and maxLength inclusive.
-}
isStringLengthBetween : Int -> Int -> String -> Bool
isStringLengthBetween minLength maxLength =
    \x ->
        let
            len = String.length x
        in
            len >= minLength && len <= maxLength


{-| stringLengthBetween validates that the given String value length is between
minlength and maxLength inclusive.
-}
stringLengthBetween : Int -> Int -> String -> (model -> String) -> (model -> State state) -> model -> State state
stringLengthBetween minLength maxLength =
    syncValidate <| isStringLengthBetween minLength maxLength


{-| reEmail is an email validation Regex.
-}
reEmail : Regex.Regex
reEmail =
    Regex.regex
        "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
        |> Regex.caseInsensitive


{-| isValidEmail returns True when the given String value is a valid email address.
-}
isValidEmail : String -> Bool
isValidEmail =
    Regex.contains reEmail


{-| email validates that the given String value is a valid email address.
-}
email : String -> (model -> String) -> (model -> State state) -> model -> State state
email =
    syncValidate (\x -> String.isEmpty x || (Regex.contains reEmail) x)


{-| regex validates that the given String value matches the Regex.
-}
regex : Regex.Regex -> String -> (model -> String) -> (model -> State state) -> model -> State state
regex re =
    syncValidate (Regex.contains re)



-- validation helpers


{-| isValid tests if the validation state has no error.
-}
isValid : { error : Maybe String } -> Bool
isValid state =
    state.error == Nothing


{-| isValidModel, given a model, tests if all the validation states are valid.
-}
isValidModel : List (model -> { error : Maybe String }) -> model -> Bool
isValidModel states =
    \model ->
        List.map (\getState -> getState model) states
            |> List.all (\state -> state.error == Nothing)


{-| isInvalid tests if the validation state has an error.
-}
isInvalid : { error : Maybe String } -> Bool
isInvalid state =
    not (isValid state)


{-| isInvalidModel, given a model, tests if any validation states are invalid.
-}
isInvalidModel : List (model -> { error : Maybe String }) -> model -> Bool
isInvalidModel states model =
    not (isValidModel states model)


{-| listModelErrors, given a model, extracts a list of errors for invalid validation states.
-}
listModelErrors : List (model -> { error : Maybe String }) -> model -> Maybe (List String)
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


{-| syncValidate creates a new synchronous validation function
given a validation function (value -> Bool) for use in validate.
-}
syncValidate : (value -> Bool) -> String -> (model -> value) -> (model -> State state) -> model -> State state
syncValidate isValid errorMessage getValue getState =
    \model ->
        let
            oldState = getState model
        in
            { oldState
                | error =
                    if (not (isValid (getValue model))) then
                        Just errorMessage
                    else
                        Nothing
            }


{-| asyncValidate creates a new asynchronous validation function
given a validation function (oldState -> value -> Task Never newState)
for use in validate.
-}
asyncValidate : (State state -> value -> Task Never (State state)) -> (model -> value) -> (model -> State state) -> model -> Task Never (State state)
asyncValidate validate' =
    \getValue getState model ->
        validate' (getState model) (getValue model)



-- validation runner


{-| validate validates a model with synchronous and asynchronous validation
functions.

Asynchronous validation is done on the value at the point of validation and
a single effect (update validation state) is returned for chained validations.

The value is first validated using chained synchronous validation functions (first failure returned),
thereafter if the value is valid, asynchronous validation functions are chained (first failure returned).
-}
validate : (model -> value) -> (model -> State state) -> (State state -> model -> model) -> List ((model -> value) -> (model -> State state) -> model -> State state) -> List ((model -> value) -> (model -> State state) -> model -> Task Never (State state)) -> model -> ( model, Maybe (Task Never (model -> model)) )
validate getValue getState setState syncValidators asyncValidators =
    \model ->
        let
            prepareValidator = List.map (\fn -> fn getValue getState)

            validateSync = prepareValidator syncValidators

            validateAsync = prepareValidator asyncValidators

            runValidateSync =
                (\validate' model'' ->
                    if (getState model'').error == Nothing then
                        setState (validate' model'') model''
                    else
                        model''
                )

            oldState = getState model

            model' =
                -- validate synchronously after resetting state to valid
                List.foldl
                    runValidateSync
                    (setState { oldState | error = Nothing } model)
                    validateSync
        in
            if (getState model').error == Nothing && List.length validateAsync > 0 then
                let
                    runValidateAsync =
                        (\next curr ->
                            \model1 ->
                                (curr model1)
                                    `Task.andThen` \newState ->
                                                    if newState.error == Nothing then
                                                        (next (setState newState model1))
                                                    else
                                                        Task.succeed newState
                        )

                    validateAsync' =
                        List.foldl
                            runValidateAsync
                            (Maybe.withDefault
                                (\model1 -> Task.succeed <| getState model1)
                                (List.head validateAsync)
                            )
                            (Maybe.withDefault [] (List.tail validateAsync))
                in
                    ( model'
                    , Just <| Task.map setState <| validateAsync' model'
                    )
            else
                ( model', Nothing )


{-| combine combines one or more validation functions and runs
async validations in sequence.
-}
combine : List (model -> ( model, Maybe (Task Never (model -> model)) )) -> model -> ( model, Maybe (Task Never (model -> model)) )
combine validators =
    \model' ->
        let
            ( model, tasks ) =
                List.foldl
                    (\validate ( model, tasks ) ->
                        let
                            ( newModel, newTask ) = validate model
                        in
                            ( newModel
                            , case newTask of
                                Just task ->
                                    task :: tasks

                                Nothing ->
                                    tasks
                            )
                    )
                    ( model', [] )
                    validators
        in
            if List.length tasks == 0 then
                ( model, Nothing )
            else
                ( model
                , Just
                    <| (Task.sequence tasks)
                    `Task.andThen` \tasks' ->
                                    Task.succeed
                                        <| \model' ->
                                            -- transform the model
                                            List.foldr
                                                (\t m -> t m)
                                                model'
                                                tasks'
                )



{- -
todo: combine should execute tasks in parallel instead of in sequence
because validators should own non-shared state and return state transforms.
-
-}


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
