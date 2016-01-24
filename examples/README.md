# Examples

## Running examples

 1. [Install elm if you haven't](http://elm-lang.org/install)
 2. Clone the repository: `git clone https://github.com/shelakel/elm-validate.git`
 3. `cd elm-validate`
 4. Install the dependencies for the main package: elm make
 5. `cd examples`
 6. Install the dependencies for the examples: elm make
 7. Run elm reactor: `elm reactor -a=localhost`
 8. Open up [localhost:8000] (http://localhost:8000/) to run examples

## Forgot password

The forgot password example demonstrates:

### Creating custom validators

 - Creating a custom synchronous validator (validateUsernameOrEmail)
 - Creating a custom asynchronous validator (checkUsernameExists)

### Field, non-field and model validation

 - Synchronous field validation (validateEmail)
 - Asynchronous field validation (validateUsername)
 - Non-field combination type validation (validateUsernameOrEmail)
 - Model validation (validateModel) (WIP)

### Custom validation state

 - Storing custom validation state (checkUsernameExists)
 - Throttling of requests (TO DO)
