module Page.Register exposing (Model, Msg, init, view, update)

{-| Registering a new user.

@docs Model, Msg, init, view, update

-}

import Effect exposing (Effect)
import Errors exposing (Errors)
import Html exposing (Html)
import Html.Events as Events
import Session exposing (Session)
import View
import View.Button



-- MODEL


{-| -}
type alias Model =
    { errors : Errors
    , form : Form
    }


type alias Form =
    { email : String
    , username : String
    , password : String
    }


{-| -}
init : ( Model, Effect msg )
init =
    ( { errors = Errors.none
      , form = { email = "", username = "", password = "" }
      }
    , Effect.none
    )



-- VIEW


{-| -}
view : Model -> { title : String, content : Html Msg }
view { form, errors } =
    { title = "Register"
    , content =
        View.register errors <|
            Html.form [ Events.onSubmit SubmittedForm ]
                [ View.textInput "Username" EnteredUsername form.username
                , View.textInput "Email" EnteredEmail form.email
                , View.passwordInput "Password" EnteredPassword form.password
                , View.Button.submit "Sign up" []
                ]
    }



-- UPDATE


{-| -}
type Msg
    = SubmittedForm
    | EnteredEmail String
    | EnteredUsername String
    | EnteredPassword String
    | CompletedRegister (Result Errors Session)


{-| -}
update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SubmittedForm ->
            ( { model | errors = Errors.none }
            , Effect.register CompletedRegister model.form
            )

        EnteredUsername username ->
            updateForm (\form -> { form | username = username }) model

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        CompletedRegister (Err errors) ->
            ( { model | errors = errors }
            , Effect.none
            )

        CompletedRegister (Ok user) ->
            ( model
            , Effect.login user
            )


{-| Helper function for `update`. Updates the form and returns Cmd.none.
Useful for recording form fields!
-}
updateForm : (Form -> Form) -> Model -> ( Model, Effect msg )
updateForm transform model =
    ( { model | form = transform model.form }, Effect.none )
