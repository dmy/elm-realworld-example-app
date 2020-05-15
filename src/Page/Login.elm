module Page.Login exposing (Model, Msg, init, view, update, subscriptions)

{-| The login page.

@docs Model, Msg, init, view, update, subscriptions

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
    , password : String
    }


{-| -}
init : ( Model, Effect msg )
init =
    ( { errors = Errors.none
      , form = { email = "", password = "" }
      }
    , Effect.none
    )



-- VIEW


{-| -}
view : Model -> { title : String, content : Html Msg }
view { form, errors } =
    { title = "Login"
    , content =
        View.login errors <|
            Html.form [ Events.onSubmit SubmittedForm ]
                [ View.textInput "Email" EnteredEmail form.email
                , View.passwordInput "Password" EnteredPassword form.password
                , View.Button.submit "Sign in" []
                ]
    }



-- UPDATE


{-| -}
type Msg
    = SubmittedForm
    | EnteredEmail String
    | EnteredPassword String
    | CompletedAuthentication (Result Errors Session)


{-| -}
update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SubmittedForm ->
            ( { model | errors = Errors.none }
            , Effect.authenticate CompletedAuthentication model.form
            )

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        CompletedAuthentication (Err errors) ->
            ( { model | errors = errors }
            , Effect.none
            )

        CompletedAuthentication (Ok session) ->
            ( model
            , Effect.login session
            )


{-| Helper function for `update`. Updates the form and returns Effect.none.
Useful for recording form fields!
-}
updateForm : (Form -> Form) -> Model -> ( Model, Effect Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Effect.none )



-- SUBSCRIPTIONS


{-| Login subscriptions
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
