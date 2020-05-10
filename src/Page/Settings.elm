module Page.Settings exposing (Model, Msg, init, view, update)

{-| User settings.

@docs Model, Msg, init, view, update

-}

import Api exposing (Cred)
import Author exposing (Settings)
import Effect exposing (Effect)
import Errors exposing (Errors)
import Html exposing (Html)
import Html.Events as Events
import Remote exposing (Remote)
import Session exposing (Session)
import View
import View.Button



-- MODEL


{-| -}
type alias Model =
    { errors : Errors
    , settings : Remote Settings
    }


{-| -}
init : ( Model, Effect Msg )
init =
    ( { errors = Errors.none
      , settings = Remote.loading "user settings"
      }
    , Effect.fetchSettings CompletedSettingsLoad
    )



-- VIEW


{-| -}
view : Session -> Model -> { title : String, content : Html Msg }
view session model =
    { title = "Settings"
    , content =
        case Session.credentials session of
            Just cred ->
                View.settings model.errors <|
                    Remote.view (viewSettings cred) model.settings

            Nothing ->
                Html.text "Sign in to view your settings."
    }


viewSettings : Cred -> Settings -> Html Msg
viewSettings cred user =
    Html.form [ Events.onSubmit (SubmittedSettings cred user) ]
        [ Html.fieldset []
            [ View.textInput "URL of profile picture" EnteredAvatar user.avatar
            , View.textInput "Settingsname" EnteredSettingsname user.name
            , View.multilineInput "Short bio about you" EnteredBio user.bio
            , View.textInput "Email" EnteredEmail user.email
            , View.passwordInput "Password" EnteredPassword user.password
            , View.Button.submit "Update Settings" []
            ]
        ]



-- UPDATE


{-| -}
type Msg
    = SubmittedSettings Cred Settings
    | EnteredEmail String
    | EnteredSettingsname String
    | EnteredPassword String
    | EnteredBio String
    | EnteredAvatar String
    | CompletedSettingsLoad (Result Errors Settings)
    | CompletedSave (Result Errors Session)


{-| -}
update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        CompletedSettingsLoad (Ok settings) ->
            ( { model | settings = Remote.loaded settings model.settings }
            , Effect.none
            )

        CompletedSettingsLoad (Err _) ->
            ( { model | settings = Remote.failed model.settings }
            , Effect.none
            )

        SubmittedSettings cred settings ->
            ( { model | settings = Remote.loaded settings model.settings }
            , Effect.updateSettings CompletedSave cred settings
            )

        EnteredEmail email ->
            updateSettings (\settings -> { settings | email = email }) model

        EnteredSettingsname name ->
            updateSettings (\settings -> { settings | name = name }) model

        EnteredPassword password ->
            updateSettings (\settings -> { settings | password = password }) model

        EnteredBio bio ->
            updateSettings (\settings -> { settings | bio = bio }) model

        EnteredAvatar avatar ->
            updateSettings (\settings -> { settings | avatar = avatar }) model

        CompletedSave (Err errors) ->
            ( { model | errors = Errors.prepend errors model.errors }
            , Effect.none
            )

        CompletedSave (Ok user) ->
            ( model
            , Effect.login user
            )


{-| Helper function for `update`. Updates the user and returns Effect.none.
Useful for recording fields!
-}
updateSettings : (Settings -> Settings) -> Model -> ( Model, Effect msg )
updateSettings transform model =
    case Remote.get model.settings of
        Just settings ->
            ( { model | settings = Remote.loaded (transform settings) model.settings }
            , Effect.none
            )

        Nothing ->
            ( model, Effect.logError )
