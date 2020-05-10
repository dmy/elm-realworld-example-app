module Page.Article.Editor exposing (Model, Msg, initNew, initEdit, view, update)

{-| Editing an individual article.

@docs Model, Msg, initNew, initEdit, view, update

-}

import Api exposing (Cred)
import Article exposing (Article, Full)
import Article.Body
import Article.Slug as Slug exposing (Slug)
import Effect exposing (Effect)
import Errors exposing (Errors)
import Html exposing (Html)
import Html.Attributes exposing (disabled)
import Html.Events as Events
import Route
import Session exposing (Session)
import View
import View.Button



-- MODEL


{-| -}
type Model
    = Loading Slug
    | LoadingFailed Slug
    | Saving Slug Form
    | Editing Slug Errors Form
      -- New Article
    | EditingNew Errors Form
    | Creating Form


type alias Form =
    { title : String
    , body : String
    , description : String
    , tags : String
    }


{-| Initialize the page with a new article.
-}
initNew : ( Model, Effect msg )
initNew =
    ( EditingNew Errors.none { title = "", body = "", description = "", tags = "" }
    , Effect.none
    )


{-| Initialize the page from an existing article.
-}
initEdit : Slug -> ( Model, Effect Msg )
initEdit slug =
    ( Loading slug
    , Effect.batch
        [ -- If init fails, store the slug that failed in the msg, so we can
          -- at least have it later to display the page's title properly!
          Effect.fetchArticle (Result.mapError (Tuple.pair slug) >> CompletedArticleLoad) slug
        ]
    )



-- VIEW


{-| -}
view : Session -> Model -> { title : String, content : Html Msg }
view session model =
    { title =
        case getSlug model of
            Just slug ->
                "Edit Article - " ++ Slug.toString slug

            Nothing ->
                "New Article"
    , content =
        case Session.credentials session of
            Just cred ->
                viewAuthenticated cred model

            Nothing ->
                Html.text "Sign in to edit this article."
    }


viewAuthenticated : Cred -> Model -> Html Msg
viewAuthenticated cred model =
    View.editor <|
        case model of
            Loading _ ->
                [ View.loading "article" ]

            Saving _ form ->
                [ viewForm cred form (editArticleSaveButton [ disabled True ]) ]

            Creating form ->
                [ viewForm cred form (newArticleSaveButton [ disabled True ]) ]

            Editing _ errors form ->
                [ View.errors errors
                , viewForm cred form (editArticleSaveButton [])
                ]

            EditingNew errors form ->
                [ View.errors errors
                , viewForm cred form (newArticleSaveButton [])
                ]

            LoadingFailed _ ->
                [ Html.text "Error loading article." ]


viewForm : Cred -> Form -> Html Msg -> Html Msg
viewForm cred form saveButton =
    Html.form [ Events.onSubmit (ClickedSave cred) ]
        [ Html.fieldset []
            [ View.textInput "Article Title" EnteredTitle form.title
            , View.textInput "What's this article about?" EnteredDescription form.description
            , View.multilineInput "Write your article (in markdown)" EnteredBody form.body
            , View.textInput "Enter tags" EnteredTags form.tags
            , saveButton
            ]
        ]


editArticleSaveButton : List (Html.Attribute msg) -> Html msg
editArticleSaveButton extraAttrs =
    View.Button.submit "Update Article" extraAttrs


newArticleSaveButton : List (Html.Attribute msg) -> Html msg
newArticleSaveButton extraAttrs =
    View.Button.submit "Publish Article" extraAttrs



-- UPDATE


{-| -}
type Msg
    = ClickedSave Cred
    | EnteredBody String
    | EnteredDescription String
    | EnteredTags String
    | EnteredTitle String
    | CompletedCreate (Result Errors (Article Full))
    | CompletedEdit (Result Errors (Article Full))
    | CompletedArticleLoad (Result ( Slug, Errors ) (Article Full))


{-| -}
update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ClickedSave cred ->
            save cred model

        EnteredTitle title ->
            updateForm (\form -> { form | title = title }) model

        EnteredDescription description ->
            updateForm (\form -> { form | description = description }) model

        EnteredTags tags ->
            updateForm (\form -> { form | tags = tags }) model

        EnteredBody body ->
            updateForm (\form -> { form | body = body }) model

        CompletedCreate (Ok article) ->
            ( model
            , Route.Article (Article.slug article)
                |> Effect.replaceUrl
            )

        CompletedCreate (Err errors) ->
            ( savingErrors errors model
            , Effect.none
            )

        CompletedEdit (Ok article) ->
            ( model
            , Effect.replaceUrl (Route.Article (Article.slug article))
            )

        CompletedEdit (Err error) ->
            ( savingErrors error model
            , Effect.none
            )

        CompletedArticleLoad (Err ( slug, _ )) ->
            ( LoadingFailed slug
            , Effect.logError
            )

        CompletedArticleLoad (Ok article) ->
            let
                { title, description, tags } =
                    Article.metadata article
            in
            ( Editing (Article.slug article)
                Errors.none
                { title = title
                , body = Article.Body.toMarkdownString (Article.body article)
                , description = description
                , tags = String.join " " tags
                }
            , Effect.none
            )


save : Cred -> Model -> ( Model, Effect Msg )
save cred model =
    case model of
        Editing slug _ form ->
            ( Saving slug form
            , Effect.editArticle CompletedEdit cred slug form
            )

        EditingNew _ form ->
            ( Creating form
            , Effect.createArticle CompletedCreate cred form
            )

        _ ->
            -- We're in a state where saving is not allowed.
            -- We tried to prevent getting here by disabling the Save
            -- button, but somehow the user got here anyway!
            ( model, Effect.logError )


savingErrors : Errors -> Model -> Model
savingErrors errors model =
    case model of
        Saving slug form -> Editing slug errors form
        Creating form -> EditingNew errors form
        _ -> model


{-| Helper function for `update`. Updates the form, if there is one,
and returns Effect.none.

Useful for recording form fields!

This could also log errors to the server if we are trying to record things in
the form and we don't actually have a form.

-}
updateForm : (Form -> Form) -> Model -> ( Model, Effect Msg )
updateForm transform model =
    ( case model of
        Loading _ -> model
        LoadingFailed _ -> model
        Saving slug form -> Saving slug (transform form)
        Editing slug errors form -> Editing slug errors (transform form)
        EditingNew errors form -> EditingNew errors (transform form)
        Creating form -> Creating (transform form)
    , Effect.none
    )



-- INTERNAL


{-| Used for setting the page's title.
-}
getSlug : Model -> Maybe Slug
getSlug model =
    case model of
        Loading slug -> Just slug
        LoadingFailed slug -> Just slug
        Saving slug _ -> Just slug
        Editing slug _ _ -> Just slug
        EditingNew _ _ -> Nothing
        Creating _ -> Nothing
