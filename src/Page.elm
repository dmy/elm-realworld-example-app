module Page exposing
    ( Msg, Page
    , blank
    , changeRouteTo
    , view, mapDocument
    , update
    )

{-| This module is responsible to handle subpages.


# Types

@docs Msg, Page


# Creation

@docs blank


# Routing

@docs changeRouteTo


# View

@docs view, mapDocument


# Update

@docs update

-}

import Article.Slug exposing (Slug)
import Author exposing (Author(..))
import Author.Username exposing (Username)
import Browser exposing (Document)
import Effect exposing (Effect)
import Env exposing (Env)
import Html exposing (Html, text)
import Page.Article as Article
import Page.Article.Editor as Editor
import Page.Blank as Blank
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Profile as Profile
import Page.Register as Register
import Page.Settings as Settings
import Route exposing (Route)
import Session exposing (Session)
import View
import View.Icon


{-| An opaque type to store the subpage and its model.
-}
type Page
    = Blank
    | NotFound
    | Home Home.Model
    | Settings Settings.Model
    | Login Login.Model
    | Register Register.Model
    | Profile Username Profile.Model
    | Article Article.Model
    | Editor (Maybe Slug) Editor.Model


{-| An empty page.
-}
blank : Page
blank =
    Blank


{-| Transform the messages produced by the page.
-}
mapDocument : (msg1 -> msg2) -> Document msg1 -> Document msg2
mapDocument changeMsg { title, body } =
    { title = title, body = List.map (Html.map changeMsg) body }



-- VIEW


{-| Turns the page into an HTML page.
-}
view : Env -> Session -> Page -> Document Msg
view env session page =
    let
        tz =
            Env.timeZone env

        viewPage toPageMsg config =
            viewDocument session page config
                |> mapDocument toPageMsg
    in
    case page of
        Blank ->
            viewDocument session page Blank.view

        NotFound ->
            viewDocument session page NotFound.view

        Settings settings ->
            viewPage GotSettingsMsg (Settings.view session settings)

        Home home ->
            viewPage GotHomeMsg (Home.view tz session home)

        Login login ->
            viewPage GotLoginMsg (Login.view login)

        Register register ->
            viewPage GotRegisterMsg (Register.view register)

        Profile _ profile ->
            viewPage GotProfileMsg (Profile.view tz session profile)

        Article article ->
            viewPage GotArticleMsg (Article.view tz session article)

        Editor Nothing editor ->
            viewPage GotEditorMsg (Editor.view session editor)

        Editor (Just _) editor ->
            viewPage GotEditorMsg (Editor.view session editor)


viewDocument : Session -> Page -> { title : String, content : Html msg } -> Document msg
viewDocument session page { title, content } =
    { title = title ++ " - Conduit"
    , body = [ viewHeader session page, content, View.footer ]
    }


viewHeader : Session -> Page -> Html msg
viewHeader session page =
    View.header (navbarLink page Route.Home [ Html.text "Home" ] :: viewMenu session page)


viewMenu : Session -> Page -> List (Html msg)
viewMenu session page =
    let
        linkTo =
            navbarLink page
    in
    case Session.username session of
        Just username ->
            [ linkTo Route.NewArticle [ View.Icon.newPost, text "\u{00A0}New Post" ]
            , linkTo Route.Settings [ View.Icon.settings, text "\u{00A0}Settings" ]
            , linkTo (Route.Profile username) [ View.avatar session, View.username username ]
            , linkTo Route.Logout [ text "Sign out" ]
            ]

        Nothing ->
            [ linkTo Route.Login [ text "Sign in" ]
            , linkTo Route.Register [ text "Sign up" ]
            ]


navbarLink : Page -> Route -> List (Html msg) -> Html msg
navbarLink page route linkContent =
    View.navLink (isActive page route) (Route.href route) linkContent


isActive : Page -> Route -> Bool
isActive page route =
    case ( page, route ) of
        ( Login _, Route.Login ) ->
            True

        ( Register _, Route.Register ) ->
            True

        ( Home _, Route.Home ) ->
            True

        ( Profile pageUsername _, Route.Profile routeUsername ) ->
            pageUsername == routeUsername

        ( Editor Nothing _, Route.NewArticle ) ->
            True

        ( Settings _, Route.Settings ) ->
            True

        _ ->
            False



-- UPDATE


{-| The subpages messages.
-}
type Msg
    = GotHomeMsg Home.Msg
    | GotSettingsMsg Settings.Msg
    | GotLoginMsg Login.Msg
    | GotRegisterMsg Register.Msg
    | GotProfileMsg Profile.Msg
    | GotArticleMsg Article.Msg
    | GotEditorMsg Editor.Msg


{-| Return the page and associated effects associated to a route change.
-}
changeRouteTo : Maybe Route -> Session -> Page -> ( Page, Effect Msg )
changeRouteTo maybeRoute session page =
    case maybeRoute of
        Nothing ->
            ( NotFound, Effect.none )

        Just Route.Logout ->
            ( page, Effect.logout )

        Just Route.NewArticle ->
            Editor.initNew
                |> updateWith (Editor Nothing) GotEditorMsg

        Just (Route.EditArticle slug) ->
            Editor.initEdit slug
                |> updateWith (Editor (Just slug)) GotEditorMsg

        Just Route.Settings ->
            Settings.init
                |> updateWith Settings GotSettingsMsg

        Just Route.Home ->
            Home.init session
                |> updateWith Home GotHomeMsg

        Just Route.Login ->
            Login.init
                |> updateWith Login GotLoginMsg

        Just Route.Register ->
            Register.init
                |> updateWith Register GotRegisterMsg

        Just (Route.Profile username) ->
            Profile.init username
                |> updateWith (Profile username) GotProfileMsg

        Just (Route.Article slug) ->
            Article.init slug
                |> updateWith Article GotArticleMsg


{-| Update the page from a message, returning an updated page and effects.
-}
update : Msg -> Page -> ( Page, Effect Msg )
update msg page =
    case ( msg, page ) of
        ( GotSettingsMsg subMsg, Settings settings ) ->
            Settings.update subMsg settings
                |> updateWith Settings GotSettingsMsg

        ( GotLoginMsg subMsg, Login login ) ->
            Login.update subMsg login
                |> updateWith Login GotLoginMsg

        ( GotRegisterMsg subMsg, Register register ) ->
            Register.update subMsg register
                |> updateWith Register GotRegisterMsg

        ( GotHomeMsg subMsg, Home home ) ->
            Home.update subMsg home
                |> updateWith Home GotHomeMsg

        ( GotProfileMsg subMsg, Profile username profile ) ->
            Profile.update subMsg profile
                |> updateWith (Profile username) GotProfileMsg

        ( GotArticleMsg subMsg, Article article ) ->
            Article.update subMsg article
                |> updateWith Article GotArticleMsg

        ( GotEditorMsg subMsg, Editor slug editor ) ->
            Editor.update subMsg editor
                |> updateWith (Editor slug) GotEditorMsg

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( page, Effect.none )


updateWith :
    (pageModel -> Page)
    -> (pageMsg -> Msg)
    -> ( pageModel, Effect pageMsg )
    -> ( Page, Effect Msg )
updateWith toPage toMsg ( pageModel, effect ) =
    ( toPage pageModel
    , Effect.map toMsg effect
    )
