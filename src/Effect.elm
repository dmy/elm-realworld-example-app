module Effect exposing
    ( Effect
    , Model, application
    , none, batch, map
    , replaceUrl, pushUrl, loadUrl
    , getTimeZone
    , scrollToTop
    , register, authenticate, login, logout, fetchSettings, updateSettings
    , fetchTags
    , fetchArticle, createArticle, editArticle, deleteArticle, favorite, unfavorite
    , fetchYourFeed, fetchGlobalFeed, fetchTagFeed, fetchAuthorFeed, fetchFavoritesFeed
    , fetchComments, postComment, deleteComment
    , fetchAuthor, follow, unfollow
    , logError
    )

{-| The application effects.

@docs Effect


# Application

@docs Model, application


# Fancy effects

@docs none, batch, map


# Navigation

@docs replaceUrl, pushUrl, loadUrl


# Time

@docs getTimeZone


# Scrolling

@docs scrollToTop


# User

@docs register, authenticate, login, logout, fetchSettings, updateSettings


# Tags

@docs fetchTags


# Article

@docs fetchArticle, createArticle, editArticle, deleteArticle, favorite, unfavorite


# Articles feed

@docs fetchYourFeed, fetchGlobalFeed, fetchTagFeed, fetchAuthorFeed, fetchFavoritesFeed


# Comments

@docs fetchComments, postComment, deleteComment


# Authors

@docs fetchAuthor, follow, unfollow


# Errors

@docs logError

-}

import Api exposing (Cred)
import Api.Endpoint as Endpoint exposing (Endpoint)
import Article exposing (Article, Full, Preview)
import Article.Comment exposing (Comment)
import Article.Comment.Id exposing (CommentId)
import Article.Slug exposing (Slug)
import Article.Tag as Tag exposing (Tag)
import Author exposing (Author, FollowedAuthor, Settings, UnfollowedAuthor)
import Author.Username as Username exposing (Username)
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Env exposing (Env)
import Errors exposing (Errors)
import Json.Decode as Decode exposing (Decoder)
import PaginatedList exposing (PaginatedList)
import Route exposing (Route)
import Session exposing (Session)
import Task
import Time
import Url exposing (Url)
import Url.Builder exposing (QueryParameter)



-- EFFECT TYPE


type alias Feed =
    PaginatedList (Article Preview)


{-| An opaque type that represents all the effects that `init` and `update` functions may produce.
-}
type Effect msg
    = None
    | Batch (List (Effect msg))
      -- Navigation
    | ReplaceUrl Route
    | PushUrl Url
    | LoadUrl String
      -- Misc
    | GetTimeZone (Time.Zone -> msg)
    | ScrollToTop
      -- Session
    | UpsertUser (Result Errors Session -> msg) (Api.Request Session)
    | ReplaceSession Session
    | FetchSettings (Result Errors Settings -> msg) (Cred -> Api.Request Settings)
      -- Tags
    | FetchTags (Result Errors (List Tag) -> msg) (Api.Request (List Tag))
      -- Articles
    | FetchArticle (Result Errors (Article Full) -> msg) (Session -> Api.Request (Article Full))
    | UpsertArticle (Result Errors (Article Full) -> msg) (Api.Request (Article Full))
    | FavorArticle (Result Errors (Article Preview) -> msg) (Api.Request (Article Preview))
      -- Feed
    | FetchFeed (Result Errors Feed -> msg) (Session -> Api.Request Feed)
      -- Comments
    | FetchComments (Result Errors (List Comment) -> msg) (Session -> Api.Request (List Comment))
    | PostComment (Result Errors Comment -> msg) (Api.Request Comment)
      -- Authors
    | FetchAuthor (Result Errors Author -> msg) (Session -> Api.Request Author)
    | ChangeFollow (Result Errors Author -> msg) (Api.Request Author)
      -- Common
    | Delete (Result Errors () -> msg) (Api.Request ())



-- APPLICATION


{-| A custom application that will turn `init` and `update` effects into
actual `Cmd` and state changes.

The additional `ignore` function is used to ignore events at a single place
in the whole application. Compared to the commonly used `NoOp`, it takes an
additionnal `String` description to be more informative in the debugger.

-}
application :
    { init : Session -> Url -> Nav.Key -> ( Model r, Effect msg )
    , view : Model r -> Browser.Document msg
    , update : msg -> Model r -> ( Model r, Effect msg )
    , ignore : String -> msg
    , subscriptions : Model r -> Sub msg
    , onUrlChange : Url -> msg
    , onUrlRequest : Browser.UrlRequest -> msg
    }
    -> Program Decode.Value (Model r) msg
application config =
    let
        init maybeSession =
            config.init (Maybe.withDefault Session.guest maybeSession)
    in
    Api.application Session.decoder
        { init = \flags url key -> init flags url key |> perform config.ignore
        , view = config.view
        , update = \msg model -> config.update msg model |> perform config.ignore
        , subscriptions = config.subscriptions
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        }



-- PERFORM


{-| The partial top level model required to perform effects.
-}
type alias Model r =
    { r
        | session : Session
        , env : Env
    }


perform : (String -> msg) -> ( Model r, Effect msg ) -> ( Model r, Cmd msg )
perform ignore ( model, effect ) =
    case effect of
        None ->
            ( model, Cmd.none )

        Batch effects ->
            List.foldl (batchEffect ignore) ( model, [] ) effects
                |> Tuple.mapSecond Cmd.batch

        -- NAVIGATION
        ReplaceUrl route ->
            ( model, Route.replaceUrl (Env.navKey model.env) route )

        PushUrl url ->
            ( model, Nav.pushUrl (Env.navKey model.env) (Url.toString url) )

        LoadUrl href ->
            ( model, Nav.load href )

        -- MISC
        GetTimeZone toMsg ->
            ( model, Task.perform toMsg Time.here )

        ScrollToTop ->
            -- It's not worth showing the user anything special if scrolling fails.
            -- If anything, we'd log this to an error recording service.
            ( model, Task.perform (\_ -> ignore "scrollToTop") <| Dom.setViewport 0 0 )

        -- SESSION
        ReplaceSession session ->
            ( { model | session = session }
            , Cmd.batch
                [ Route.replaceUrl (Env.navKey model.env) Route.Home
                , Session.store session
                ]
            )

        FetchSettings toMsg toRequest ->
            sendAuthenticated model toMsg toRequest

        UpsertUser toMsg request ->
            send model toMsg request

        -- TAGS
        FetchTags toMsg request ->
            send model toMsg request

        -- ARTICLES
        FetchArticle toMsg toRequest ->
            send model toMsg (toRequest model.session)

        UpsertArticle toMsg request ->
            send model toMsg request

        FavorArticle toMsg request ->
            send model toMsg request

        -- FEED
        FetchFeed toMsg toRequest ->
            send model toMsg (toRequest model.session)

        -- COMMENTS
        FetchComments toMsg toRequest ->
            send model toMsg (toRequest model.session)

        PostComment toMsg request ->
            send model toMsg request

        -- AUTHORS
        FetchAuthor toMsg toRequest ->
            send model toMsg (toRequest model.session)

        ChangeFollow toMsg request ->
            send model toMsg request

        -- COMMON
        Delete toMsg request ->
            send model toMsg request


batchEffect : (String -> msg) -> Effect msg -> ( Model r, List (Cmd msg) ) -> ( Model r, List (Cmd msg) )
batchEffect ignore effect ( model, cmds ) =
    perform ignore ( model, effect )
        |> Tuple.mapSecond (\cmd -> cmd :: cmds)


send : Model r -> (Result Errors a -> msg) -> Api.Request a -> ( Model r, Cmd msg )
send model toMsg request =
    ( model, Endpoint.request toMsg request )


sendAuthenticated : Model r -> (Result Errors a -> msg) -> (Cred -> Api.Request a) -> ( Model r, Cmd msg )
sendAuthenticated model toMsg toRequest =
    case Session.credentials model.session of
        Just cred ->
            send model toMsg (toRequest cred)

        Nothing ->
            ( model, Route.pushUrl (Env.navKey model.env) Route.Login )



-- EFFECTS


{-| No effect.
-}
none : Effect msg
none =
    None


{-| Batch several effects together.
-}
batch : List (Effect msg) -> Effect msg
batch =
    Batch


{-| Transform the messages produced by an effect.
-}
map : (a -> msg) -> Effect a -> Effect msg
map changeMsg effect =
    case effect of
        None -> None
        Batch effects -> Batch (List.map (map changeMsg) effects)
        ReplaceUrl route -> ReplaceUrl route
        PushUrl url -> PushUrl url
        LoadUrl href -> LoadUrl href
        GetTimeZone toMsg -> GetTimeZone (toMsg >> changeMsg)
        ScrollToTop -> ScrollToTop
        ReplaceSession session -> ReplaceSession session
        UpsertUser toMsg request -> UpsertUser (toMsg >> changeMsg) request
        FetchSettings toMsg request -> FetchSettings (toMsg >> changeMsg) request
        FetchTags toMsg request -> FetchTags (toMsg >> changeMsg) request
        FetchArticle toMsg request -> FetchArticle (toMsg >> changeMsg) request
        UpsertArticle toMsg request -> UpsertArticle (toMsg >> changeMsg) request
        FavorArticle toMsg request -> FavorArticle (toMsg >> changeMsg) request
        FetchFeed toMsg request -> FetchFeed (toMsg >> changeMsg) request
        FetchComments toMsg request -> FetchComments (toMsg >> changeMsg) request
        PostComment toMsg request -> PostComment (toMsg >> changeMsg) request
        FetchAuthor toMsg request -> FetchAuthor (toMsg >> changeMsg) request
        ChangeFollow toMsg request -> ChangeFollow (toMsg >> changeMsg) request
        Delete toMsg request -> Delete (toMsg >> changeMsg) request



-- NAVIGATION


{-| Replace the current URL without adding an entry to the browser history.
-}
replaceUrl : Route -> Effect msg
replaceUrl =
    ReplaceUrl


{-| Change the URL and add a new entry to the browser history.
-}
pushUrl : Url -> Effect msg
pushUrl =
    PushUrl


{-| Leave the current page and load the given URL.
-}
loadUrl : String -> Effect msg
loadUrl href =
    LoadUrl href



-- MISC


{-| Retrieve a time zone based on the current UTC offset.
-}
getTimeZone : (Time.Zone -> msg) -> Effect msg
getTimeZone toMsg =
    GetTimeZone toMsg


{-| Scroll page to top.
-}
scrollToTop : Effect msg
scrollToTop =
    ScrollToTop



-- SESSION


{-| Create a new user.
-}
register :
    (Result Errors Session -> msg)
    -> { username : String, email : String, password : String }
    -> Effect msg
register toMsg credentials =
    UpsertUser toMsg (Author.register credentials)


{-| Authenticate with the backend.
-}
authenticate : (Result Errors Session -> msg) -> { email : String, password : String } -> Effect msg
authenticate toMsg credentials =
    UpsertUser toMsg (Author.authenticate credentials)


{-| Log in with an authenticated session.
-}
login : Session -> Effect msg
login session =
    ReplaceSession session


{-| Logout
-}
logout : Effect msg
logout =
    ReplaceSession Session.guest


{-| Fetch the logged user settings.
-}
fetchSettings : (Result Errors Settings -> msg) -> Effect msg
fetchSettings toMsg =
    FetchSettings toMsg Author.fetchUserSettings


{-| Update the logged user settings.
-}
updateSettings : (Result Errors Session -> msg) -> Cred -> Settings -> Effect msg
updateSettings toMsg cred settings =
    UpsertUser toMsg (Author.edit cred settings)



-- TAGS


{-| Fetch tags.
-}
fetchTags : (Result Errors (List Tag) -> msg) -> Effect msg
fetchTags toMsg =
    FetchTags toMsg Tag.list



-- ARTICLES


{-| Fetch an article.
-}
fetchArticle : (Result Errors (Article Full) -> msg) -> Slug -> Effect msg
fetchArticle toMsg slug =
    FetchArticle toMsg (Session.credentials >> Article.fetch slug)


{-| Create a new article.
-}
createArticle :
    (Result Errors (Article Full) -> msg)
    -> Cred
    -> { title : String, description : String, body : String, tags : String }
    -> Effect msg
createArticle toMsg cred article =
    UpsertArticle toMsg (Article.create article cred)


{-| Edit a previously created article.
-}
editArticle :
    (Result Errors (Article Full) -> msg)
    -> Cred
    -> Slug
    -> { r | title : String, description : String, body : String }
    -> Effect msg
editArticle toMsg cred slug article =
    UpsertArticle toMsg (Article.edit slug article cred)


{-| Delete a previously created article.
-}
deleteArticle : (Result Errors () -> msg) -> Cred -> Slug -> Effect msg
deleteArticle toMsg cred slug =
    Delete toMsg <|
        Api.delete (Endpoint.article slug) cred (Decode.succeed ())


{-| Set an article as favorite.
-}
favorite : (Result Errors (Article Preview) -> msg) -> Cred -> Slug -> Effect msg
favorite toMsg cred slug =
    FavorArticle toMsg (Article.favorite slug cred)


{-| Unset an article as favorite.
-}
unfavorite : (Result Errors (Article Preview) -> msg) -> Cred -> Slug -> Effect msg
unfavorite toMsg cred slug =
    FavorArticle toMsg (Article.unfavorite slug cred)



-- FEED


{-| Fetch the user articles feed.
-}
fetchYourFeed : (Result Errors (PaginatedList (Article Preview)) -> msg) -> Int -> Effect msg
fetchYourFeed toMsg page =
    FetchFeed toMsg (feedRequest <| Endpoint.feed <| feedParams page)


{-| Fetch the global articles feed.
-}
fetchGlobalFeed : (Result Errors (PaginatedList (Article Preview)) -> msg) -> Int -> Effect msg
fetchGlobalFeed toMsg page =
    FetchFeed toMsg (feedRequest <| Endpoint.articles <| feedParams page)


{-| Fetch an author feed.
-}
fetchAuthorFeed : (Result Errors (PaginatedList (Article Preview)) -> msg) -> Int -> Username -> Effect msg
fetchAuthorFeed toMsg page username =
    let
        firstParam =
            Url.Builder.string "author" (Username.toString username)
    in
    FetchFeed toMsg (feedRequest <| Endpoint.articles <| firstParam :: feedParams page)


{-| Fetch the favorite artciles feed.
-}
fetchFavoritesFeed : (Result Errors (PaginatedList (Article Preview)) -> msg) -> Int -> Username -> Effect msg
fetchFavoritesFeed toMsg page username =
    let
        firstParam =
            Url.Builder.string "favorited" (Username.toString username)
    in
    FetchFeed toMsg (feedRequest <| Endpoint.articles <| firstParam :: feedParams page)


{-| Fetch the current tag feed.
-}
fetchTagFeed : (Result Errors (PaginatedList (Article Preview)) -> msg) -> Int -> Tag -> Effect msg
fetchTagFeed toMsg page tag =
    let
        firstParam =
            Url.Builder.string "tag" (Tag.toString tag)
    in
    FetchFeed toMsg (feedRequest <| Endpoint.articles <| firstParam :: feedParams page)


feedRequest : Endpoint -> Session -> Api.Request (PaginatedList (Article Preview))
feedRequest endpoint session =
    let
        maybeCred =
            Session.credentials session
    in
    Api.get endpoint maybeCred (feedDecoder maybeCred)


feedParams : Int -> List QueryParameter
feedParams page =
    PaginatedList.params { page = page, resultsPerPage = articlesPerPage }


feedDecoder : Maybe Cred -> Decoder (PaginatedList (Article Preview))
feedDecoder maybeCred =
    PaginatedList.decoder (Article.previewDecoder maybeCred) articlesPerPage


articlesPerPage : Int
articlesPerPage =
    10



-- COMMENTS


{-| Fetch an article comments.
-}
fetchComments : (Result Errors (List Comment) -> msg) -> Slug -> Effect msg
fetchComments toMsg slug =
    FetchComments toMsg (Session.credentials >> Article.Comment.list slug)


{-| Post an article comment.
-}
postComment : (Result Errors Comment -> msg) -> Cred -> Slug -> String -> Effect msg
postComment toMsg cred slug comment =
    PostComment toMsg (Article.Comment.post slug comment cred)


{-| Delete a previously posted article comment.
-}
deleteComment : (Result Errors () -> msg) -> Cred -> Slug -> CommentId -> Effect msg
deleteComment toMsg cred slug commentId =
    Delete toMsg (Article.Comment.delete slug commentId cred)



-- AUTHORS


{-| Fetch an author profile.
-}
fetchAuthor : (Result Errors Author -> msg) -> Username -> Effect msg
fetchAuthor toMsg username =
    FetchAuthor toMsg <|
        \session -> Author.fetch username (Session.credentials session)


{-| Follow an author.
-}
follow : (Result Errors Author -> msg) -> Cred -> UnfollowedAuthor -> Effect msg
follow toMsg cred author =
    ChangeFollow toMsg <|
        Author.follow author cred


{-| Unfollow an author.
-}
unfollow : (Result Errors Author -> msg) -> Cred -> FollowedAuthor -> Effect msg
unfollow toMsg cred author =
    ChangeFollow toMsg <|
        Author.unfollow author cred



-- ERRORS


{-| This is a placeholder API for how we might log errors to a server.

Whenever you see Effect.logError used in this code base, it means
"Something unexpected happened. This is where we would log an error to a server
with some diagnostic info so we could investigate what happened later."

(Since this is outside the scope of the RealWorld spec, and is only a
placeholder anyway, this function does not accept actual diagnostic info,
authentication tokens, etc.)

-}
logError : Effect msg
logError =
    none
