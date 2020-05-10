module Page.Profile exposing (Model, Msg, init, view, update)

{-| An Author's profile.

@docs Model, Msg, init, view, update

-}

import Api exposing (Cred)
import Article.Feed as Feed
import Author exposing (Author(..), FollowedAuthor, UnfollowedAuthor)
import Author.Username as Username exposing (Username)
import Effect exposing (Effect)
import Errors exposing (Errors)
import Html exposing (Html)
import Remote exposing (Remote)
import Session exposing (Session)
import Time
import View
import View.Button



-- MODEL


{-| -}
type alias Model =
    { errors : Errors
    , feedTab : FeedTab
    , feedPage : Int
    , username : Username
    , author : Remote Author
    , feed : Remote Feed.Model
    }


type FeedTab
    = MyArticles
    | FavoritedArticles


{-| -}
init : Username -> ( Model, Effect Msg )
init username =
    ( { errors = Errors.none
      , feedTab = defaultFeedTab
      , feedPage = 1
      , username = username
      , author = Remote.loading (Username.toString username ++ " profile")
      , feed = Remote.loading "feed"
      }
    , Effect.batch
        [ Effect.fetchAuthor CompletedAuthorLoad username
        , fetchFeed defaultFeedTab username 1
        ]
    )


defaultFeedTab : FeedTab
defaultFeedTab =
    MyArticles



-- HTTP


fetchFeed : FeedTab -> Username -> Int -> Effect Msg
fetchFeed feedTabs username page =
    case feedTabs of
        MyArticles ->
            Effect.fetchAuthorFeed
                (Result.map Feed.init >> Result.mapError (Tuple.pair username) >> CompletedFeedLoad)
                page
                username

        FavoritedArticles ->
            Effect.fetchFavoritesFeed
                (Result.map Feed.init >> Result.mapError (Tuple.pair username) >> CompletedFeedLoad)
                page
                username



-- VIEW


{-| -}
view : Time.Zone -> Session -> Model -> { title : String, content : Html Msg }
view timeZone session model =
    let
        title =
            case Remote.get model.author of
                Just (Authenticated _) ->
                    myProfileTitle

                Just (Followed _) ->
                    titleForOther model.username

                Just (Unfollowed _) ->
                    titleForOther model.username

                Nothing ->
                    titleForMe (Session.credentials session) model.username
    in
    { title = title
    , content = Remote.view (viewProfile timeZone session model) model.author
    }


viewProfile : Time.Zone -> Session -> Model -> Author -> Html Msg
viewProfile timeZone session model author =
    View.profile
        [ View.dismissableErrors ClickedDismissErrors model.errors
        , View.profileAuthor (followButton session) author
        , Remote.view (viewFeed timeZone session model) model.feed
        ]


followButton : Session -> Author -> Html Msg
followButton session author =
    case Session.credentials session of
        Just cred ->
            case author of
                Authenticated _ ->
                    -- We can't follow ourselves!
                    View.nothing

                Followed followedAuthor ->
                    View.Button.unfollow ClickedUnfollow cred followedAuthor

                Unfollowed unfollowedAuthor ->
                    View.Button.follow ClickedFollow cred unfollowedAuthor

        Nothing ->
            -- We can't follow if we're logged out
            View.nothing


viewFeed : Time.Zone -> Session -> Model -> Feed.Model -> Html Msg
viewFeed timeZone session model feed =
    View.profileFeed <|
        List.concat
            [ [ viewTabs model.feedTab ]
            , Feed.viewArticles timeZone session feed
                |> List.map (Html.map GotFeedMsg)
            , [ Feed.viewPagination ClickedFeedPage model.feedPage feed ]
            ]



-- PAGE TITLE


titleForOther : Username -> String
titleForOther otherUsername =
    "Profile — " ++ Username.toString otherUsername


titleForMe : Maybe Cred -> Username -> String
titleForMe maybeCred username =
    case maybeCred of
        Just cred ->
            if username == Api.username cred then
                myProfileTitle

            else
                defaultTitle

        Nothing ->
            defaultTitle


myProfileTitle : String
myProfileTitle =
    "My Profile"


defaultTitle : String
defaultTitle =
    "Profile"



-- TABS


viewTabs : FeedTab -> Html Msg
viewTabs tab =
    case tab of
        MyArticles ->
            View.tabs [] myArticles [ favoritedArticles ]

        FavoritedArticles ->
            View.tabs [ myArticles ] favoritedArticles []


myArticles : ( String, Msg )
myArticles =
    ( "My Articles", ClickedTab MyArticles )


favoritedArticles : ( String, Msg )
favoritedArticles =
    ( "Favorited Articles", ClickedTab FavoritedArticles )



-- UPDATE


{-| -}
type Msg
    = ClickedDismissErrors
    | ClickedFollow Cred UnfollowedAuthor
    | ClickedUnfollow Cred FollowedAuthor
    | ClickedTab FeedTab
    | ClickedFeedPage Int
    | CompletedFollowChange (Result Errors Author)
    | CompletedAuthorLoad (Result Errors Author)
    | CompletedFeedLoad (Result ( Username, Errors ) Feed.Model)
    | GotFeedMsg Feed.Msg


{-| -}
update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ClickedDismissErrors ->
            ( { model | errors = Errors.none }, Effect.none )

        ClickedUnfollow cred followedAuthor ->
            ( model
            , Effect.unfollow CompletedFollowChange cred followedAuthor
            )

        ClickedFollow cred unfollowedAuthor ->
            ( model
            , Effect.follow CompletedFollowChange cred unfollowedAuthor
            )

        ClickedTab tab ->
            ( { model | feedTab = tab }
            , fetchFeed tab model.username 1
            )

        ClickedFeedPage page ->
            ( { model | feedPage = page }
            , fetchFeed model.feedTab model.username page
            )

        CompletedFollowChange (Ok author) ->
            ( { model | author = Remote.loaded author model.author }
            , Effect.none
            )

        CompletedFollowChange (Err errors) ->
            ( { model | errors = errors }
            , Effect.logError
            )

        CompletedAuthorLoad (Ok author) ->
            ( { model | author = Remote.loaded author model.author }
            , Effect.none
            )

        CompletedAuthorLoad (Err _) ->
            ( { model | author = Remote.failed model.author }
            , Effect.logError
            )

        CompletedFeedLoad (Ok feed) ->
            ( { model | feed = Remote.loaded feed model.feed }
            , Effect.none
            )

        CompletedFeedLoad (Err _) ->
            ( { model | feed = Remote.failed model.feed }
            , Effect.logError
            )

        GotFeedMsg subMsg ->
            case Remote.get model.feed of
                Just feed ->
                    let
                        ( newFeed, effect ) =
                            Feed.update subMsg feed
                    in
                    ( { model | feed = Remote.loaded newFeed model.feed }
                    , Effect.map GotFeedMsg effect
                    )

                Nothing ->
                    ( model, Effect.logError )
