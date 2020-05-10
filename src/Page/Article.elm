module Page.Article exposing (Model, Msg, init, view, update)

{-| Viewing an individual article.

@docs Model, Msg, init, view, update

-}

import Api exposing (Cred)
import Article exposing (Article, Full)
import Article.Body exposing (Body)
import Article.Comment as Comment exposing (Comment)
import Article.Comment.Id exposing (CommentId)
import Article.Slug exposing (Slug)
import Author exposing (Author(..), FollowedAuthor, UnfollowedAuthor)
import Effect exposing (Effect)
import Errors exposing (Errors)
import Html exposing (Html)
import Remote exposing (Remote)
import Route
import Session exposing (Session)
import Time
import View
import View.Button



-- MODEL


{-| -}
type alias Model =
    { errors : Errors
    , comments : Remote ( CommentText, List Comment )
    , article : Remote (Article Full)
    }


type CommentText
    = Editing String
    | Sending String


{-| -}
init : Slug -> ( Model, Effect Msg )
init slug =
    ( { errors = Errors.none
      , comments = Remote.loading "comments"
      , article = Remote.loading "article"
      }
    , Effect.batch
        [ Effect.fetchArticle CompletedLoadArticle slug
        , Effect.fetchComments CompletedLoadComments slug
        ]
    )



-- VIEW


{-| -}
view : Time.Zone -> Session -> Model -> { title : String, content : Html Msg }
view timeZone session model =
    { title =
        case Remote.get model.article of
            Just article -> (Article.metadata article).title
            Nothing -> "Article"
    , content = Remote.view (viewArticle timeZone session model) model.article
    }


viewArticle : Time.Zone -> Session -> Model -> Article Full -> Html Msg
viewArticle timeZone session model article =
    let
        slug =
            Article.slug article
    in
    View.article timeZone model.errors ClickedDismissErrors article (viewButtons session article) <|
        Remote.viewList (viewComments timeZone session slug) model.comments


viewComments : Time.Zone -> Session -> Slug -> ( CommentText, List Comment ) -> List (Html Msg)
viewComments timeZone session slug ( commentText, comments ) =
    -- Don't let users add comments until they can see the
    -- existing comments! Otherwise you may be about to repeat
    -- something that's already been said.
    viewAddComment session slug commentText
        :: List.map (viewComment timeZone session slug) comments


viewAddComment : Session -> Slug -> CommentText -> Html Msg
viewAddComment session slug commentText =
    let
        ( commentStr, disabled ) =
            case commentText of
                Editing str -> ( str, False )
                Sending str -> ( str, True )
    in
    View.newComment session
        { onInput = EnteredCommentText
        , onSubmit = ClickedPostComment slug
        , disabled = disabled
        , comment = commentStr
        }


viewButtons : Session -> Article Full -> List (Html Msg)
viewButtons session article =
    case Session.credentials session of
        Just cred ->
            case Article.author article of
                Followed author ->
                    [ View.Button.unfollow ClickedUnfollow cred author
                    , Html.text " "
                    , favoriteButton cred article
                    ]

                Unfollowed author ->
                    [ View.Button.follow ClickedFollow cred author
                    , Html.text " "
                    , favoriteButton cred article
                    ]

                Authenticated _ ->
                    [ View.Button.edit " Edit Article" (Route.EditArticle (Article.slug article))
                    , Html.text " "
                    , View.Button.delete " Delete Article" (ClickedDeleteArticle cred (Article.slug article))
                    ]

        Nothing ->
            []


viewComment : Time.Zone -> Session -> Slug -> Comment -> Html Msg
viewComment timeZone session slug comment =
    View.comment timeZone session (ClickedDeleteComment slug) comment



-- UPDATE


{-| -}
type Msg
    = ClickedDeleteArticle Cred Slug
    | ClickedDeleteComment Slug Cred CommentId
    | ClickedDismissErrors
    | ClickedFavorite Cred Slug Body
    | ClickedUnfavorite Cred Slug Body
    | ClickedFollow Cred UnfollowedAuthor
    | ClickedUnfollow Cred FollowedAuthor
    | ClickedPostComment Slug Cred
    | EnteredCommentText String
    | CompletedLoadArticle (Result Errors (Article Full))
    | CompletedLoadComments (Result Errors (List Comment))
    | CompletedDeleteArticle (Result Errors ())
    | CompletedDeleteComment CommentId (Result Errors ())
    | CompletedFavoriteChange (Result Errors (Article Full))
    | CompletedFollowChange (Result Errors Author)
    | CompletedPostComment (Result Errors Comment)


{-| -}
update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ClickedDismissErrors ->
            ( { model | errors = Errors.none }, Effect.none )

        ClickedFavorite cred slug body ->
            ( model
            , Effect.favorite (Result.map (Article.fromPreview body) >> CompletedFavoriteChange) cred slug
            )

        ClickedUnfavorite cred slug body ->
            ( model
            , Effect.unfavorite (Result.map (Article.fromPreview body) >> CompletedFavoriteChange) cred slug
            )

        CompletedLoadArticle (Ok article) ->
            ( { model | article = Remote.loaded article model.article }
            , Effect.none
            )

        CompletedLoadArticle (Err _) ->
            ( { model | article = Remote.failed model.article }
            , Effect.logError
            )

        CompletedLoadComments (Ok comments) ->
            ( { model | comments = Remote.loaded ( Editing "", comments ) model.comments }
            , Effect.none
            )

        CompletedLoadComments (Err _) ->
            ( { model | comments = Remote.failed model.comments }
            , Effect.logError
            )

        CompletedFavoriteChange (Ok article) ->
            ( { model | article = Remote.loaded article model.article }
            , Effect.none
            )

        CompletedFavoriteChange (Err errors) ->
            ( { model | errors = Errors.prepend errors model.errors }
            , Effect.logError
            )

        ClickedUnfollow cred followedAuthor ->
            ( model
            , Effect.unfollow CompletedFollowChange cred followedAuthor
            )

        ClickedFollow cred unfollowedAuthor ->
            ( model
            , Effect.follow CompletedFollowChange cred unfollowedAuthor
            )

        CompletedFollowChange (Ok newAuthor) ->
            case Remote.get model.article of
                Just article ->
                    ( { model | article = Remote.loaded (Article.mapAuthor (\_ -> newAuthor) article) model.article }
                    , Effect.none
                    )

                Nothing ->
                    ( model, Effect.logError )

        CompletedFollowChange (Err errors) ->
            ( { model | errors = Errors.prepend errors model.errors }
            , Effect.logError
            )

        EnteredCommentText str ->
            case Remote.get model.comments of
                Just ( Editing _, comments ) ->
                    -- You can only edit comment text once comments have loaded
                    -- successfully, and when the comment is not currently
                    -- being submitted.
                    ( { model | comments = Remote.loaded ( Editing str, comments ) model.comments }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.logError )

        ClickedPostComment slug cred ->
            case Remote.get model.comments of
                Just ( Editing "", _ ) ->
                    -- No posting empty comments!
                    -- We don't use Effect.logError here because this isn't an error,
                    -- it just doesn't do anything.
                    ( model, Effect.none )

                Just ( Editing str, comments ) ->
                    ( { model | comments = Remote.loaded ( Sending str, comments ) model.comments }
                    , Effect.postComment CompletedPostComment cred slug str
                    )

                _ ->
                    -- Either we have no comment to post, or there's already
                    -- one in the process of being posted, or we don't have
                    -- a valid article, in which case how did we post this?
                    ( model, Effect.logError )

        CompletedPostComment (Ok comment) ->
            case Remote.get model.comments of
                Just ( _, comments ) ->
                    ( { model | comments = Remote.loaded ( Editing "", comment :: comments ) model.comments }
                    , Effect.none
                    )

                Nothing ->
                    ( model, Effect.logError )

        CompletedPostComment (Err errors) ->
            ( { model | errors = Errors.prepend errors model.errors }
            , Effect.logError
            )

        ClickedDeleteComment slug cred id ->
            ( model
            , Effect.deleteComment (CompletedDeleteComment id) cred slug id
            )

        CompletedDeleteComment id (Ok ()) ->
            case Remote.get model.comments of
                Just ( commentText, comments ) ->
                    ( { model | comments = Remote.loaded ( commentText, withoutComment id comments ) model.comments }
                    , Effect.none
                    )

                Nothing ->
                    ( model, Effect.logError )

        CompletedDeleteComment _ (Err errors) ->
            ( { model | errors = Errors.prepend errors model.errors }
            , Effect.logError
            )

        ClickedDeleteArticle cred slug ->
            ( model
            , Effect.deleteArticle CompletedDeleteArticle cred slug
            )

        CompletedDeleteArticle (Ok ()) ->
            ( model, Effect.replaceUrl Route.Home )

        CompletedDeleteArticle (Err errors) ->
            ( { model | errors = Errors.prepend errors model.errors }
            , Effect.logError
            )



-- INTERNAL


withoutComment : CommentId -> List Comment -> List Comment
withoutComment id list =
    List.filter (\comment -> Comment.id comment /= id) list


favoriteButton : Cred -> Article Full -> Html Msg
favoriteButton cred article =
    let
        { favoritesCount, favorited } =
            Article.metadata article

        slug =
            Article.slug article

        body =
            Article.body article

        kids =
            [ Html.text (" Favorite Article (" ++ String.fromInt favoritesCount ++ ")") ]
    in
    if favorited then
        View.Button.unfavorite (ClickedUnfavorite cred slug body) [] kids

    else
        View.Button.favorite (ClickedFavorite cred slug body) [] kids
