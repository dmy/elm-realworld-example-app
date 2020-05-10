module Article.Feed exposing (Model, Msg, init, update, viewArticles, viewPagination)

import Api exposing (Cred)
import Article exposing (Article, Preview)
import Article.Slug exposing (Slug)
import Effect exposing (Effect)
import Errors exposing (Errors)
import Html exposing (Html)
import PaginatedList exposing (PaginatedList)
import Session exposing (Session)
import Time
import View
import View.Button



-- MODEL


type Model
    = Model Internals


{-| This should not be exposed! We want to benefit from the guarantee that only
this module can create or alter this model. This way if it ever ends up in
a surprising state, we know exactly where to look: this module.
-}
type alias Internals =
    { errors : Errors
    , articles : PaginatedList (Article Preview)
    , isLoading : Bool
    }


init : PaginatedList (Article Preview) -> Model
init articles =
    Model
        { errors = Errors.none
        , articles = articles
        , isLoading = False
        }



-- VIEW


viewArticles : Time.Zone -> Session -> Model -> List (Html Msg)
viewArticles timeZone session (Model { articles, errors }) =
    let
        articlesHtml =
            PaginatedList.values articles
                |> List.map (viewPreview timeZone (Session.credentials session))
    in
    View.dismissableErrors ClickedDismissErrors errors :: articlesHtml


viewPreview : Time.Zone -> Maybe Cred -> Article Preview -> Html Msg
viewPreview timeZone maybeCred article =
    View.articlePreview timeZone maybeCred faveButton article


faveButton : Maybe Cred -> Slug -> Article.Metadata -> Html Msg
faveButton maybeCred slug metadata =
    case maybeCred of
        Just cred ->
            let
                viewButton =
                    if metadata.favorited then
                        View.Button.unfavorite (ClickedUnfavorite cred slug)

                    else
                        View.Button.favorite (ClickedFavorite cred slug)
            in
            viewButton [ View.Button.alignRight ]
                [ Html.text (" " ++ String.fromInt metadata.favoritesCount) ]

        Nothing ->
            View.nothing


viewPagination : (Int -> msg) -> Int -> Model -> Html msg
viewPagination onPageLinkClick page (Model feed) =
    View.pagination
        { onPageLinkClick = onPageLinkClick
        , page = page
        , pages = PaginatedList.total feed.articles
        }



-- UPDATE


type Msg
    = ClickedDismissErrors
    | ClickedFavorite Cred Slug
    | ClickedUnfavorite Cred Slug
    | CompletedFavorite (Result Errors (Article Preview))


update : Msg -> Model -> ( Model, Effect Msg )
update msg (Model model) =
    case msg of
        ClickedDismissErrors ->
            ( Model { model | errors = Errors.none }
            , Effect.none
            )

        ClickedFavorite cred slug ->
            ( Model model
            , Effect.favorite CompletedFavorite cred slug
            )

        ClickedUnfavorite cred slug ->
            ( Model model
            , Effect.unfavorite CompletedFavorite cred slug
            )

        CompletedFavorite (Ok article) ->
            ( Model { model | articles = PaginatedList.map (replaceArticle article) model.articles }
            , Effect.none
            )

        CompletedFavorite (Err errors) ->
            ( Model { model | errors = Errors.prepend errors model.errors }
            , Effect.none
            )


replaceArticle : Article a -> Article a -> Article a
replaceArticle newArticle oldArticle =
    if Article.slug newArticle == Article.slug oldArticle then
        newArticle

    else
        oldArticle
