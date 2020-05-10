module Page.Home exposing (Model, Msg, init, view, update)

{-| The homepage. You can get here via either the / or /#/ routes.

@docs Model, Msg, init, view, update

-}

import Api exposing (Cred)
import Article.Feed as Feed
import Article.Tag as Tag exposing (Tag)
import Effect exposing (Effect)
import Errors exposing (Errors)
import Html exposing (Html)
import Remote exposing (Remote)
import Session exposing (Session)
import Time
import View



-- MODEL


{-| -}
type alias Model =
    { feedTab : FeedTab
    , feedPage : Int
    , tags : Remote (List Tag)
    , feed : Remote Feed.Model
    }


type FeedTab
    = YourFeed Cred
    | GlobalFeed
    | TagFeed Tag


{-| -}
init : Session -> ( Model, Effect Msg )
init session =
    let
        feedTab =
            case Session.credentials session of
                Just cred ->
                    YourFeed cred

                Nothing ->
                    GlobalFeed
    in
    ( { feedTab = feedTab
      , feedPage = 1
      , tags = Remote.loading "tags"
      , feed = Remote.loading "feed"
      }
    , Effect.batch
        [ fetchFeed feedTab 1
        , Effect.fetchTags CompletedTagsLoad
        ]
    )



-- VIEW


{-| -}
view : Time.Zone -> Session -> Model -> { title : String, content : Html Msg }
view timeZone session model =
    { title = "Home"
    , content = View.home (viewFeed timeZone session model) (viewTags model.tags)
    }


viewFeed : Time.Zone -> Session -> Model -> List (Html Msg)
viewFeed timeZone session model =
    Remote.viewList
        (\feed ->
            List.concat
                [ [ viewTabs (Session.credentials session) model.feedTab ]
                , Feed.viewArticles timeZone session feed
                    |> List.map (Html.map GotFeedMsg)
                , [ Feed.viewPagination ClickedFeedPage model.feedPage feed ]
                ]
        )
        model.feed


viewTags : Remote (List Tag) -> Html Msg
viewTags tags =
    Remote.view (View.tags << List.map (View.tag ClickedTag)) tags



-- TABS


viewTabs : Maybe Cred -> FeedTab -> Html Msg
viewTabs maybeCred tab =
    case ( tab, maybeCred ) of
        ( YourFeed cred, _ ) ->
            View.tabs [] (yourFeed cred) [ globalFeed ]

        ( GlobalFeed, Just cred ) ->
            View.tabs [ yourFeed cred ] globalFeed []

        ( GlobalFeed, Nothing ) ->
            View.tabs [] globalFeed []

        ( TagFeed tag, Just cred ) ->
            View.tabs [ yourFeed cred, globalFeed ] (tagFeed tag) []

        ( TagFeed tag, Nothing ) ->
            View.tabs [ globalFeed ] (tagFeed tag) []


yourFeed : Cred -> ( String, Msg )
yourFeed cred =
    ( "Your Feed", ClickedTab (YourFeed cred) )


globalFeed : ( String, Msg )
globalFeed =
    ( "Global Feed", ClickedTab GlobalFeed )


tagFeed : Tag -> ( String, Msg )
tagFeed tag =
    ( "#" ++ Tag.toString tag, ClickedTab (TagFeed tag) )



-- UPDATE


{-| -}
type Msg
    = ClickedTag Tag
    | ClickedTab FeedTab
    | ClickedFeedPage Int
    | CompletedFeedLoad (Result Errors Feed.Model)
    | CompletedTagsLoad (Result Errors (List Tag))
    | GotFeedMsg Feed.Msg


{-| -}
update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ClickedTag tag ->
            let
                feedTab =
                    TagFeed tag
            in
            ( { model | feedTab = feedTab }
            , fetchFeed feedTab 1
            )

        ClickedTab tab ->
            ( { model | feedTab = tab }
            , fetchFeed tab 1
            )

        ClickedFeedPage page ->
            ( { model | feedPage = page }
            , fetchFeed model.feedTab page
            )

        CompletedFeedLoad (Ok feed) ->
            ( { model | feed = Remote.loaded feed model.feed }
            , Effect.scrollToTop
            )

        CompletedFeedLoad (Err _) ->
            ( { model | feed = Remote.failed model.feed }
            , Effect.logError
            )

        CompletedTagsLoad (Ok tags) ->
            ( { model | tags = Remote.loaded tags model.tags }
            , Effect.none
            )

        CompletedTagsLoad (Err _) ->
            ( { model | tags = Remote.failed model.tags }
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



-- FETCH FEED EFFECT


fetchFeed : FeedTab -> Int -> Effect Msg
fetchFeed feedTabs page =
    let
        toFeedMsg =
            Result.map Feed.init >> CompletedFeedLoad
    in
    case feedTabs of
        YourFeed _ -> Effect.fetchYourFeed toFeedMsg page
        GlobalFeed -> Effect.fetchGlobalFeed toFeedMsg page
        TagFeed tag -> Effect.fetchTagFeed toFeedMsg page tag
