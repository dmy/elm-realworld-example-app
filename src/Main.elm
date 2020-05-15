module Main exposing
    ( Model, Msg(..)
    , init, view, update
    , main
    )

{-|


# The Elm architecture

[The usual Elm Architecture](https://guide.elm-lang.org/architecture/) is used, except that `Effect msg` are returned instead of `Cmd msg`, thanks to [`Effect.application`](Effect#application).

@docs Model, Msg

@docs init, view, update


# Main

@docs main

-}

import Browser exposing (Document)
import Browser.Navigation as Nav
import Effect exposing (Effect)
import Env exposing (Env)
import Json.Decode exposing (Value)
import Page exposing (Page)
import Route exposing (Route)
import Session exposing (Session)
import Time
import Url exposing (Url)


{-| The application model, storing the current session (guest or authenticated),
an environment and the current page.
-}
type alias Model =
    { env : Env
    , session : Session
    , page : Page
    }



-- MODEL


{-| Initialize the application.
-}
init : Session -> Url -> Nav.Key -> ( Model, Effect Msg )
init session url navKey =
    let
        ( model, effect ) =
            changeRouteTo (Route.fromUrl url)
                { env = Env.init navKey Time.utc
                , session = session
                , page = Page.blank
                }
    in
    ( model
    , Effect.batch [ effect, Effect.getTimeZone GotTimeZone ]
    )



-- VIEW


{-| Turns the model into an HTML page.
-}
view : Model -> Document Msg
view ({ env, session } as model) =
    Page.view env session model.page
        |> Page.mapDocument GotPageMsg



-- UPDATE


{-| The top level application `Msg` type.
-}
type Msg
    = Ignored String
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotTimeZone Time.Zone
    | GotSession Session
    | GotPageMsg Page.Msg


changeRouteTo : Maybe Route -> Model -> ( Model, Effect Msg )
changeRouteTo route model =
    Page.changeRouteTo route model.session model.page
        |> fromPage model


{-| Turns messages into effects and model update.
-}
update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        -- This allows to see tagged ignored events in the debugger
        Ignored _ ->
            ( model, Effect.none )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            -- If we got a link that didn't include a fragment,
                            -- it's from one of those (href "") attributes that
                            -- we have to include to make the RealWorld CSS work.
                            --
                            -- In an application doing path routing instead of
                            -- fragment-based routing, this entire
                            -- `case url.fragment of` expression this comment
                            -- is inside would be unnecessary.
                            ( model, Effect.none )

                        Just _ ->
                            ( model, Effect.pushUrl url )

                Browser.External href ->
                    ( model
                    , Effect.loadUrl href
                    )

        ChangedUrl url ->
            changeRouteTo (Route.fromUrl url) model

        GotTimeZone timeZone ->
            ( { model | env = Env.updateTimeZone timeZone model.env }
            , Effect.none
            )

        GotSession session ->
            ( { model | session = session }, Effect.replaceUrl Route.Home )

        GotPageMsg pageMsg ->
            Page.update pageMsg model.page
                |> fromPage model


fromPage : Model -> ( Page, Effect Page.Msg ) -> ( Model, Effect Msg )
fromPage model ( page, effect ) =
    ( { model | page = page }
    , Effect.map GotPageMsg effect
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Session.onChange GotSession
        , Sub.map GotPageMsg (Page.subscriptions model.page)
        ]



-- MAIN


{-| The application entry point. It will receive the session from local storage
as a flag if present.
-}
main : Program Value Model Msg
main =
    Effect.application
        { init = init
        , view = view
        , update = update
        , ignore = Ignored
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        }
