module Api.Endpoint exposing
    ( Endpoint
    , Request, request
    , login, user, users, profiles, follow
    , article, articles, feed, favorite, comment, comments, tags
    )

{-| This module exposes an opaque Endpoint type which is guaranteed to point
to the correct Conduit API URL.

@docs Endpoint


# Request

@docs Request, request


# Endpoints


## Authors

@docs login, user, users, profiles, follow


## Articles

@docs article, articles, feed, favorite, comment, comments, tags

-}

import Article.Comment.Id exposing (CommentId)
import Article.Slug as Slug exposing (Slug)
import Author.Username as Username exposing (Username)
import Errors exposing (Errors)
import Http
import Json.Decode as Decode exposing (Decoder)
import Url.Builder exposing (QueryParameter)


{-| A type to represent an HTTP request.

This is needed as we can't store a `Cmd` into an `Effect` if we want to be able
to test it.

-}
type alias Request a =
    { method : String
    , headers : List Http.Header
    , url : Endpoint
    , body : Http.Body
    , decoder : Decoder a
    }


{-| Turn the request into an actual `Cmd`.
-}
request : (Result Errors a -> msg) -> Request a -> Cmd msg
request toMsg req =
    Http.request
        { method = req.method
        , headers = req.headers
        , url = unwrap req.url
        , body = req.body
        , expect = expectJson toMsg req.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


expectJson : (Result Errors a -> msg) -> Decoder a -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ badUrl ->
                    Err <| Errors.fromStrings [ "Bad Url: " ++ badUrl ]

                Http.Timeout_ ->
                    Err <| Errors.fromStrings [ "Timeout" ]

                Http.NetworkError_ ->
                    Err <| Errors.fromStrings [ "Network Error" ]

                Http.BadStatus_ metadata body ->
                    case Decode.decodeString Errors.decoder body of
                        Ok errors ->
                            Err errors

                        Err _ ->
                            Err <| Errors.fromStrings [ "Server Error: " ++ metadata.statusText ]

                Http.GoodStatus_ _ body ->
                    case Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err <| Errors.fromStrings [ "Bad Body: " ++ Decode.errorToString err ]



-- TYPES


{-| Get a URL to the Conduit API.

This is not publicly exposed, because we want to make sure the only way to get one of these URLs is from this module.

-}
type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    Url.Builder.crossOrigin "https://conduit.productionready.io" ("api" :: paths) queryParams
        |> Endpoint



-- ENDPOINTS


{-| -}
login : Endpoint
login =
    url [ "users", "login" ] []


{-| -}
user : Endpoint
user =
    url [ "user" ] []


{-| -}
users : Endpoint
users =
    url [ "users" ] []


{-| -}
follow : Username -> Endpoint
follow uname =
    url [ "profiles", Username.toString uname, "follow" ] []



-- ARTICLE ENDPOINTS


{-| -}
article : Slug -> Endpoint
article slug =
    url [ "articles", Slug.toString slug ] []


{-| -}
comments : Slug -> Endpoint
comments slug =
    url [ "articles", Slug.toString slug, "comments" ] []


{-| -}
comment : Slug -> CommentId -> Endpoint
comment slug commentId =
    url [ "articles", Slug.toString slug, "comments", Article.Comment.Id.toString commentId ] []


{-| -}
favorite : Slug -> Endpoint
favorite slug =
    url [ "articles", Slug.toString slug, "favorite" ] []


{-| -}
articles : List QueryParameter -> Endpoint
articles params =
    url [ "articles" ] params


{-| -}
profiles : Username -> Endpoint
profiles name =
    url [ "profiles", Username.toString name ] []


{-| -}
feed : List QueryParameter -> Endpoint
feed params =
    url [ "articles", "feed" ] params


{-| -}
tags : Endpoint
tags =
    url [ "tags" ] []
