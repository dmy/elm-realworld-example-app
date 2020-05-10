port module Api exposing
    ( Cred, username
    , Request
    , register, login, get, post, put, delete, settings
    , application
    , storeSession, onSessionChange
    )

{-| This module is responsible for communicating to the Conduit API.


# Credentials

@docs Cred, username


# HTTP Requests

@docs Request
@docs register, login, get, post, put, delete, settings


# Application

@docs application


# Persistence

@docs storeSession, onSessionChange

-}

import Api.Endpoint as Endpoint exposing (Endpoint, Request)
import Author.Username as Username exposing (Username)
import Browser
import Browser.Navigation as Nav
import Http exposing (Body)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Url exposing (Url)



-- CRED


{-| The authentication credentials for the User (that is, the currently logged-in user.)

This includes:

  - The cred's Username
  - The cred's authentication token

By design, there is no way to access the token directly as a `String`.
It can be added to a header to a HttpBuilder for a request, or encoded
into local storage, but that's it.

This token should never be rendered to the end user, and with this API, it
can't be!

-}
type Cred
    = Cred Username String


{-| Return the username stored into the credentials.
-}
username : Cred -> Username
username (Cred name _) =
    name


credHeaders : Maybe Cred -> List Http.Header
credHeaders maybeCred =
    case maybeCred of
        Just (Cred _ token) ->
            [ Http.header "authorization" ("Token " ++ token) ]

        _ ->
            []



-- SERIALIZATION


{-| It's important that this is never exposed!

We expose `login`, `application`, `storeSession` and `onSessionChange` instead,
so we can be certain that if anyone ever has access to a `Cred` value,
it came from these APIs.

-}
credDecoder : Decoder Cred
credDecoder =
    Decode.map2 Cred
        (Decode.field "username" Username.decoder)
        (Decode.field "token" Decode.string)


decoderFromCred : Decoder (Cred -> a) -> Decoder a
decoderFromCred decoder =
    Decode.map2 (\fromCred cred -> fromCred cred)
        decoder
        credDecoder



-- PERSISTENCE


{-| Store the given session fields together with the credentials into local storage.
-}
storeSession : List ( String, Value ) -> Maybe Cred -> Cmd msg
storeSession fields maybeCred =
    case maybeCred of
        Nothing ->
            store Encode.null

        Just (Cred name token) ->
            [ ( "username", Username.encode name )
            , ( "token", Encode.string token )
            ]
                |> List.append fields
                |> Encode.object
                |> store


port store : Value -> Cmd msg


{-| An event listener for session changes.
It will trigger for any change of the session stored into the local
storage, even those from other browser tabs or windows.
-}
onSessionChange : (Maybe session -> msg) -> Decoder (Cred -> session) -> Sub msg
onSessionChange toMsg decoder =
    onStoreChange (decodeSession (decoderFromCred decoder) >> toMsg)


decodeSession : Decoder session -> Value -> Maybe session
decodeSession decoder value =
    value
        |> Decode.decodeValue Decode.string
        |> Result.andThen (Decode.decodeString decoder)
        |> Result.toMaybe


port onStoreChange : (Value -> msg) -> Sub msg



-- APPLICATION


{-| A custom application that knows how to decode the potential
credentials from the flags.
-}
application :
    Decoder (Cred -> session)
    ->
        { init : Maybe session -> Url -> Nav.Key -> ( model, Cmd msg )
        , onUrlChange : Url -> msg
        , onUrlRequest : Browser.UrlRequest -> msg
        , subscriptions : model -> Sub msg
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Browser.Document msg
        }
    -> Program Value model msg
application decoder config =
    let
        init flags =
            config.init (decodeSession (decoderFromCred decoder) flags)
    in
    Browser.application
        { init = init
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        , subscriptions = config.subscriptions
        , update = config.update
        , view = config.view
        }



-- HTTP


{-| For convenience, to avoid having to import [`Api.Endpoint`](Api-Endpoint) when using `Api`.
-}
type alias Request a =
    Endpoint.Request a


{-| HTTP GET request.
-}
get : Endpoint -> Maybe Cred -> Decoder a -> Request a
get url maybeCred decoder =
    { method = "GET"
    , headers = credHeaders maybeCred
    , url = url
    , body = Http.emptyBody
    , decoder = decoder
    }


{-| HTTP PUT request.
-}
put : Endpoint -> Cred -> Body -> Decoder a -> Request a
put url cred body decoder =
    { method = "PUT"
    , headers = credHeaders (Just cred)
    , url = url
    , body = body
    , decoder = decoder
    }


{-| HTTP POST request.
-}
post : Endpoint -> Maybe Cred -> Body -> Decoder a -> Request a
post url maybeCred body decoder =
    { method = "POST"
    , headers = credHeaders maybeCred
    , url = url
    , body = body
    , decoder = decoder
    }


{-| HTTP DELETE request.

It does not require any body as the `url` is enough to identify the
targeted resource.

-}
delete : Endpoint -> Cred -> Decoder a -> Request a
delete url cred decoder =
    { method = "DELETE"
    , headers = credHeaders (Just cred)
    , url = url
    , body = Http.emptyBody
    , decoder = decoder
    }


{-| A login request that will decode the credentials into the
[`Cred`](Cred) opaque type.
-}
login : Http.Body -> Decoder (Cred -> a) -> Request a
login body decoder =
    post Endpoint.login Nothing body (Decode.field "user" (decoderFromCred decoder))


{-| A register request that will decode the credentials into the
[`Cred`](Cred) opaque type.
-}
register : Http.Body -> Decoder (Cred -> a) -> Request a
register body decoder =
    post Endpoint.users Nothing body (Decode.field "user" (decoderFromCred decoder))


{-| A user update request that will decode the credentials into the
[`Cred`](Cred) opaque type.
-}
settings : Cred -> Http.Body -> Decoder (Cred -> a) -> Request a
settings cred body decoder =
    put Endpoint.user cred body (Decode.field "user" (decoderFromCred decoder))
