module Session exposing
    ( Session
    , guest
    , avatar, credentials, username
    , decoder, store, onChange
    )

{-| The application session.

@docs Session


# Create

@docs guest


# Query

@docs avatar, credentials, username


# Persistence

@docs decoder, store, onChange

-}

import Api exposing (Cred)
import Author.Avatar as Avatar exposing (Avatar)
import Author.Username exposing (Username)
import Json.Decode as Decode exposing (Decoder)



-- TYPES


{-| -}
type Session
    = Authenticated Avatar Cred
    | Guest



-- BUILD


{-| Create an anonymous session.
-}
guest : Session
guest =
    Guest



-- INFO


{-| Return the user avatar.
-}
avatar : Session -> Avatar
avatar session =
    case session of
        Authenticated av _ ->
            av

        Guest ->
            Avatar.default


{-| Return the session username if authenticated.
-}
username : Session -> Maybe Username
username session =
    case session of
        Authenticated _ cred ->
            Just (Api.username cred)

        Guest ->
            Nothing


{-| Return the session credentials if authenticated.
-}
credentials : Session -> Maybe Cred
credentials session =
    case session of
        Authenticated _ cred ->
            Just cred

        Guest ->
            Nothing



-- SERIALIZATION


{-| -}
decoder : Decoder (Cred -> Session)
decoder =
    Decode.map Authenticated (Decode.field "image" Avatar.decoder)



-- PERSISTENCE


{-| Store the session into local storage.
-}
store : Session -> Cmd msg
store session =
    Api.storeSession
        [ ( "image", Avatar.encode (avatar session) ) ]
        (credentials session)


{-| An event listener for [`Session`](Session#Session) changes.
It will trigger for any change of the session stored into the local
storage, even those from other browser tabs or windows.
-}
onChange : (Session -> msg) -> Sub msg
onChange toMsg =
    Api.onSessionChange (Maybe.withDefault Guest >> toMsg) decoder
