module Author exposing
    ( Author(..), FollowedAuthor, UnfollowedAuthor
    , fetch, decoder
    , bio, avatarSrc
    , follow, unfollow
    , Settings, fetchUserSettings
    , register, authenticate, edit
    , name
    )

{-| The author of an Article. It includes a Profile.

I designed this to make sure the compiler would help me keep these three
possibilities straight when displaying follow buttons and such:

  - I'm following this author.
  - I'm not following this author.
  - I _can't_ follow this author, because it's me!

To do this, I defined `Author` a custom type with three variants, one for each
of those possibilities.

I also made separate types for FollowedAuthor and UnfollowedAuthor.
They are custom type wrappers around Profile, and their sole purpose is to
help me keep track of which operations are supported.

For example, consider these functions:

follow : UnfollowedAuthor -> Cred -> Api.Request Author
unfollow : FollowedAuthor -> Cred -> Api.Request Author

These types help the compiler prevent several mistakes:

  - Displaying a Follow button for an author the user already follows.
  - Displaying an Unfollow button for an author the user already doesn't follow.
  - Displaying either button when the author is ourself.

There are still ways we could mess things up (e.g. make a button that calls Author.unfollow when you click it, but which displays "Follow" to the user) - but this rules out a bunch of potential problems.

@docs Author, FollowedAuthor, UnfollowedAuthor
@docs fetch, decoder
@docs bio, avatarSrc
@docs follow, unfollow
@docs Settings, fetchUserSettings
@docs register, authenticate, edit

-}

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Author.Avatar as Avatar exposing (Avatar)
import Author.Username as Username exposing (Username)
import Html
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Session exposing (Session)


{-| An author - either the current user, another user we're following, or
another user we aren't following.

These distinctions matter because we can only perform "follow" requests for
users we aren't following, we can only perform "unfollow" requests for
users we _are_ following, and we can't perform either for ourselves.

-}
type Author
    = Authenticated AuthenticatedAuthor
    | Followed FollowedAuthor
    | Unfollowed UnfollowedAuthor


{-| Ther logged-in user.
-}
type AuthenticatedAuthor
    = AuthenticatedAuthor Profile


{-| An author we're following.
-}
type FollowedAuthor
    = FollowedAuthor Profile


{-| An author we're not following.
-}
type UnfollowedAuthor
    = UnfollowedAuthor Profile


{-| An author profile.
-}
type alias Profile =
    { username : Username
    , bio : Maybe String
    , avatar : Avatar
    }


{-| The logged user settings.
-}
type alias Settings =
    { email : String
    , name : String
    , bio : String
    , avatar : String
    , password : String
    }


{-| Return an Author's profile.
-}
profile : Author -> Profile
profile author =
    case author of
        Authenticated (AuthenticatedAuthor prof) -> prof
        Followed (FollowedAuthor prof) -> prof
        Unfollowed (UnfollowedAuthor prof) -> prof


name : Author -> Username
name =
    profile >> .username


bio : Author -> String
bio =
    profile >> .bio >> Maybe.withDefault ""


avatarSrc : Author -> Html.Attribute msg
avatarSrc =
    profile >> .avatar >> Avatar.src



-- FETCH


fetch : Username -> Maybe Cred -> Api.Request Author
fetch username maybeCred =
    Api.get (Endpoint.profiles username) maybeCred <|
        Decode.field "profile" (decoder maybeCred)


fetchUserSettings : Cred -> Api.Request Settings
fetchUserSettings cred =
    Api.get Endpoint.user (Just cred) (Decode.field "user" userSettingsDecoder)



-- FOLLOWING


follow : UnfollowedAuthor -> Cred -> Api.Request Author
follow author cred =
    Api.post (Endpoint.follow <| name <| Unfollowed author)
        (Just cred)
        Http.emptyBody
        (Decode.field "profile" <| decoder <| Just cred)


unfollow : FollowedAuthor -> Cred -> Api.Request Author
unfollow author cred =
    Api.delete (Endpoint.follow <| name <| Followed author) cred <|
        (Decode.field "profile" <| decoder <| Just cred)



-- REQUESTS


register : { username : String, email : String, password : String } -> Api.Request Session
register user =
    let
        userObject =
            Encode.object
                [ ( "username", Encode.string <| String.trim user.username )
                , ( "email", Encode.string <| String.trim user.email )
                , ( "password", Encode.string user.password )
                ]

        body =
            Encode.object [ ( "user", userObject ) ]
                |> Http.jsonBody
    in
    Api.register body Session.decoder


authenticate : { email : String, password : String } -> Api.Request Session
authenticate { email, password } =
    let
        user =
            Encode.object
                [ ( "email", Encode.string <| String.trim email )
                , ( "password", Encode.string password )
                ]

        body =
            Encode.object [ ( "user", user ) ]
                |> Http.jsonBody
    in
    Api.login body Session.decoder


edit : Cred -> Settings -> Api.Request Session
edit cred settings =
    let
        body =
            Encode.object [ ( "user", encodeSettings settings ) ]
                |> Http.jsonBody
    in
    Api.settings cred body Session.decoder



-- SERIALIZATION


decoder : Maybe Cred -> Decoder Author
decoder maybeCred =
    Decode.map2 (toAuthor maybeCred)
        profileDecoder
        (Decode.field "following" Decode.bool)


profileDecoder : Decoder Profile
profileDecoder =
    Decode.map3 Profile
        (Decode.field "username" Username.decoder)
        (Decode.field "bio" <| Decode.nullable Decode.string)
        (Decode.field "image" <| Avatar.decoder)


toAuthor : Maybe Cred -> Profile -> Bool -> Author
toAuthor maybeCred prof followed =
    case maybeCred of
        Nothing ->
            -- If you're logged out, you can't be following anyone!
            Unfollowed (UnfollowedAuthor prof)

        Just cred ->
            if prof.username == Api.username cred then
                Authenticated (AuthenticatedAuthor prof)

            else if followed then
                Followed (FollowedAuthor prof)

            else
                Unfollowed (UnfollowedAuthor prof)


userSettingsDecoder : Decoder Settings
userSettingsDecoder =
    Decode.map5 Settings
        (Decode.field "email" Decode.string)
        (Decode.field "username" Decode.string)
        (Decode.field "bio" <| Decode.map (Maybe.withDefault "") (Decode.nullable Decode.string))
        (Decode.field "image" <| Decode.map (Maybe.withDefault "") (Decode.nullable Decode.string))
        (Decode.succeed "")


encodeSettings : Settings -> Encode.Value
encodeSettings settings =
    let
        updates =
            [ ( "email", Encode.string <| String.trim settings.email )
            , ( "username", Encode.string <| String.trim settings.name )
            , ( "bio", nullableString settings.bio )
            , ( "image", nullableString settings.avatar )
            ]

        nullableString str =
            case String.trim str of
                "" -> Encode.null
                trimmed -> Encode.string trimmed
    in
    Encode.object <|
        if String.isEmpty settings.password then
            updates

        else
            ( "password", Encode.string settings.password ) :: updates
