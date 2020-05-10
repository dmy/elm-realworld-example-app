module View.Button exposing (alignRight, delete, edit, favorite, follow, submit, unfavorite, unfollow)

import Api exposing (Cred)
import Author exposing (Author(..), FollowedAuthor, UnfollowedAuthor)
import Author.Username as Username
import Html exposing (Attribute, Html)
import Html.Attributes exposing (class)
import Html.Events as Events
import Json.Decode
import Route exposing (Route)
import View.Icon



-- ATTRIBUTES


alignRight : Attribute msg
alignRight =
    class "pull-xs-right"



-- BUTTONS


delete : String -> msg -> Html msg
delete label onClick =
    Html.button [ class "btn btn-outline-danger btn-sm", Events.onClick onClick ]
        [ View.Icon.delete, Html.text label ]


edit : String -> Route -> Html msg
edit label route =
    Html.a [ class "btn btn-outline-secondary btn-sm", Route.href route ]
        [ View.Icon.edit, Html.text label ]


submit : String -> List (Attribute msg) -> Html msg
submit label attrs =
    Html.button (class "btn btn-lg btn-primary pull-xs-right" :: attrs)
        [ Html.text label ]


follow : (Cred -> UnfollowedAuthor -> msg) -> Cred -> UnfollowedAuthor -> Html msg
follow toMsg cred author =
    toggleFollow "Follow"
        [ "btn-outline-secondary" ]
        (toMsg cred author)
        (Unfollowed author)


unfollow : (Cred -> FollowedAuthor -> msg) -> Cred -> FollowedAuthor -> Html msg
unfollow toMsg cred author =
    toggleFollow "Unfollow"
        [ "btn-secondary" ]
        (toMsg cred author)
        (Followed author)


toggleFollow : String -> List String -> msg -> Author -> Html msg
toggleFollow txt extraClasses onClick author =
    let
        classStr =
            "btn btn-sm " ++ String.join " " extraClasses ++ " action-btn"

        caption =
            "\u{00A0}" ++ txt ++ " " ++ Username.toString (Author.name author)
    in
    Html.button [ class classStr, Events.onClick onClick ]
        [ View.Icon.add, Html.text caption ]


favorite :
    msg
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
favorite msg attrs kids =
    toggleFavorite "btn btn-sm btn-outline-primary" msg attrs kids


unfavorite :
    msg
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
unfavorite msg attrs kids =
    toggleFavorite "btn btn-sm btn-primary" msg attrs kids


toggleFavorite :
    String
    -> msg
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
toggleFavorite classStr msg attrs kids =
    Html.button
        (class classStr :: onClickStopPropagation msg :: attrs)
        (Html.i [ class "ion-heart" ] [] :: kids)


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    Events.stopPropagationOn "click"
        (Json.Decode.succeed ( msg, True ))
