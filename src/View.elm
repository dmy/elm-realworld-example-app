module View exposing
    ( nothing, loading
    , header, footer, navLink
    , errors, dismissableErrors
    , authorLink, username, avatarLink, avatar
    , textInput, passwordInput, multilineInput
    , login, register
    , home, tags, tag
    , tabs, pagination
    , editor, settings
    , article, articlePreview, date, comment, newComment
    , profile, profileAuthor, profileFeed
    )

{-|

@docs nothing, loading, mapDocument
@docs header, footer, navLink
@docs errors, dismissableErrors
@docs authorLink, username, avatarLink, avatar
@docs textInput, passwordInput, multilineInput
@docs login, register
@docs home, tags, tag
@docs tabs, pagination
@docs editor, settings
@docs article, articlePreview, date, comment, newComment
@docs profile, profileAuthor, profileFeed

-}

import Api exposing (Cred)
import Article exposing (Article, Full, Metadata, Preview)
import Article.Body
import Article.Comment as Comment exposing (Comment)
import Article.Comment.Id exposing (CommentId)
import Article.Slug exposing (Slug)
import Article.Tag as Tag exposing (Tag)
import Author exposing (Author(..))
import Author.Avatar as Avatar
import Author.Username as Username exposing (Username)
import Errors exposing (Errors)
import Html exposing (Attribute, Html, a, button, div, h1, h4, hr, i, img, li, nav, p, span, text, ul)
import Html.Attributes as Attributes exposing (class, href, style)
import Html.Events as Events
import Route
import Session exposing (Session)
import Time exposing (Month(..))



-- MISC


nothing : Html msg
nothing =
    text ""


loading : String -> Html msg
loading description =
    div [ class "loader" ]
        [ text ("Loading " ++ description ++ "...") ]



-- ERRORS


errors : Errors -> Html msg
errors errs =
    Html.ul [ class "error-messages" ]
        (List.map error <| Errors.toStrings errs)


error : String -> Html msg
error err =
    Html.li [] [ Html.text err ]


{-| Render dismissable errors. We use this all over the place!
-}
dismissableErrors : msg -> Errors -> Html msg
dismissableErrors dismissErrors errs =
    if Errors.isEmpty errs then
        nothing

    else
        div
            [ class "error-messages"
            , style "position" "fixed"
            , style "top" "0"
            , style "background" "rgb(250, 250, 250)"
            , style "padding" "20px"
            , style "border" "1px solid"
            ]
        <|
            List.map (\err -> p [] [ text err ]) (Errors.toStrings errs)
                ++ [ button [ Events.onClick dismissErrors ] [ text "Ok" ] ]



-- USERS


authorLink : Author -> Html msg
authorLink author =
    let
        name =
            Author.name author
    in
    a [ class "author", Route.href (Route.Profile name) ]
        [ username name ]


avatarLink : Author -> Html msg
avatarLink author =
    a [ Route.href (Route.Profile (Author.name author)) ]
        [ img [ Author.avatarSrc author ] [] ]


avatar : Session -> Html msg
avatar session =
    img [ class "user-pic", Avatar.src (Session.avatar session) ] []


username : Username -> Html msg
username =
    Username.toString >> text



-- PAGE


header : List (Html msg) -> Html msg
header links =
    nav [ class "navbar navbar-light" ]
        [ div [ class "container" ]
            [ a [ class "navbar-brand", Route.href Route.Home ]
                [ text "conduit" ]
            , ul [ class "nav navbar-nav pull-xs-right" ] links
            ]
        ]


navLink : Bool -> Attribute msg -> List (Html msg) -> Html msg
navLink isActive href content =
    li [ Attributes.classList [ ( "nav-item", True ), ( "active", isActive ) ] ]
        [ a [ class "nav-link", href ] content ]


footer : Html msg
footer =
    Html.footer []
        [ div [ class "container" ]
            [ a [ class "logo-font", href "/" ] [ text "conduit" ]
            , span [ class "attribution" ]
                [ text "An interactive learning project from "
                , a [ href "https://thinkster.io" ] [ text "Thinkster" ]
                , text ". Code & design licensed under MIT."
                ]
            ]
        ]



-- LOGIN / REGISTER


login : Errors -> Html msg -> Html msg
login =
    authenticate "Sign in" <|
        a [ Route.href Route.Register ] [ text "Need an account?" ]


register : Errors -> Html msg -> Html msg
register =
    authenticate "Sign up" <|
        a [ Route.href Route.Login ] [ text "Have an account?" ]


authenticate : String -> Html msg -> Errors -> Html msg -> Html msg
authenticate title accountLink errs form =
    div [ class "auth-page" ]
        [ div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                    [ h1 [ class "text-xs-center" ] [ text title ]
                    , p [ class "text-xs-center" ] [ accountLink ]
                    , errors errs
                    , form
                    ]
                ]
            ]
        ]



-- HOME


home : List (Html msg) -> Html msg -> Html msg
home feed sidebar =
    div [ class "home-page" ]
        [ banner
        , div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-9" ]
                    [ div [ class "feed-toggle" ]
                        feed
                    ]
                , div [ class "col-md-3" ]
                    [ sidebar ]
                ]
            ]
        ]


banner : Html msg
banner =
    div [ class "banner" ]
        [ div [ class "container" ]
            [ h1 [ class "logo-font" ] [ text "conduit" ]
            , p [] [ text "A place to share your knowledge." ]
            ]
        ]


tags : List (Html msg) -> Html msg
tags popularTags =
    div [ class "sidebar" ] <|
        [ p [] [ text "Popular Tags" ]
        , div [ class "tag-list" ]
            popularTags
        ]


tag : (Tag -> msg) -> Tag -> Html msg
tag onTagClick tagName =
    link
        [ class "tag-pill tag-default"
        , Events.onClick (onTagClick tagName)
        ]
        [ text (Tag.toString tagName) ]



-- FEED


tabs : List ( String, msg ) -> ( String, msg ) -> List ( String, msg ) -> Html msg
tabs before selected after =
    ul [ class "nav nav-pills outline-active" ] <|
        List.concat
            [ List.map (tab []) before
            , [ tab [ class "active" ] selected ]
            , List.map (tab []) after
            ]


tab : List (Attribute msg) -> ( String, msg ) -> Html msg
tab attrs ( name, msg ) =
    li [ class "nav-item" ]
        [ link (class "nav-link" :: Events.onClick msg :: attrs)
            [ text name ]
        ]



-- EDITOR


editor : List (Html msg) -> Html msg
editor form =
    div [ class "editor-page" ]
        [ div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-10 offset-md-1 col-xs-12" ]
                    form
                ]
            ]
        ]



-- SETTINGS


settings : Errors -> Html msg -> Html msg
settings errs form =
    div [ class "settings-page" ]
        [ div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-6 offset-md-3 col-xs-12" ] <|
                    [ h1 [ class "text-xs-center" ] [ text "Your Settings" ]
                    , errors errs
                    , form
                    ]
                ]
            ]
        ]



-- ARTICLE


articlePreview :
    Time.Zone
    -> Maybe Cred
    -> (Maybe Cred -> Slug -> Metadata -> Html msg)
    -> Article Preview
    -> Html msg
articlePreview timeZone maybeCred faveButton preview =
    let
        author =
            Article.author preview

        metadata =
            Article.metadata preview

        slug =
            Article.slug preview
    in
    div [ class "article-preview" ]
        [ div [ class "article-meta" ]
            [ avatarLink author
            , div [ class "info" ]
                [ authorLink author
                , date timeZone metadata.createdAt
                ]
            , faveButton maybeCred slug metadata
            ]
        , a [ class "preview-link", Route.href (Route.Article slug) ]
            [ h1 [] [ text metadata.title ]
            , p [] [ text metadata.description ]
            , span [] [ text "Read more..." ]
            , ul [ class "tag-list" ]
                (List.map articlePreviewTag metadata.tags)
            ]
        ]


articlePreviewTag : String -> Html msg
articlePreviewTag tagName =
    li [ class "tag-default tag-pill tag-outline" ] [ text tagName ]


article : Time.Zone -> Errors -> msg -> Article Full -> List (Html msg) -> List (Html msg) -> Html msg
article timeZone errs onDismissErrors article_ buttons comments =
    let
        author =
            Article.author article_
    in
    div [ class "article-page" ]
        [ articleBanner timeZone errs onDismissErrors author article_ buttons
        , articleContent timeZone author article_ buttons comments
        ]


articleBanner : Time.Zone -> Errors -> msg -> Author -> Article Full -> List (Html msg) -> Html msg
articleBanner timeZone errs onDismissErrors author article_ buttons =
    div [ class "banner" ]
        [ div [ class "container" ]
            [ h1 [] [ text (Article.metadata article_).title ]
            , div [ class "article-meta" ] <|
                List.append
                    [ a [ Route.href (Route.Profile (Author.name author)) ]
                        [ img [ Author.avatarSrc author ] [] ]
                    , div [ class "info" ]
                        [ authorLink author
                        , date timeZone (Article.metadata article_).createdAt
                        ]
                    ]
                    buttons
            , dismissableErrors onDismissErrors errs
            ]
        ]


articleContent : Time.Zone -> Author -> Article Full -> List (Html msg) -> List (Html msg) -> Html msg
articleContent timeZone author article_ buttons comments =
    div [ class "container page" ]
        [ div [ class "row article-content" ]
            [ div [ class "col-md-12" ]
                [ Article.Body.toHtml (Article.body article_) [] ]
            ]
        , hr [] []
        , div [ class "article-actions" ]
            [ div [ class "article-meta" ] <|
                List.append
                    [ avatarLink author
                    , div [ class "info" ]
                        [ authorLink author
                        , date timeZone (Article.metadata article_).createdAt
                        ]
                    ]
                    buttons
            ]
        , div [ class "row" ]
            [ div [ class "col-xs-12 col-md-8 offset-md-2" ]
                comments
            ]
        ]


comment : Time.Zone -> Session -> (Cred -> CommentId -> msg) -> Comment -> Html msg
comment timeZone session onDelete cmt =
    let
        author =
            Comment.author cmt

        name =
            Author.name author
    in
    div [ class "card" ]
        [ div [ class "card-block" ]
            [ p [ class "card-text" ] [ text (Comment.body cmt) ] ]
        , div [ class "card-footer" ]
            [ a [ class "comment-author", href "" ]
                [ img
                    [ class "comment-author-img"
                    , Author.avatarSrc author
                    ]
                    []
                , text " "
                ]
            , text " "
            , a [ class "comment-author", Route.href (Route.Profile name) ]
                [ username name ]
            , span [ class "date-posted" ]
                [ text (dateString timeZone <| Comment.createdAt cmt) ]
            , commentDeleteButton session onDelete cmt
            ]
        ]


commentDeleteButton : Session -> (Cred -> CommentId -> msg) -> Comment -> Html msg
commentDeleteButton session onDelete cmt =
    case ( Comment.author cmt, Session.credentials session ) of
        ( Authenticated _, Just cred ) ->
            span
                [ class "mod-options"
                , Events.onClick (onDelete cred (Comment.id cmt))
                ]
                [ i [ class "ion-trash-a" ] [] ]

        _ ->
            -- You can't delete other peoples' comments!
            nothing


newComment :
    Session
    ->
        { onInput : String -> msg
        , onSubmit : Cred -> msg
        , comment : String
        , disabled : Bool
        }
    -> Html msg
newComment session config =
    case Session.credentials session of
        Just cred ->
            Html.form
                [ class "card comment-form"
                , Events.onSubmit (config.onSubmit cred)
                ]
                [ div [ class "card-block" ]
                    [ multilineInput "Write a comment..." config.onInput config.comment ]
                , div [ class "card-footer" ]
                    [ img [ class "comment-author-img", (Session.avatar >> Avatar.src) session ] []
                    , button
                        [ class "btn btn-sm btn-primary"
                        , Attributes.disabled config.disabled
                        ]
                        [ text "Post Comment" ]
                    ]
                ]

        Nothing ->
            p []
                [ a [ Route.href Route.Login ] [ text "Sign in" ]
                , text " or "
                , a [ Route.href Route.Register ] [ text "sign up" ]
                , text " to comment."
                ]



-- PROFILE


profile : List (Html msg) -> Html msg
profile kids =
    div [ class "profile-page" ] kids


profileAuthor : (Author -> Html msg) -> Author -> Html msg
profileAuthor followButton author =
    div [ class "user-info" ]
        [ div [ class "container" ]
            [ div [ class "row" ]
                [ div [ class "col-xs-12 col-md-10 offset-md-1" ]
                    [ img [ class "user-img", Author.avatarSrc author ] []
                    , h4 [] [ username (Author.name author) ]
                    , p [] [ text (Author.bio author) ]
                    , followButton author
                    ]
                ]
            ]
        ]


profileFeed : List (Html msg) -> Html msg
profileFeed feed =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "col-xs-12 col-md-10 offset-md-1" ]
                [ div [ class "articles-toggle" ]
                    feed
                ]
            ]
        ]



-- FORM


textInput : String -> (String -> msg) -> String -> Html msg
textInput =
    input "text"


passwordInput : String -> (String -> msg) -> String -> Html msg
passwordInput =
    input "password"


input : String -> String -> (String -> msg) -> String -> Html msg
input type_ placeholder onInput value =
    Html.fieldset [ class "form-group" ]
        [ Html.input
            [ class "form-control form-control-lg"
            , Attributes.type_ type_
            , Attributes.placeholder placeholder
            , Attributes.value value
            , Events.onInput onInput
            ]
            []
        ]


multilineInput : String -> (String -> msg) -> String -> Html msg
multilineInput label onInput value =
    Html.fieldset [ class "form-group" ]
        [ Html.textarea
            [ class "form-control form-control-lg"
            , Attributes.placeholder label
            , Attributes.rows 8
            , Attributes.value value
            , Events.onInput onInput
            ]
            []
        ]



-- TIME


date : Time.Zone -> Time.Posix -> Html msg
date timeZone time =
    span [ class "date" ] [ text (dateString timeZone time) ]


{-| Format a time as a date String, like so:

    "February 14, 2018"

For more complex date formatting scenarios, here's a nice package:
<https://package.elm-lang.org/packages/ryannhg/date-format/latest/>

-}
dateString : Time.Zone -> Time.Posix -> String
dateString zone time =
    let
        month =
            case Time.toMonth zone time of
                Jan -> "January"
                Feb -> "February"
                Mar -> "March"
                Apr -> "April"
                May -> "May"
                Jun -> "June"
                Jul -> "July"
                Aug -> "August"
                Sep -> "September"
                Oct -> "October"
                Nov -> "November"
                Dec -> "December"

        day =
            String.fromInt (Time.toDay zone time)

        year =
            String.fromInt (Time.toYear zone time)
    in
    month ++ " " ++ day ++ ", " ++ year



-- PAGINATION


pagination : { onPageLinkClick : Int -> msg, page : Int, pages : Int } -> Html msg
pagination config =
    if config.pages > 1 then
        ul [ class "pagination" ] <|
            List.map (pageLink config) (List.range 1 config.pages)

    else
        nothing


pageLink : { r | onPageLinkClick : Int -> msg, page : Int } -> Int -> Html msg
pageLink config page =
    li [ Attributes.classList [ ( "page-item", True ), ( "active", page == config.page ) ] ]
        [ link
            [ class "page-link"
            , Events.onClick (config.onPageLinkClick page)
            ]
            [ text (String.fromInt page) ]
        ]



-- LINKS


{-| The RealWorld CSS requires an href to work properly.
-}
link : List (Attribute msg) -> List (Html msg) -> Html msg
link attrs kids =
    a (href "" :: attrs) kids
