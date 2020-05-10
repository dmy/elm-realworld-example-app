module Article exposing (Article, Full, Metadata, Preview, author, body, create, edit, favorite, fetch, fromPreview, fullDecoder, mapAuthor, metadata, previewDecoder, slug, unfavorite)

{-| The interface to the Article data structure.

This includes:

  - The Article type itself
  - Ways to make HTTP requests to retrieve and modify articles
  - Ways to access information about an article
  - Converting between various types

-}

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Article.Body as Body exposing (Body)
import Article.Slug as Slug exposing (Slug)
import Author exposing (Author)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Time



-- TYPES


{-| An article, optionally with an article body.

To see the difference between { extraInfo : a } and { extraInfo : Maybe Body },
consider the difference between the "view individual article" page (which
renders one article, including its body) and the "article feed" -
which displays multiple articles, but without bodies.

This definition for `Article` means we can write:

viewArticle : Article Full -> Html msg
viewFeed : List (Article Preview) -> Html msg

This indicates that `viewArticle` requires an article _with a `body` present_,
wereas `viewFeed` accepts articles with no bodies. (We could also have written
it as `List (Article a)` to specify that feeds can accept either articles that
have `body` present or not. Either work, given that feeds do not attempt to
read the `body` field from articles.)

This is an important distinction, because in Request.Article, the `feed`
function produces `List (Article Preview)` because the API does not return bodies.
Those articles are useful to the feed, but not to the individual article view.

-}
type Article a
    = Article Internals a


{-| Metadata about the article - its title, description, and so on.

Importantly, this module's public API exposes a way to read this metadata, but
not to alter it. This is read-only information!

If we find ourselves using any particular piece of metadata often,
for example `title`, we could expose a convenience function like this:

Article.title : Article a -> String

If you like, it's totally reasonable to expose a function like that for every one
of these fields!

(Okay, to be completely honest, exposing one function per field is how I prefer
to do it, and that's how I originally wrote this module. However, I'm aware that
this code base has become a common reference point for beginners, and I think it
is _extremely important_ that slapping some "getters and setters" on a record
does not become a habit for anyone who is getting started with Elm. The whole
point of making the Article type opaque is to create guarantees through
_selectively choosing boundaries_ around it. If you aren't selective about
where those boundaries are, and instead expose a "getter and setter" for every
field in the record, the result is an API with no more guarantees than if you'd
exposed the entire record directly! It is so important to me that beginners not
fall into the terrible "getters and setters" trap that I've exposed this
Metadata record instead of exposing a single function for each of its fields,
as I did originally. This record is not a bad way to do it, by any means,
but if this seems at odds with <https://youtu.be/x1FU3e0sT1I> - now you know why!
)

-}
type alias Metadata =
    { description : String
    , title : String
    , tags : List String
    , createdAt : Time.Posix
    , favorited : Bool
    , favoritesCount : Int
    }


type alias Internals =
    { slug : Slug
    , author : Author
    , metadata : Metadata
    }


type Preview
    = Preview


type Full
    = Full Body



-- INFO


author : Article a -> Author
author (Article internals _) =
    internals.author


metadata : Article a -> Metadata
metadata (Article internals _) =
    internals.metadata


slug : Article a -> Slug
slug (Article internals _) =
    internals.slug


body : Article Full -> Body
body (Article _ (Full extraInfo)) =
    extraInfo



-- TRANSFORM


{-| This is the only way you can transform an existing article:
you can change its author (e.g. to follow or unfollow them).
All other article data necessarily comes from the server!

We can tell this for sure by looking at the types of the exposed functions
in this module.

-}
mapAuthor : (Author -> Author) -> Article a -> Article a
mapAuthor transform (Article info extras) =
    Article { info | author = transform info.author } extras


fromPreview : Body -> Article Preview -> Article Full
fromPreview newBody (Article info Preview) =
    Article info (Full newBody)



-- SERIALIZATION


previewDecoder : Maybe Cred -> Decoder (Article Preview)
previewDecoder maybeCred =
    Decode.map2 Article
        (internalsDecoder maybeCred)
        (Decode.succeed Preview)


fullDecoder : Maybe Cred -> Decoder (Article Full)
fullDecoder maybeCred =
    Decode.map2 Article
        (internalsDecoder maybeCred)
        (Decode.field "body" (Decode.map Full Body.decoder))


internalsDecoder : Maybe Cred -> Decoder Internals
internalsDecoder maybeCred =
    Decode.map3 Internals
        (Decode.field "slug" Slug.decoder)
        (Decode.field "author" (Author.decoder maybeCred))
        metadataDecoder


metadataDecoder : Decoder Metadata
metadataDecoder =
    Decode.map6 Metadata
        (Decode.field "description" (Decode.map (Maybe.withDefault "") (Decode.nullable Decode.string)))
        (Decode.field "title" Decode.string)
        (Decode.field "tagList" (Decode.list Decode.string))
        (Decode.field "createdAt" Iso8601.decoder)
        (Decode.field "favorited" Decode.bool)
        (Decode.field "favoritesCount" Decode.int)



-- SINGLE


fetch : Slug -> Maybe Cred -> Api.Request (Article Full)
fetch articleSlug maybeCred =
    Decode.field "article" (fullDecoder maybeCred)
        |> Api.get (Endpoint.article articleSlug) maybeCred



-- EDIT


create :
    { title : String, description : String, body : String, tags : String }
    -> Cred
    -> Api.Request (Article Full)
create article cred =
    let
        articleObject =
            Encode.object
                [ ( "title", Encode.string <| String.trim article.title )
                , ( "description", Encode.string <| String.trim article.description )
                , ( "body", Encode.string <| String.trim article.body )
                , ( "tagList", Encode.list Encode.string (tagsFromString article.tags) )
                ]

        jsonBody =
            Encode.object [ ( "article", articleObject ) ]
                |> Http.jsonBody
    in
    Decode.field "article" (fullDecoder (Just cred))
        |> Api.post (Endpoint.articles []) (Just cred) jsonBody


tagsFromString : String -> List String
tagsFromString str =
    String.split " " str
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)


edit :
    Slug
    -> { r | title : String, description : String, body : String }
    -> Cred
    -> Api.Request (Article Full)
edit articleSlug article cred =
    let
        articleObject =
            Encode.object
                [ ( "title", Encode.string article.title )
                , ( "description", Encode.string article.description )
                , ( "body", Encode.string article.body )
                ]

        jsonBody =
            Encode.object [ ( "article", articleObject ) ]
                |> Http.jsonBody
    in
    Decode.field "article" (fullDecoder (Just cred))
        |> Api.put (Endpoint.article articleSlug) cred jsonBody



-- FAVORITE


favorite : Slug -> Cred -> Api.Request (Article Preview)
favorite articleSlug cred =
    Api.post (Endpoint.favorite articleSlug) (Just cred) Http.emptyBody (faveDecoder cred)


unfavorite : Slug -> Cred -> Api.Request (Article Preview)
unfavorite articleSlug cred =
    Api.delete (Endpoint.favorite articleSlug) cred (faveDecoder cred)


faveDecoder : Cred -> Decoder (Article Preview)
faveDecoder cred =
    Decode.field "article" (previewDecoder (Just cred))
