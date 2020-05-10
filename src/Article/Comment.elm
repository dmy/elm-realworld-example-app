module Article.Comment exposing (Comment, author, body, createdAt, delete, id, list, post)

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Article.Comment.Id exposing (CommentId)
import Article.Slug exposing (Slug)
import Author exposing (Author)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Time



-- TYPES


type Comment
    = Comment Internals


type alias Internals =
    { id : CommentId
    , body : String
    , createdAt : Time.Posix
    , author : Author
    }



-- INFO


id : Comment -> CommentId
id (Comment comment) =
    comment.id


body : Comment -> String
body (Comment comment) =
    comment.body


createdAt : Comment -> Time.Posix
createdAt (Comment comment) =
    comment.createdAt


author : Comment -> Author
author (Comment comment) =
    comment.author



-- LIST


list : Slug -> Maybe Cred -> Api.Request (List Comment)
list articleSlug maybeCred =
    Decode.field "comments" (Decode.list (decoder maybeCred))
        |> Api.get (Endpoint.comments articleSlug) maybeCred



-- POST


post : Slug -> String -> Cred -> Api.Request Comment
post articleSlug commentBody cred =
    let
        bod =
            encodeCommentBody commentBody
                |> Http.jsonBody
    in
    Decode.field "comment" (decoder (Just cred))
        |> Api.post (Endpoint.comments articleSlug) (Just cred) bod


encodeCommentBody : String -> Value
encodeCommentBody str =
    Encode.object [ ( "comment", Encode.object [ ( "body", Encode.string str ) ] ) ]



-- DELETE


delete : Slug -> CommentId -> Cred -> Api.Request ()
delete articleSlug commentId cred =
    Api.delete (Endpoint.comment articleSlug commentId) cred (Decode.succeed ())



-- SERIALIZATION


decoder : Maybe Cred -> Decoder Comment
decoder maybeCred =
    Decode.map4 Internals
        (Decode.field "id" Article.Comment.Id.decoder)
        (Decode.field "body" Decode.string)
        (Decode.field "createdAt" Iso8601.decoder)
        (Decode.field "author" (Author.decoder maybeCred))
        |> Decode.map Comment
