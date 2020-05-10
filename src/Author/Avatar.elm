module Author.Avatar exposing (Avatar, decoder, default, encode, src)

import Html
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Url exposing (Url)


type Avatar
    = Avatar (Maybe Url)


decoder : Decoder Avatar
decoder =
    Decode.nullable Decode.string
        |> Decode.map (Maybe.andThen Url.fromString)
        |> Decode.map Avatar


encode : Avatar -> Value
encode (Avatar maybeUrl) =
    case maybeUrl of
        Just url ->
            Encode.string (Url.toString url)

        Nothing ->
            Encode.null


default : Avatar
default =
    Avatar (Url.fromString "https://static.productionready.io/images/smiley-cyrus.jpg")


src : Avatar -> Html.Attribute msg
src (Avatar maybeUrl) =
    case maybeUrl of
        Just url ->
            Html.Attributes.src (Url.toString url)

        Nothing ->
            src default
