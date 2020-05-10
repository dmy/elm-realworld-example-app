module Author.Username exposing (Username, decoder, encode, toString, urlParser)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Url.Parser



-- TYPES


type Username
    = Username String



-- CREATE


decoder : Decoder Username
decoder =
    Decode.map Username Decode.string


encode : Username -> Value
encode (Username username) =
    Encode.string username



-- TRANSFORM


toString : Username -> String
toString (Username name) =
    name


urlParser : Url.Parser.Parser (Username -> a) a
urlParser =
    Url.Parser.custom "USERNAME" (\str -> Just (Username str))
