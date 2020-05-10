module Errors exposing (Errors, decoder, fromStrings, isEmpty, none, prepend, toStrings)

import Json.Decode as Decode exposing (Decoder)


type Errors
    = Errors (List String)



-- CREATION


none : Errors
none =
    Errors []


fromStrings : List String -> Errors
fromStrings =
    Errors


decoder : Decoder Errors
decoder =
    Decode.map Errors <|
        Decode.field "errors" <|
            Decode.map (List.concatMap fromPair) <|
                Decode.keyValuePairs (Decode.list Decode.string)


fromPair : ( String, List String ) -> List String
fromPair ( field, errors ) =
    List.map (\error -> field ++ " " ++ error) errors



-- QUERY


isEmpty : Errors -> Bool
isEmpty (Errors errors) =
    List.isEmpty errors



-- MANIPULATE


prepend : Errors -> Errors -> Errors
prepend (Errors firstErrors) (Errors secondErrors) =
    Errors (firstErrors ++ secondErrors)



-- STRINGS


toStrings : Errors -> List String
toStrings (Errors errors) =
    errors
