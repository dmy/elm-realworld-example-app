module PaginatedList exposing (PaginatedList, decoder, fromList, map, params, total, values)

import Json.Decode as Decode exposing (Decoder)
import Url.Builder exposing (QueryParameter)



-- TYPES


type PaginatedList a
    = PaginatedList
        { values : List a
        , total : Int
        }



-- INFO


values : PaginatedList a -> List a
values (PaginatedList info) =
    info.values


total : PaginatedList a -> Int
total (PaginatedList info) =
    info.total



-- CREATE


fromList : Int -> List a -> PaginatedList a
fromList totalCount list =
    PaginatedList { values = list, total = totalCount }



-- TRANSFORM


map : (a -> a) -> PaginatedList a -> PaginatedList a
map transform (PaginatedList info) =
    PaginatedList { info | values = List.map transform info.values }



-- PARAMS


{-| I decided to accept a record here so I don't mess up the argument order of the two Ints.
-}
params : { page : Int, resultsPerPage : Int } -> List QueryParameter
params { page, resultsPerPage } =
    let
        offset =
            (page - 1) * resultsPerPage
    in
    [ Url.Builder.string "limit" (String.fromInt resultsPerPage)
    , Url.Builder.string "offset" (String.fromInt offset)
    ]



-- SERIALIZATION


decoder : Decoder a -> Int -> Decoder (PaginatedList a)
decoder itemDecoder resultsPerPage =
    Decode.map2 fromList
        (Decode.field "articlesCount" (pageCountDecoder resultsPerPage))
        (Decode.field "articles" (Decode.list itemDecoder))


pageCountDecoder : Int -> Decoder Int
pageCountDecoder resultsPerPage =
    Decode.int
        |> Decode.map (\results -> ceiling (toFloat results / toFloat resultsPerPage))
