module Remote exposing
    ( Remote
    , loading
    , loaded, failed
    , get, description
    , view, viewList
    )

{-|

@docs Remote


# Create

@docs loading


# Manipulate

@docs loaded, failed


# Query

@docs get, description


# View

@docs view, viewList

-}

import Html exposing (Html)
import View


{-| A remotely loaded resource.
-}
type Remote a
    = Loading String
    | Loaded String a
    | Failed String



-- CREATE


{-| Initialize a loading resource with a description.
-}
loading : String -> Remote a
loading desc =
    Loading desc



-- MANIPULATE


{-| Store the loaded resource.
-}
loaded : a -> Remote a -> Remote a
loaded a remote =
    Loaded (description remote) a


{-| Set the loading status as failed.
-}
failed : Remote a -> Remote a
failed remote =
    Failed (description remote)


{-| Return the resource description.
-}
description : Remote a -> String
description remote =
    case remote of
        Loading desc -> desc
        Loaded desc _ -> desc
        Failed desc -> desc



-- QUERY


{-| Return the resource if loaded.
-}
get : Remote a -> Maybe a
get remote =
    case remote of
        Loading _ -> Nothing
        Loaded _ a -> Just a
        Failed _ -> Nothing



-- VIEW


{-| View a loading message, the resource or an error according to
the resource status. A `viewer` function is needed to turn the
resource into an HTML element.
-}
view : (a -> Html msg) -> Remote a -> Html msg
view viewer remote =
    case remote of
        Loading desc ->
            View.loading desc

        Loaded _ a ->
            viewer a

        Failed desc ->
            Html.text ("Error loading " ++ desc ++ ".")


{-| Like [`view`](Remote#view) but using a viewer that renders a `List (Html msg)`.
-}
viewList : (a -> List (Html msg)) -> Remote a -> List (Html msg)
viewList viewer remote =
    case remote of
        Loading desc ->
            [ View.loading desc ]

        Loaded _ a ->
            viewer a

        Failed desc ->
            [ Html.text ("Error loading " ++ desc ++ ".") ]
