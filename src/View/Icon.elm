module View.Icon exposing (add, delete, edit, newPost, settings)

import Html exposing (Html)
import Html.Attributes as Attributes


add : Html msg
add =
    icon "ion-plus-round"


delete : Html msg
delete =
    icon "ion-trash-a"


edit : Html msg
edit =
    icon "ion-edit"


newPost : Html msg
newPost =
    icon "ion-compose"


settings : Html msg
settings =
    icon "ion-gear-a"



-- INTERNAL


icon : String -> Html msg
icon class =
    Html.i [ Attributes.class class ] []
