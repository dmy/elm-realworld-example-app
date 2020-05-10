module Env exposing
    ( Env
    , init
    , navKey, timeZone
    , updateTimeZone
    )

{-| The application environment.

@docs Env


# Create

@docs init


# Query

@docs navKey, timeZone


# Update

@docs updateTimeZone

-}

import Browser.Navigation as Nav
import Time


{-| The application environment.
-}
type Env
    = Env
        { timeZone : Time.Zone
        , navKey : Nav.Key
        }


{-| -}
init : Nav.Key -> Time.Zone -> Env
init key tz =
    Env
        { timeZone = tz
        , navKey = key
        }


{-| -}
navKey : Env -> Nav.Key
navKey (Env env) =
    env.navKey


{-| -}
timeZone : Env -> Time.Zone
timeZone (Env env) =
    env.timeZone


{-| -}
updateTimeZone : Time.Zone -> Env -> Env
updateTimeZone newTimZone (Env env) =
    Env { env | timeZone = newTimZone }
