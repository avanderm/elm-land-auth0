module Shared.Msg exposing (Msg(..))

import Auth.Authentication
import Time

{-| -}


type Msg
    = AuthenticationMsg Auth.Authentication.Msg
    | AdjustTimeZone Time.Zone
