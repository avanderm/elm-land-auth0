module Auth exposing (User, onPageLoad)

import Auth.Action
import Dict
import Route exposing (Route)
import Route.Path
import Shared

import Auth.Auth0


type alias User =
    Auth.Auth0.LoggedInUser


{-| Called before an auth-only page is loaded. -}
onPageLoad : Shared.Model -> Route () -> Auth.Action.Action User
onPageLoad shared route =
    case shared.authModel.state of
        Auth.Auth0.LoggedIn user ->
            Auth.Action.loadPageWithUser user

        Auth.Auth0.LoggedOut ->
            Auth.Action.pushRoute
                { path = Route.Path.SignIn
                , query =
                    Dict.fromList
                        [ ( "from", route.url.path )
                        ]
                , hash = Nothing
                }
