module Pages.SignIn exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element as Elem
import Element.Input as Input
import Route exposing (Route)
import Html.Events
import Page exposing (Page)
import Shared
import View exposing (View)

import Auth.Authentication


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = SigningIn
    | SigningOut


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SigningIn ->
            ( model
            , Effect.signIn
            )

        SigningOut ->
            ( model
            , Effect.signOut
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.SignIn"
    , attributes = []
    , element = Elem.el [ Elem.centerX ]
        (viewPage model)
    }

viewPage : Model -> Elem.Element Msg
viewPage model =
    Elem.column []
        [ Input.button []
            { onPress = Just SigningIn
            , label = Elem.text "Sign In"
            }
        , Input.button []
            { onPress = Just SigningOut
            , label = Elem.text "Sign Out"
            }
        ]
