module Pages.Home_ exposing (Model, Msg, page)

import Auth
import Effect exposing (Effect)
import Route exposing (Route)
import Element as Elem
import Layouts
import Page exposing (Page)
import Shared
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (toLayout user)


toLayout : Auth.User -> Model -> Layouts.Layout
toLayout user model =
    Layouts.Navigation
        { navigation =
            { title = "Home"
            , user = user
            }
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
    = ExampleMsgReplaceMe


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ExampleMsgReplaceMe ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.Home_"
    , attributes = []
    , element = Elem.el [ Elem.centerX, Elem.centerY ]
        (Elem.text "/")
    }
