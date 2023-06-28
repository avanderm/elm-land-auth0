module Layouts.Navigation exposing (Model, Msg, Settings, layout)

import Auth
import Colors.Alpha exposing (black, lightgrey, lightblue)
import Effect exposing (Effect)
import Element exposing (Element, column, clip, el, fill, image, link, row, text, width)
import Element.Border as Border
import Element.Input as Input
import Element.Font as Font
import Element.Region as Region
import Layout exposing (Layout)
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)


type alias Settings =
    { title : String
    , user : Auth.User
    }


layout : Settings -> Shared.Model -> Route () -> Layout Model Msg mainMsg
layout settings shared route =
    Layout.new
        { init = init
        , update = update
        , view = view settings route
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init _ =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = SignOut


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SignOut ->
            ( model
            , Effect.signOut
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view :
    Settings
    -> Route ()
    -> { fromMsg : Msg -> mainMsg
       , content : View mainMsg
       , model : Model
       }
    -> View mainMsg
view settings route { fromMsg, model, content } =
    { title = content.title
    , attributes = []
    , element =
        column [ width fill ]
            [ viewNavigationBar
                { route = route
                , user = settings.user
                }
                |> Element.map fromMsg
            , viewMainContent
                { title = settings.title
                , content = content
                }
            ]
    }


viewNavigationBar : { route : Route (), user : Auth.User } -> Element Msg
viewNavigationBar { route, user } =
    el
        [ Region.navigation
        , Border.widthEach
            { bottom = 1
            , left = 0
            , right = 0
            , top = 0
            }
        , Element.paddingXY 40 10
        , Font.family
            [ Font.typeface "Helvetica"
            , Font.serif
            ]
        , width fill
        ] <|
            row [ width fill
                , Element.spacingXY 30 0
                ]
                [ el
                    [ Font.size 28
                    , Font.extraBold
                    ]
                    (text "BrainFartLab")
                , viewNavigationLinks route
                , viewUser user
                ]


viewNavigationLinks : Route () -> Element msg
viewNavigationLinks route =
    let
        viewNavigationLink : ( String, Route.Path.Path ) -> Element msg
        viewNavigationLink ( label, path ) =
            el []
                (link
                    [ if route.path == path then
                        Font.color (lightblue 1)
                      else
                        Font.color (lightgrey 1)
                    ]
                    { url = Route.Path.toString path
                    , label = text label
                    }
                )
    in
    row []
        (List.map viewNavigationLink
            [ ( "My Games", Route.Path.Game )
            ]
        )


viewUser : Auth.User -> Element Msg
viewUser user =
    el [ Element.alignRight ] <|
        row
            [ Element.spacingXY 20 0
            ]
            [ image
                [ Element.centerX
                , Element.centerY
                , Element.width (Element.px 50)
                , Border.rounded 50
                , clip
                ]
                { src = user.profile.picture
                , description = user.profile.email
                }
            , column []
                [ el [ Font.size 14 ] (text user.profile.email)
                , Input.button [ Font.size 12 ]
                    { onPress = Just SignOut
                    , label = Element.text "Sign Out"
                    }
                ]
            ]



viewMainContent : { title : String, content : View msg } -> Element msg
viewMainContent { title, content } =
    el [ Region.mainContent, width fill ] <|
        column [ width fill ]
            [ el content.attributes (content.element)
            ]
