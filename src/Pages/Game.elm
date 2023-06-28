module Pages.Game exposing (Model, Msg, page)

import Auth
import Colors.Alpha exposing (black, lightgrey, lightblue)
import Effect exposing (Effect)
import Element exposing (Element, el, column, row, px, width, height)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html
import Html.Events
import Json.Decode
import Route exposing (Route)
import Route.Path
import Html.Events
import Http
import Layouts
import Page exposing (Page)
import Set exposing (Set)
import Shared
import Time
import View exposing (View)

import Api
import Api.Game exposing (Game, Error)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init user shared
        , update = update user
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (toLayout user)


toLayout : Auth.User -> Model -> Layouts.Layout
toLayout user model =
    Layouts.Navigation
        { navigation =
            { title = "Games"
            , user = user
            }
        }


-- INIT


type alias Model =
    { games : Api.Data (List Game)
    , keywordField : String
    , keywords : Set Keyword
    , errors : List Error
    , timeZone : Time.Zone
    }

type alias Keyword
    = String

type Field
    = Keywords
    | QuestionsLimit


init : Auth.User -> Shared.Model -> () -> ( Model, Effect Msg )
init user shared () =
    ( { games = Api.Loading
      , keywordField = ""
      , keywords = Set.empty
      , errors = []
      , timeZone = shared.timeZone
      }
    , Api.Game.list
        { onResponse = GameApiResponded
        , token = user.token
        }
    )



-- UPDATE


type Msg
    = GameApiResponded (Result Http.Error (List Game))
    | KeywordUpdated String
    | AddKeyword Keyword
    | RemoveKeyword Keyword
    | StartNewGame
    | DisabledStartNewGame
    | GameCreated (Result (List Error) Game)


update : Auth.User -> Msg -> Model -> ( Model, Effect Msg )
update user msg model =
    case msg of
        GameApiResponded (Ok listOfGames) ->
            ( { model | games = Api.Success listOfGames }
            , Effect.none
            )

        GameApiResponded (Err httpError) ->
            ( { model | games = Api.Failure httpError }
            , Effect.none
            )

        KeywordUpdated keyword ->
            ( { model | keywordField = keyword }
            , Effect.none
            )

        RemoveKeyword keyword ->
            let
                keywords : Set Keyword
                keywords = Set.remove keyword model.keywords
            in
            ( { model | keywords = keywords, keywordField = "" }
            , Effect.none
            )

        AddKeyword keyword ->
            let
                keywords : Set Keyword
                keywords =
                    if Set.size model.keywords < 5 then
                        Set.insert keyword model.keywords
                    else
                        model.keywords
            in
            ( { model | keywords = keywords, keywordField = "" }
            , Effect.none
            )

        StartNewGame ->
            ( model
            , Api.Game.post
                { onResponse = GameCreated
                , token = user.token
                , keywords = model.keywords
                }
            )

        DisabledStartNewGame ->
            ( model
            , Effect.none
            )

        GameCreated (Ok game) ->
            case model.games of
                Api.Success games ->
                    ( { model | games  = Api.Success (game :: games) }
                    , Effect.none
                    )

                _ ->
                    ( model
                    , Effect.none
                    )

        GameCreated (Err errors) ->
            ( { model | errors = errors }
            , Effect.none
            )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.Game"
    , attributes = [ width Element.fill ]
    , element =
        column
            [ width Element.fill
            , Border.width 1
            ]
            [ row [ Element.centerX, width Element.fill ]
                [ viewStartNewGame model
                , viewGameList model
                ]
            ]
    }

viewStartNewGame : Model -> Element Msg
viewStartNewGame model =
    column
        [ Element.width <| Element.fillPortion 1
        , Element.spacingXY 20 0
        , Element.padding 30
        , height Element.fill
        , Element.alignTop
        , Element.alignLeft
        , Border.width 1
        ]
        [ el
            [ Region.heading 2
            , Border.width 1
            , Font.color (lightblue 1)
            , Font.size 36
            , Font.bold
            , Element.padding 50
            ]
            (Element.text "New Game")
        , row
            [ Element.centerY
            , Element.spacingXY 20 5
            , Border.width 1
            ]
            [ Input.text
                [ onEnter (AddKeyword model.keywordField)
                ]
                { onChange = KeywordUpdated
                , text = model.keywordField
                , placeholder = Just <| Input.placeholder [] <| Element.text "keyword"
                , label = Input.labelHidden "Keywords"
                }
            , if Set.isEmpty model.keywords then
                Input.button
                    [ Background.color (lightgrey 1)
                    ]
                    { onPress = Just DisabledStartNewGame
                    , label = Element.text "Go!"
                    }
              else
                Input.button
                    [ Background.color (lightblue 1)
                    ]
                    { onPress = Just StartNewGame
                    , label = Element.text "Go!"
                    }
            ]
        , viewKeywords model.keywords True
        ]

viewKeywords : Set Keyword -> Bool -> Element Msg
viewKeywords keywords removable =
    row
        [ Element.spacing 10
        , Border.width 1
        , Element.height (px 50)
        ]
        <| List.map (viewKeyword removable)
        <| Set.toList keywords

viewKeyword : Bool -> Keyword -> Element Msg
viewKeyword removable keyword =
    el
        [ Element.padding 5
        , Background.color (lightgrey 1)
        , Font.size 12
        ] <|
            row [ Element.spacingXY 5 0]
                [ Element.text keyword
                , if removable then
                    Input.button [ Font.bold ]
                        { onPress = Just (RemoveKeyword keyword)
                        , label = Element.text "x"
                        }
                  else
                      Element.text ""
                ]

viewGameList : Model -> Element Msg
viewGameList model =
    column
        [ width <| Element.fillPortion 3
        , height Element.fill
        , Element.alignTop
        , Element.alignLeft
        , Element.padding 30
        , Border.width 1
        ]
        [ el
            [ Region.heading 2
            , Border.width 1
            , Font.color (lightblue 1)
            , Font.size 36
            , Font.bold
            , Element.padding 50
            ]
            (Element.text "History")
        , case model.games of
            Api.Loading ->
                el []
                    (Element.text "Loading your games...")

            Api.Success games ->
                Element.column
                    [ Element.spacing 20
                    , width Element.fill
                    ]
                    (List.map (viewGameLink model.timeZone) games)

            Api.Failure httpError ->
                el []
                    (Element.text (Api.toUserFriendlyMessage httpError))
        ]

viewGameLink : Time.Zone -> Game -> Element Msg
viewGameLink timeZone game =
    let
        gameDetailRoute : Route.Path.Path
        gameDetailRoute =
            Route.Path.Game_Id_
                { id = game.id
                }

        shortHash : String
        shortHash =
            String.slice 0 8 game.id

        creationDate : String
        creationDate =
            Shared.toDatetime game.creationTime timeZone
    in
    Element.row
        [ width Element.fill
        ]
        [ Element.link
            [ Element.padding 10
            , Border.width 1
            , width Element.fill
            ]
            { url = Route.Path.toString gameDetailRoute
            , label = viewGame shortHash creationDate game.keywords
            }
        ]

viewGame : String -> String -> Set Keyword -> Element Msg
viewGame hash creationDate keywords =
    column []
        [ row []
            [ Element.text hash
            , Element.text creationDate
            ]
        , viewKeywords keywords False
        ]

onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Json.Decode.field "key" Json.Decode.string
                |> Json.Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Json.Decode.succeed msg
                        else
                            Json.Decode.fail "Not the enter key"
                    )
            )
        )

-- ERROR

findFieldError : String -> Model -> Maybe Error
findFieldError field model =
    let
        hasMatchingField : Error -> Bool
        hasMatchingField error =
            error.field == Just field
    in
    model.errors
        |> List.filter hasMatchingField
        |> List.head

findFormError : Model -> Maybe Error
findFormError model =
    let
        doesntHaveField : Error -> Bool
        doesntHaveField error =
            error.field == Nothing
    in
    model.errors
        |> List.filter doesntHaveField
        |> List.head
