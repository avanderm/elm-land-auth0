module Pages.Game.Id_ exposing (Model, Msg, page)

import Auth
import Effect exposing (Effect)
import Element exposing (Element, el, row, column)
import Element.Input as Input
import Element.Region as Region
import Route exposing (Route)
import Html exposing (Html, input, label)
import Html.Attributes as Attr
import Html.Events
import Http
import Page exposing (Page)
import Shared
import View exposing (View)

import Api
import Api.Question exposing (Question, Feedback)


page : Auth.User -> Shared.Model -> Route { id : String } -> Page Model Msg
page user shared route =
    Page.new
        { init = init user route
        , update = update user
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { gameId : GameId
    , progress : Progress
    }

type Progress
    = Prompted (Api.Data Question) (Maybe Choice)
    | Answered Question Choice (Api.Data Feedback)

type alias GameId
    = String

type Choice
    = Choice Int


init : Auth.User -> Route { id : String } -> () -> ( Model, Effect Msg )
init user route () =
    ( { gameId = route.params.id
      , progress = Prompted Api.Loading Nothing
      }
    , Api.Question.ask
        { onResponse = QuestionPrompted
        , token = user.token
        , gameId = route.params.id
        }
    )



-- UPDATE


type Msg
    = QuestionPrompted (Result Http.Error Question)
    | QuestionAnswered (Result Http.Error Feedback)
    | ChooseOption Choice
    | SubmitChoice
    | NextQuestion


update : Auth.User -> Msg -> Model -> ( Model, Effect Msg )
update user msg model =
    case msg of
        QuestionPrompted (Ok question) ->
            ( { model | progress = Prompted (Api.Success question) Nothing }
            , Effect.none
            )

        QuestionPrompted (Err httpError) ->
            ( model
            , Effect.none
            )

        ChooseOption choice ->
            case model.progress of
                Prompted questionData _ ->
                    ( { model | progress = Prompted questionData (Just choice) }
                    , Effect.none
                    )

                _ ->
                    ( model
                    , Effect.none
                    )

        SubmitChoice ->
            case model.progress of
                Prompted (Api.Success question) (Just (Choice choice)) ->
                    ( { model | progress = Answered question (Choice choice) Api.Loading }
                    , Api.Question.answer
                        { onResponse = QuestionAnswered
                        , token = user.token
                        , gameId = model.gameId
                        , choice = choice
                        }
                    )

                _ ->
                    ( model
                    , Effect.none
                    )

        NextQuestion ->
            ( { model | progress = Prompted Api.Loading Nothing }
            , Api.Question.ask
                { onResponse = QuestionPrompted
                , token = user.token
                , gameId = model.gameId
                }
            )

        QuestionAnswered (Ok feedback) ->
            case model.progress of
                Answered question choice _ ->
                    ( { model | progress = Answered question choice (Api.Success feedback) }
                    , Effect.none
                    )

                _ ->
                    ( model
                    , Effect.none
                    )

        QuestionAnswered (Err httpError) ->
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
    { title = "Pages.Game.Id_"
    , attributes = []
    , element = el [ Element.centerX ]
        (viewPage model)
    }

viewPage : Model -> Element Msg
viewPage model =
    column []
        [ el [ Region.heading 1 ] (Element.text model.gameId)
        , case model.progress of
            Prompted questionData choice ->
                case questionData of
                    Api.Loading ->
                        el []
                            (Element.text "Loading question...")

                    Api.Success question ->
                        column []
                            [ viewQuestion question choice
                            ]

                    Api.Failure httpError ->
                        el []
                            (Element.text (Api.toUserFriendlyMessage httpError))

            Answered question choice feedbackData ->
                case feedbackData of
                    Api.Loading ->
                        column []
                            [ viewQuestion question (Just choice)
                            , el [] (Element.text "Loading feedback...")
                            ]

                    Api.Success feedback ->
                        column []
                            [ viewQuestion question (Just choice)
                            , viewFeedback feedback
                            ]

                    Api.Failure httpError ->
                        el []
                            (Element.text (Api.toUserFriendlyMessage httpError))
        ]

viewQuestion : Question -> Maybe Choice -> Element Msg
viewQuestion question choice =
    column []
        [ el [ Region.heading 2 ] (Element.text question.prompt)
        , viewOptions question choice
        , viewSubmit
        ]

viewOptions : Question -> Maybe Choice -> Element Msg
viewOptions question choice =
    Input.radio []
        { onChange = ChooseOption
        , selected = choice
        , label = Input.labelAbove [] (Element.text "Choice")
        , options =
            (List.indexedMap viewOption question.options)
        }

viewOption : Int -> String -> Input.Option Choice index
viewOption index option =
    Input.option (Choice (index + 1)) (Element.text option)

viewSubmit : Element Msg
viewSubmit =
    Input.button []
        { onPress = Just SubmitChoice
        , label = Element.text "Choose"
        }

viewFeedback : Feedback -> Element Msg
viewFeedback feedback =
    column []
        [ Element.paragraph []
            [ if feedback.result then
                Element.text "Correct"
              else
                Element.text "Wrong"
            ]
        , Element.paragraph []
            [ Element.text ("The answer is: " ++ feedback.solution) ]
        , Element.paragraph []
            [ Element.text feedback.clarification ]
        , Input.button []
            { onPress = Just NextQuestion
            , label = Element.text "Next"
            }
        ]
