module Api.Question exposing
    ( Question, Feedback
    , list, get, ask, answer
    )

import Effect exposing (Effect)
import Http
import Json.Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode

import Api.Game exposing (Game)

type alias Question =
    { prompt : String
    , options : List String
    }

type alias QuestionStatus =
    { prompt : String
    , is_answered : Bool
    }

type alias Feedback =
    { result : Bool
    , solution : String
    , clarification : String
    }


-- METHODS


list :
    { onResponse : Result Http.Error (List QuestionStatus) -> msg
    , token : String
    , game_id : String
    }
    -> Effect msg
list options =
    let
        url : String
        url = "https://api.tst.brainfartlab.com/quiz/v1/games/" ++ options.game_id ++ "/questions"

        cmd : Cmd msg
        cmd =
            Http.request
                { url = url
                , method = "GET"
                , headers =
                    [ authHeader options.token ]
                , body = Http.emptyBody
                , expect = Http.expectJson options.onResponse questionsDecoder
                , timeout = Just 10000
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd

get :
    { onResponse : Result Http.Error QuestionStatus -> msg
    , token : String
    , game_id : String
    , id : String
    }
    -> Effect msg
get options =
    let
        url : String
        url = "https://api.tst.brainfartlab.com/quiz/v1/games/" ++ options.game_id ++ "/questions/" ++ options.id

        cmd : Cmd msg
        cmd =
            Http.request
                { url = url
                , method = "GET"
                , headers =
                    [ authHeader options.token ]
                , body = Http.emptyBody
                , expect = Http.expectJson options.onResponse questionStatusDecoder
                , timeout = Just 10000
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd


ask:
    { onResponse : Result Http.Error Question -> msg
    , token : String
    , gameId : String
    }
    -> Effect msg
ask options =
    let
        url : String
        url = "https://api.tst.brainfartlab.com/quiz/v1/games/" ++ options.gameId ++ "/questions/ask"

        cmd : Cmd msg
        cmd =
            Http.request
                { url = url
                , method = "POST"
                , headers =
                    [ authHeader options.token
                    ]
                , body = Http.emptyBody
                , expect = Http.expectJson options.onResponse questionDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd


answer:
    { onResponse : Result Http.Error Feedback -> msg
    , token : String
    , gameId : String
    , choice : Int
    }
    -> Effect msg
answer options =
    let
        url : String
        url = "https://api.tst.brainfartlab.com/quiz/v1/games/" ++ options.gameId ++ "/questions/answer"

        cmd : Cmd msg
        cmd =
            Http.request
                { url = url
                , method = "POST"
                , headers =
                    [ authHeader options.token
                    ]
                , body = Http.jsonBody ( answerEncoder { choice = options.choice } )
                , expect = Http.expectJson options.onResponse feedbackDecoder
                , timeout = Just 10000
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd


authHeader : String -> Http.Header
authHeader token =
    Http.header "Authorization" ("Bearer " ++ token)


-- DECODERS


questionDecoder : Json.Decode.Decoder Question
questionDecoder =
    Json.Decode.succeed Question
        |> required "prompt" Json.Decode.string
        |> required "options" (Json.Decode.list Json.Decode.string)

questionStatusDecoder : Json.Decode.Decoder QuestionStatus
questionStatusDecoder =
    Json.Decode.succeed QuestionStatus
        |> required "prompt" Json.Decode.string
        |> required "is_answered" Json.Decode.bool

questionsDecoder : Json.Decode.Decoder (List QuestionStatus)
questionsDecoder =
    Json.Decode.field "questions" (Json.Decode.list questionStatusDecoder)

feedbackDecoder : Json.Decode.Decoder Feedback
feedbackDecoder =
    Json.Decode.succeed Feedback
        |> required "result" Json.Decode.bool
        |> required "solution" Json.Decode.string
        |> required "clarification" Json.Decode.string

answerEncoder : { choice : Int } -> Json.Encode.Value
answerEncoder { choice } =
    Json.Encode.object
        [ ( "choice", Json.Encode.int choice )
        ]
