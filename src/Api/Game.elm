module Api.Game exposing
    ( Game, Error
    , list, get, post
    )

import Effect exposing (Effect)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra exposing (set)
import Json.Decode.Pipeline exposing (optional, required, resolve)
import Json.Encode as Encode
import Set exposing (Set)
import Time


type alias Game =
    { id : String
    , keywords : Set String
    , questionsCount : Int
    , questionsLimit : Int
    , creationTime : Time.Posix
    }


type alias Error =
    { message : String
    , field : Maybe String
    }


-- METHODS


list :
    { onResponse : Result Http.Error (List Game) -> msg
    , token : String
    }
    -> Effect msg
list options =
    let
        url : String
        url = "https://api.tst.brainfartlab.com/quiz/v1/games"

        cmd : Cmd msg
        cmd =
            Http.request
                { url = url
                , method = "GET"
                , headers =
                    [ authHeader options.token ]
                , body = Http.emptyBody
                , expect = Http.expectJson options.onResponse gamesDecoder
                , timeout = Just 10000
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd


get :
    { onResponse : Result Http.Error Game -> msg
    , token : String
    , id : String
    }
    -> Effect msg
get options =
    let
        url : String
        url = "https://api.tst.brainfartlab.com/quiz/v1/games/" ++ options.id

        cmd : Cmd msg
        cmd =
            Http.request
                { url = url
                , method = "GET"
                , headers =
                    [ authHeader options.token ]
                , body = Http.emptyBody
                , expect = Http.expectJson options.onResponse gameDecoder
                , timeout = Just 10000
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd


post:
    { onResponse : Result (List Error) Game -> msg
    , token : String
    , keywords : Set String
    }
    -> Effect msg
post options =
    let
        url : String
        url = "https://api.tst.brainfartlab.com/quiz/v1/games"

        cmd : Cmd msg
        cmd =
            Http.request
                { url = url
                , method = "POST"
                , headers =
                    [ authHeader options.token
                    ]
                , body = Http.jsonBody ( newGameEncoder { keywords = options.keywords } )
                , expect = Http.expectStringResponse options.onResponse handleHttpResponse
                , timeout = Just 10000
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd


authHeader : String -> Http.Header
authHeader token =
    Http.header "Authorization" ("Bearer " ++ token)


-- RESPONSE


handleHttpResponse : Http.Response String -> Result (List Error) Game
handleHttpResponse response =
    case response of
        Http.BadUrl_ _ ->
            Err [ { message = "Unexpected URL format"
                  , field = Nothing
                  }
                ]

        Http.Timeout_ ->
            Err [ { message = "Request timed out"
                  , field = Nothing
                  }
                ]

        Http.NetworkError_ ->
            Err [ { message = "Unable to connect"
                  , field = Nothing
                  }
                ]

        Http.BadStatus_ { statusCode } body ->
            case statusCode of
                404 ->
                    Err [ { message = "Unable to connect"
                          , field = Nothing
                          }
                        ]

                _ ->
                    Err [ { message = "Something unexpected happened"
                          , field = Nothing
                          }
                        ]

        Http.GoodStatus_ _ body ->
            case Decode.decodeString gameDecoder body of
                Ok data ->
                    Ok data

                Err _ ->
                    Err [ { message = "Something unexpected happened"
                          , field = Nothing
                          }
                        ]


-- DECODERS


gameDecoder : Decoder Game
gameDecoder =
    let
        toDecoder : String -> Set String -> Int -> Int -> Int -> Decoder Game
        toDecoder id keywords questionsCount questionsLimit creationTime =
            Decode.succeed (Game id keywords questionsCount questionsLimit (Time.millisToPosix creationTime))
    in
    Decode.succeed toDecoder
        |> required "id" Decode.string
        |> required "keywords" (set Decode.string)
        |> required "questions_count" Decode.int
        |> required "questions_limit" Decode.int
        |> required "creation_time" Decode.int
        |> resolve

gamesDecoder : Decoder (List Game)
gamesDecoder =
    Decode.field "games" (Decode.list gameDecoder)

newGameEncoder : { keywords : Set String } -> Encode.Value
newGameEncoder { keywords } =
    Encode.object
        [ ( "keywords", Encode.list Encode.string <| Set.toList keywords)
        ]

errorsDecoder : Decoder (List Error)
errorsDecoder =
    Decode.field "errors" (Decode.list errorDecoder)

errorDecoder : Decoder Error
errorDecoder =
    Decode.succeed Error
        |> required "message" Decode.string
        |> optional "field" (Decode.map Just Decode.string) Nothing
