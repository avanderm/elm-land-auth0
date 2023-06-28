port module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    , toDatetime
    )


import Effect exposing (Effect)
import Json.Decode
import Json.Decode.Pipeline exposing (optional, required)
import Route exposing (Route)
import Route.Path
import Shared.Model
import Shared.Msg
import Task
import Time

import Auth.Auth0
import Auth.Authentication


-- FLAGS


type alias Flags =
    { user : Maybe Auth.Auth0.LoggedInUser
    }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.succeed Flags
        |> optional "user" (Json.Decode.map Just userDecoder) Nothing

userDecoder : Json.Decode.Decoder Auth.Auth0.LoggedInUser
userDecoder =
    Json.Decode.succeed Auth.Auth0.LoggedInUser
        |> required "profile" profileDecoder
        |> required "token" Json.Decode.string


profileDecoder : Json.Decode.Decoder Auth.Auth0.UserProfile
profileDecoder =
    Json.Decode.succeed Auth.Auth0.UserProfile
        |> required "email" Json.Decode.string
        |> required "email_verified" Json.Decode.bool
        |> required "picture" Json.Decode.string



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult route =
    let
        flags : Flags
        flags =
            (Debug.log "flags" flagsResult)
                |> Result.withDefault { user = Nothing }
    in
    ( { authModel = (Auth.Authentication.init Effect.auth0authorize Effect.auth0logout flags.user)
      , timeZone = Time.utc
      }
    , Effect.sendCmd (Task.perform Shared.Msg.AdjustTimeZone Time.here)
    )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        Shared.Msg.AuthenticationMsg authMsg ->
            let
                ( authModel, cmd ) =
                    Auth.Authentication.update authMsg model.authModel
            in
            ( { model | authModel = authModel }
            , Effect.sendCmd (Cmd.map Shared.Msg.AuthenticationMsg cmd)
            )

        Shared.Msg.AdjustTimeZone newZone ->
            ( { model | timeZone = newZone }
            , Effect.none
            )


-- PORTS


port auth0authResult : (Auth.Auth0.RawAuthenticationResult -> msg) -> Sub msg


-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    auth0authResult (Auth.Authentication.handleAuthResult >> Shared.Msg.AuthenticationMsg)

-- HELPER FUNCTIONS
toDatetime : Time.Posix -> Time.Zone -> String
toDatetime time zone =
    let
        year : String
        year = String.fromInt (Time.toYear zone time)

        month: String
        month = toMonth (Time.toMonth zone time)

        day : String
        day = String.fromInt (Time.toDay zone time)

        weekDay : String
        weekDay = toWeekday (Time.toWeekday zone time)

        hour : String
        hour = String.fromInt (Time.toHour zone time)

        minute: String
        minute = String.fromInt (Time.toMinute zone time)

        second : String
        second = String.fromInt (Time.toSecond zone time)
    in
    weekDay ++ " " ++ month ++ " " ++ day ++ ", " ++ year ++ " at " ++ hour ++ ":" ++ minute ++ ":" ++ second


toWeekday : Time.Weekday -> String
toWeekday weekday =
    case weekday of
        Time.Mon ->
            "Monday"

        Time.Tue ->
            "Tuesday"

        Time.Wed ->
            "Wednesday"

        Time.Thu ->
            "Thursday"

        Time.Fri ->
            "Friday"

        Time.Sat ->
            "Saturday"

        Time.Sun  ->
            "Sunday"


toMonth : Time.Month -> String
toMonth month =
    case month of
        Time.Jan->
            "January"

        Time.Feb ->
            "February"

        Time.Mar ->
            "March"

        Time.Apr ->
            "April"

        Time.May ->
            "May"

        Time.Jun ->
            "June"

        Time.Jul ->
            "July"

        Time.Aug  ->
            "August"

        Time.Sep ->
            "September"

        Time.Oct ->
            "October"

        Time.Nov ->
            "November"

        Time.Dec ->
            "December"
