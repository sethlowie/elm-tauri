module Tauri exposing
    ( Error(..)
    , emptyBody
    , expectJson
    , expectString
    , expectWhatever
    , invoke
    , jsonBody
    )

import Http
import Json.Decode as D exposing (Decoder)
import Json.Encode as E



{--
TODO:
 - Custom Type for Invoke... etc
        "Unknown Tauri operation 'invike'"
--}


type Error
    = NotImplemented
    | Other String


type Expect a msg
    = Json (Result Error a -> msg) (Decoder a)
    | Whatever (Result Error () -> msg)
    | ExpectString (Result Error String -> msg)


type alias Invoke a msg =
    { command : String
    , body : Http.Body
    , expect : Expect a msg
    }


emptyBody : Http.Body
emptyBody =
    Http.emptyBody


jsonBody : E.Value -> Http.Body
jsonBody e =
    Http.jsonBody e


expectJson : (Result Error a -> msg) -> Decoder a -> Expect a msg
expectJson e d =
    Json e d


expectWhatever : (Result Error () -> msg) -> Expect a msg
expectWhatever e =
    Whatever e


expectString : (Result Error String -> msg) -> Expect a msg
expectString e =
    ExpectString e


invoke : Invoke a msg -> Cmd msg
invoke cfg =
    tauri cfg.body cfg.expect <| "invoke/" ++ cfg.command


tauri : Http.Body -> Expect a msg -> String -> Cmd msg
tauri body expect url =
    Http.request
        { method = "TAURI"
        , body = body
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        , expect =
            case expect of
                Json e d ->
                    expectHttpJson e d

                Whatever e ->
                    expectHttpWhatever e

                ExpectString e ->
                    expectHttpString e
        , url = url
        }


expectHttpJson : (Result Error a -> msg) -> D.Decoder a -> Http.Expect msg
expectHttpJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Other ("bad url:" ++ url))

                Http.Timeout_ ->
                    Err (Other "got timeout")

                Http.NetworkError_ ->
                    Err (Other "network error")

                Http.BadStatus_ metadata body ->
                    Err (Other ("bad status: " ++ String.fromInt metadata.statusCode))

                Http.GoodStatus_ metadata body ->
                    case D.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Other (D.errorToString err))


expectHttpString : (Result Error String -> msg) -> Http.Expect msg
expectHttpString toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Other ("bad url:" ++ url))

                Http.Timeout_ ->
                    Err (Other "got timeout")

                Http.NetworkError_ ->
                    Err (Other "network error")

                Http.BadStatus_ metadata _ ->
                    Err (Other ("bad status: " ++ String.fromInt metadata.statusCode))

                Http.GoodStatus_ _ body ->
                    Ok body


expectHttpWhatever : (Result Error () -> msg) -> Http.Expect msg
expectHttpWhatever toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Other ("bad url:" ++ url))

                Http.Timeout_ ->
                    Err (Other "got timeout")

                Http.NetworkError_ ->
                    Err (Other "network error")

                Http.BadStatus_ metadata body ->
                    Err (Other ("bad status: " ++ String.fromInt metadata.statusCode))

                Http.GoodStatus_ metadata body ->
                    Ok ()
