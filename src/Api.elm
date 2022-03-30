module Api exposing (..)

import Http
import Model exposing (..)
import Retry
import Task
import Xml.Decode as Decode


baseUrl : String
baseUrl =
    "https://api.geekdo.com/xmlapi2"


parseXmlResponse : Decode.Decoder a -> Http.Response String -> Result Http.Error a
parseXmlResponse decoder response =
    case response of
        Http.GoodStatus_ md b ->
            case md.statusCode of
                200 ->
                    b
                        |> Decode.run decoder
                        |> Result.mapError Http.BadBody
                        |> Debug.log "Parsed"

                _ ->
                    Err (Http.BadStatus md.statusCode)

        Http.BadStatus_ md _ ->
            Err (Http.BadStatus md.statusCode)

        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError


getBoardgames : (Result Http.Error (List Boardgame) -> msg) -> String -> Cmd msg
getBoardgames msg username =
    let
        task =
            Http.task
                { method = "GET"
                , headers = []
                , url = baseUrl ++ "/collection?excludesubtype=boardgameexpansion&subtype=boardgame&brief=0&stats=1&own=1&username=" ++ username
                , body = Http.emptyBody
                , resolver = Http.stringResolver (parseXmlResponse (Decode.path [ "item" ] (Decode.list boardgameDecoder)))
                , timeout = Nothing
                }
    in
    task
        |> Retry.with [ Retry.exponentialBackoff { interval = 1500, maxInterval = 6000 } ]
        |> Task.attempt msg



-- Http.get
--     { url = baseUrl ++ "/collection?excludesubtype=boardgameexpansion&subtype=boardgame&brief=0&stats=1&username=" ++ username
--     , expect = Http.Xml.expectXml msg (Decode.path [ "item" ] (Decode.list boardgameDecoder))
--     }
