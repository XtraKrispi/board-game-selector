module Model exposing (..)

import Xml.Decode as Decode exposing (Decoder)


type alias BoardGame =
    { bggId : String
    , title : String
    , yearPublished : Int
    , minPlayers : Float
    , maxPlayers : Float
    , minPlaytime : Maybe Float
    , maxPlaytime : Maybe Float
    , rating : Float
    , thumbnailUrl : String
    , fullSizeUrl : String
    }


boardgameDecoder : Decoder BoardGame
boardgameDecoder =
    Decode.succeed BoardGame
        |> Decode.andMap
            (Decode.stringAttr
                "objectid"
            )
        |> Decode.andMap
            (Decode.path [ "name" ] (Decode.single Decode.string))
        |> Decode.andMap (Decode.path [ "yearpublished" ] (Decode.single Decode.int))
        |> Decode.andMap (Decode.path [ "stats" ] (Decode.single (Decode.floatAttr "minplayers")))
        |> Decode.andMap (Decode.path [ "stats" ] (Decode.single (Decode.floatAttr "maxplayers")))
        |> Decode.andMap (Decode.path [ "stats" ] (Decode.single (Decode.maybe (Decode.floatAttr "minplaytime"))))
        |> Decode.andMap (Decode.path [ "stats" ] (Decode.single (Decode.maybe (Decode.floatAttr "maxplaytime"))))
        |> Decode.andMap (Decode.path [ "stats", "rating", "average" ] (Decode.single (Decode.floatAttr "value")))
        |> Decode.andMap (Decode.path [ "thumbnail" ] (Decode.single Decode.string))
        |> Decode.andMap (Decode.path [ "image" ] (Decode.single Decode.string))
