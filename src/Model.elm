module Model exposing (..)

import Xml.Decode as Decode exposing (Decoder)


type alias Boardgame =
    { bggId : String
    , title : String
    , yearPublished : Int
    , minPlayers : Float
    , maxPlayers : Float
    , minPlaytime : Float
    , maxPlaytime : Float
    , rating : Float
    , thumbnailUrl : String
    , fullSizeUrl : String
    }


boardgameDecoder : Decoder Boardgame
boardgameDecoder =
    Decode.succeed Boardgame
        |> Decode.andMap
            (Decode.stringAttr
                "objectid"
            )
        |> Decode.andMap
            (Decode.path [ "name" ] (Decode.single Decode.string))
        |> Decode.andMap (Decode.path [ "yearpublished" ] (Decode.single Decode.int))
        |> Decode.andMap (Decode.path [ "stats" ] (Decode.single (Decode.floatAttr "minplayers")))
        |> Decode.andMap (Decode.path [ "stats" ] (Decode.single (Decode.floatAttr "maxplayers")))
        |> Decode.andMap (Decode.path [ "stats" ] (Decode.single (Decode.floatAttr "minplaytime")))
        |> Decode.andMap (Decode.path [ "stats" ] (Decode.single (Decode.floatAttr "maxplaytime")))
        |> Decode.andMap (Decode.path [ "stats", "rating", "average" ] (Decode.single (Decode.floatAttr "value")))
        |> Decode.andMap (Decode.path [ "thumbnail" ] (Decode.single Decode.string))
        |> Decode.andMap (Decode.path [ "image" ] (Decode.single Decode.string))



{-
   <item objecttype="thing" objectid="5" subtype="boardgame" collid="25909673">
   <name sortindex="1">Acquire</name>
   <yearpublished>1964</yearpublished>
   <image>https://cf.geekdo-images.com/3C--kJRhi6kTPHsr9dNaWw__original/img/WeRigNSL9w0EP12b2EJfpHoh4M0=/0x0/filters:format(jpeg)/pic3299296.jpg</image>
   <thumbnail>https://cf.geekdo-images.com/3C--kJRhi6kTPHsr9dNaWw__thumb/img/EQqszaHS3n6XplVVGQfTZtGc8fE=/fit-in/200x150/filters:strip_icc()/pic3299296.jpg</thumbnail>
   <stats minplayers="2" maxplayers="6" minplaytime="90" maxplaytime="90" playingtime="90" numowned="23984">
   <rating value="N/A">
   <usersrated value="18872"/>
   <average value="7.33823"/>
   <bayesaverage value="7.13764"/>
   <stddev value="1.33676"/>
   <median value="0"/>
   <ranks>
   <rank type="subtype" id="1" name="boardgame" friendlyname="Board Game Rank" value="299" bayesaverage="7.13764"/>
   <rank type="family" id="5497" name="strategygames" friendlyname="Strategy Game Rank" value="226" bayesaverage="7.15001"/>
   </ranks>
   </rating>
   </stats>
   <status own="1" prevowned="0" fortrade="0" want="0" wanttoplay="0" wanttobuy="0" wishlist="0" preordered="0" lastmodified="2014-12-03 17:16:55"/>
   <numplays>0</numplays>
   </item>
-}
