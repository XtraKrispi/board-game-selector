module Main exposing (main)

import Api
import Basics.Extra exposing (uncurry)
import Browser
import Dict exposing (Dict)
import DoubleSlider
import Helpers exposing (..)
import Html
    exposing
        ( Html
        , button
        , div
        , form
        , img
        , input
        , s
        , span
        , text
        , u
        )
import Html.Attributes
    exposing
        ( classList
        , placeholder
        , src
        , type_
        , value
        )
import Html.Events
    exposing
        ( onClick
        , onInput
        , onMouseEnter
        , onMouseLeave
        , onSubmit
        )
import Http
import Icons
import List.Extra as List
import Maybe.Extra as Maybe
import Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Round as R
import SingleSlider
import Svg exposing (svg)
import Svg.Attributes as SA


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { currentUsername : String
    , usernamesToSearch : List String
    , results :
        Dict String (WebData (List Boardgame))
    , playerCountSlider : SingleSlider.SingleSlider Msg
    , playtimeSlider : DoubleSlider.DoubleSlider Msg
    , ratingSlider : DoubleSlider.DoubleSlider Msg
    , selectedBoardgame : Maybe ( List String, Boardgame )
    , filtersVisible : Bool
    }


hasResults : { r | results : Dict String (WebData (List Boardgame)) } -> Bool
hasResults { results } =
    results
        |> Dict.toList
        |> List.any (\( _, rs ) -> RemoteData.isSuccess rs)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentUsername = ""
      , usernamesToSearch = []
      , results = Dict.empty
      , playerCountSlider =
            SingleSlider.init
                { min = 1
                , max = 20
                , step = 1
                , value = 4
                , onChange = PlayerCountFilterValueChanged
                }
      , playtimeSlider =
            DoubleSlider.init
                { min = 0
                , max = 480
                , step = 30
                , lowValue = 0
                , highValue = 480
                , onLowChange = PlaytimeFilterLowValueChanged
                , onHighChange = PlaytimeFilterHighValueChanged
                }
      , ratingSlider =
            DoubleSlider.init
                { min = 0
                , max = 10
                , step = 0.5
                , lowValue = 0
                , highValue = 10
                , onLowChange = RatingFilterLowValueChanged
                , onHighChange = RatingFilterHighValueChanged
                }
      , filtersVisible = False
      , selectedBoardgame = Nothing
      }
    , Cmd.none
    )


type Msg
    = CurrentUsernameChanged String
    | AddUsernameToSearch
    | RemoveUsername String
    | SearchForCollections
    | GotBoardgames String (Result Http.Error (List Boardgame))
    | PlayerCountFilterValueChanged Float
    | PlaytimeFilterLowValueChanged Float
    | PlaytimeFilterHighValueChanged Float
    | RatingFilterLowValueChanged Float
    | RatingFilterHighValueChanged Float
    | ToggleFilters
    | SelectBoardgame ( List String, Boardgame )
    | UnselectBoardgame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CurrentUsernameChanged un ->
            ( { model | currentUsername = un }, Cmd.none )

        AddUsernameToSearch ->
            let
                usernames =
                    if
                        model.currentUsername
                            == ""
                            || List.any
                                (\un ->
                                    String.toLower un
                                        == String.toLower model.currentUsername
                                )
                                model.usernamesToSearch
                    then
                        model.usernamesToSearch

                    else
                        model.currentUsername :: model.usernamesToSearch
            in
            ( { model
                | currentUsername = ""
                , usernamesToSearch = usernames
              }
            , Cmd.none
            )

        RemoveUsername u ->
            ( { model
                | usernamesToSearch =
                    List.filter (\s -> s /= u) model.usernamesToSearch
              }
            , Cmd.none
            )

        SearchForCollections ->
            if not (List.isEmpty model.usernamesToSearch) then
                let
                    initDict =
                        model.usernamesToSearch
                            |> List.map (\u -> ( u, Loading ))
                            |> Dict.fromList
                in
                ( { model | results = Dict.union initDict model.results }
                , model.usernamesToSearch
                    |> List.map (\u -> Api.getBoardgames (GotBoardgames u) u)
                    |> Cmd.batch
                )

            else
                ( model
                , Cmd.none
                )

        GotBoardgames u res ->
            let
                val =
                    case res of
                        Ok bgs ->
                            Success bgs

                        Err err ->
                            Failure err

                newDict =
                    Dict.insert u val model.results
            in
            ( { model | results = newDict }, Cmd.none )

        PlayerCountFilterValueChanged val ->
            ( { model
                | playerCountSlider =
                    SingleSlider.update val model.playerCountSlider
              }
            , Cmd.none
            )

        PlaytimeFilterLowValueChanged val ->
            ( { model
                | playtimeSlider =
                    DoubleSlider.updateLowValue val model.playtimeSlider
              }
            , Cmd.none
            )

        PlaytimeFilterHighValueChanged val ->
            ( { model
                | playtimeSlider =
                    DoubleSlider.updateHighValue val model.playtimeSlider
              }
            , Cmd.none
            )

        RatingFilterLowValueChanged val ->
            ( { model
                | ratingSlider =
                    DoubleSlider.updateLowValue val model.ratingSlider
              }
            , Cmd.none
            )

        RatingFilterHighValueChanged val ->
            ( { model
                | ratingSlider =
                    DoubleSlider.updateHighValue val model.ratingSlider
              }
            , Cmd.none
            )

        ToggleFilters ->
            ( { model | filtersVisible = not model.filtersVisible }, Cmd.none )

        SelectBoardgame bg ->
            ( { model | selectedBoardgame = Just bg }, Cmd.none )

        UnselectBoardgame ->
            ( { model | selectedBoardgame = Nothing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


overlay : List String -> Boardgame -> Html Msg
overlay u bg =
    let
        item icon txt =
            div
                [ classes
                    [ "text-center"
                    , "font-bold"
                    , "flex"
                    , "items-center"
                    , "space-x-1"
                    ]
                ]
                [ svg
                    [ SA.class "h-4 w-4"
                    , SA.stroke "currentColor"
                    , SA.strokeWidth "2"
                    , SA.viewBox "0 0 24 24"
                    , SA.fill "none"
                    ]
                    [ icon ]
                , span [ classes [ "text-left" ] ]
                    [ text txt ]
                ]
    in
    div
        [ classes
            [ "transition-all"
            , "bg-white"
            , "w-full"
            , "h-full"
            , "flex"
            , "items-center"
            , "justify-center"
            , "border-2"
            ]
        ]
        [ div
            [ classes
                [ "m-2"
                , "flex"
                , "flex-col"
                ]
            ]
            [ item Icons.pencil bg.title
            , item Icons.star (R.round 1 bg.rating)
            , item Icons.users (R.round 0 bg.minPlayers ++ " - " ++ R.round 0 bg.maxPlayers)
            , item Icons.clock
                (if bg.minPlaytime == bg.maxPlaytime then
                    R.round 0 bg.minPlaytime

                 else
                    R.round 0 bg.minPlaytime ++ " - " ++ R.round 0 bg.maxPlaytime
                )
            , item Icons.user (String.join ", " u)
            ]
        ]


viewBoardgame : List String -> Boardgame -> Html Msg
viewBoardgame u bg =
    div
        [ classes
            [ "flex-grow"
            , "h-24"
            , "transition-all"
            , "hover:scale-150"
            , "duration-200"
            , "group"
            ]
        , onMouseEnter (SelectBoardgame ( u, bg ))
        , onMouseLeave UnselectBoardgame
        ]
        [ img
            [ src bg.thumbnailUrl
            , classes
                [ "max-h-full"
                , "min-w-full"
                , "object-cover"
                , "align-bottom"
                ]
            ]
            []
        ]


isBoardgameApplicable : Model -> Boardgame -> Bool
isBoardgameApplicable model bg =
    let
        players =
            SingleSlider.fetchValue
                model.playerCountSlider

        ( minPlaytime, maxPlaytime ) =
            fork DoubleSlider.fetchLowValue
                DoubleSlider.fetchHighValue
                model.playtimeSlider

        ( minRating, maxRating ) =
            fork DoubleSlider.fetchLowValue
                DoubleSlider.fetchHighValue
                model.ratingSlider
    in
    bg.minPlayers
        <= players
        && bg.maxPlayers
        >= players
        && bg.minPlaytime
        >= minPlaytime
        && bg.maxPlaytime
        <= maxPlaytime
        && bg.rating
        >= minRating
        && bg.rating
        <= maxRating


allBoardGames : { r | results : Dict String (WebData (List Boardgame)) } -> List ( List String, Boardgame )
allBoardGames { results } =
    let
        getOwners bg =
            results
                |> Dict.toList
                |> List.filterMap
                    (\( u, r ) ->
                        r
                            |> RemoteData.toMaybe
                            |> Maybe.andThen
                                (\bs ->
                                    bs
                                        |> List.find (\b -> b.bggId == bg.bggId)
                                        |> Maybe.map (always u)
                                )
                    )
    in
    results
        |> Dict.values
        |> List.concatMap
            (\r ->
                case r of
                    Success bgs ->
                        bgs

                    _ ->
                        []
            )
        |> List.uniqueBy .bggId
        |> List.map (\bg -> ( getOwners bg, bg ))


viewBoardGames : Model -> Html Msg
viewBoardGames model =
    let
        boardgames =
            model
                |> allBoardGames
                |> List.sortBy (.title << Tuple.second)
                |> List.filter (isBoardgameApplicable model << Tuple.second)
                |> List.map (uncurry viewBoardgame)
                |> flip List.append
                    [ div
                        [ classes
                            [ "flex-grow-[200]"
                            , "h-24"
                            ]
                        ]
                        []
                    ]
                |> div
                    [ classes
                        [ "flex"
                        , "flex-wrap"
                        ]
                    ]
    in
    div [ classes [ "p-3" ] ] [ div [] [ boardgames ] ]


view : Model -> Html Msg
view model =
    div
        [ classes
            [ "bg-gray-300"
            , "h-screen"
            , "w-screen"
            , "flex"
            , "flex-col"
            ]
        ]
        [ div
            [ classes
                [ "p-20"
                , "flex"
                , "items-center"
                , "justify-center"
                , "h-[20vh]"
                ]
            ]
            [ span
                [ classes
                    [ "text-black"
                    , "text-3xl"
                    ]
                ]
                [ text "Board Game Selector" ]
            ]
        , div
            [ classes
                [ "m-6"
                , "bg-white"
                , "flex"
                , "flex-col"
                , "relative"
                , "pb-36"
                ]
            ]
            [ div
                [ classes
                    [ "m-6"
                    , "flex"
                    , "flex-col"
                    ]
                ]
                [ div
                    [ classes
                        [ "flex"
                        , "flex-col"
                        , "space-y-3"
                        ]
                    ]
                    (Maybe.values
                        [ Just
                            (form
                                [ onSubmit AddUsernameToSearch
                                , classes [ "flex" ]
                                ]
                                [ input
                                    [ type_ "text"
                                    , placeholder
                                        "Enter a board game geek username and press enter"
                                    , classes
                                        [ "p-6"
                                        , "w-full"
                                        , "border-2"
                                        , "rounded-l-lg"
                                        ]
                                    , onInput CurrentUsernameChanged
                                    , value model.currentUsername
                                    ]
                                    []
                                , button
                                    [ classes
                                        [ "border-r-2"
                                        , "border-y-2"
                                        , "rounded-r-lg"
                                        , "bg-gray-200"
                                        , "p-3"
                                        , "hover:bg-gray-400"
                                        , "transition-colors"
                                        ]
                                    , type_ "submit"
                                    ]
                                    [ svg
                                        [ SA.class "h-6 w-6"
                                        , SA.stroke "currentColor"
                                        , SA.strokeWidth "2"
                                        , SA.viewBox "0 0 24 24"
                                        ]
                                        [ Icons.plus ]
                                    ]
                                ]
                            )
                        , if List.length model.usernamesToSearch > 0 then
                            Just
                                (div
                                    [ classes
                                        [ "flex"
                                        , "items-center"
                                        ]
                                    ]
                                    [ button
                                        [ classes
                                            [ "border-l-2"
                                            , "border-y-2"
                                            , "rounded-l-lg"
                                            , "bg-gray-200"
                                            , "p-5"
                                            , "hover:bg-gray-400"
                                            , "transition-colors"
                                            ]
                                        , onClick SearchForCollections
                                        ]
                                        [ svg
                                            [ SA.class "h-6 w-6"
                                            , SA.stroke "currentColor"
                                            , SA.fill "none"
                                            , SA.strokeWidth "2"
                                            , SA.viewBox "0 0 24 24"
                                            ]
                                            [ Icons.search ]
                                        ]
                                    , div
                                        [ classes
                                            [ "flex"
                                            , "space-x-3"
                                            , "border-2"
                                            , "rounded-r-lg"
                                            , "p-3"
                                            , "w-full"
                                            ]
                                        ]
                                      <|
                                        List.map
                                            (\u ->
                                                button
                                                    [ classes
                                                        [ "bg-gray-200"
                                                        , "p-2"
                                                        , "rounded-lg"
                                                        , "hover:line-through"
                                                        , "transition-colors"
                                                        , "hover:bg-gray-300"
                                                        , "flex"
                                                        , "space-x-1"
                                                        , "items-center"
                                                        ]
                                                    , onClick (RemoveUsername u)
                                                    ]
                                                    (Maybe.values
                                                        [ Just (span [] [ text u ])
                                                        , model.results
                                                            |> Dict.get u
                                                            |> Maybe.andThen
                                                                (\r ->
                                                                    if RemoteData.isLoading r then
                                                                        Just (Icons.spinner [ SA.class "h-4", SA.class "w-4" ])

                                                                    else
                                                                        Nothing
                                                                )
                                                        ]
                                                    )
                                            )
                                            model.usernamesToSearch
                                    ]
                                )

                          else
                            Nothing
                        , Just
                            (div
                                [ classes
                                    [ "border-2"
                                    , "flex"
                                    , "rounded-lg"
                                    , "items-start"
                                    , "flex-col"
                                    , "flex-grow"
                                    ]
                                ]
                                [ div
                                    [ classes
                                        [ "relative"
                                        ]
                                    ]
                                    [ viewBoardGames model
                                    , if hasResults model then
                                        filters model

                                      else
                                        span [] []
                                    ]
                                ]
                            )
                        ]
                    )
                ]
            ]
        , case model.selectedBoardgame of
            Just ( u, bg ) ->
                div
                    [ classes
                        [ "fixed"
                        , "bottom-0"
                        , "left-0"
                        , "w-full"
                        , "h-36"
                        ]
                    ]
                    [ overlay u bg ]

            Nothing ->
                div [] []
        ]


filters : Model -> Html Msg
filters model =
    div
        [ classes
            [ "fixed"
            , "top-0"
            , "right-12"
            , "h-full"
            ]
        ]
        [ div
            [ classes
                [ "sticky"
                , "top-0"
                , "right-12"
                ]
            ]
            (Maybe.values
                [ Just <|
                    button
                        [ classes
                            [ "rounded-full"
                            , "bg-white"
                            , "border-2"
                            ]
                        , onClick ToggleFilters
                        ]
                        [ svg
                            [ SA.class "h-8 w-8"
                            , SA.stroke "currentColor"
                            , SA.fill "none"
                            , SA.strokeWidth "2"
                            , SA.viewBox "0 0 24 24"
                            ]
                            [ Icons.filters ]
                        ]
                , if model.filtersVisible then
                    Just <|
                        div
                            [ classes
                                [ "flex"
                                , "flex-col"
                                , "bg-white"
                                , "p-4"
                                , "absolute"
                                , "right-0"
                                , "rounded-lg"
                                , "border-2"
                                , "w-96"
                                ]
                            ]
                            [ div [ classes [ "flex", "flex-col" ] ]
                                [ div [] [ text "Players" ]
                                , div []
                                    [ SingleSlider.view model.playerCountSlider
                                    ]
                                ]
                            , div []
                                [ div [] [ text "Play Time" ]
                                , div []
                                    [ DoubleSlider.view model.playtimeSlider
                                    ]
                                ]
                            , div []
                                [ div [] [ text "Rating" ]
                                , div []
                                    [ DoubleSlider.view model.ratingSlider
                                    ]
                                ]
                            ]

                  else
                    Nothing
                ]
            )
        ]
