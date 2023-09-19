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
        , a
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
        , disabled
        , href
        , placeholder
        , src
        , target
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
import Random
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


type alias OwnedBoardGame =
    ( List String, BoardGame )


type alias Model =
    { currentUsername : String
    , usernamesToSearch : List String
    , results :
        Dict String (WebData (List BoardGame))
    , playerCountSlider : SingleSlider.SingleSlider Msg
    , playtimeSlider : DoubleSlider.DoubleSlider Msg
    , ratingSlider : DoubleSlider.DoubleSlider Msg
    , selectedBoardGame : Maybe OwnedBoardGame
    , randomlyChosenGame : Maybe OwnedBoardGame
    , filtersVisible : Bool
    }


hasResults : { r | results : Dict String (WebData (List BoardGame)) } -> Bool
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
      , selectedBoardGame = Nothing
      , randomlyChosenGame = Nothing
      }
    , Cmd.none
    )


type Msg
    = CurrentUsernameChanged String
    | AddUsernameToSearch
    | RemoveUsername String
    | SearchForCollections
    | GotBoardGames String (Result Http.Error (List BoardGame))
    | PlayerCountFilterValueChanged Float
    | PlaytimeFilterLowValueChanged Float
    | PlaytimeFilterHighValueChanged Float
    | RatingFilterLowValueChanged Float
    | RatingFilterHighValueChanged Float
    | ToggleFilters
    | SelectBoardGame OwnedBoardGame
    | UnSelectBoardGame
    | RandomlySelectGame
    | BoardGameRandomlySelected OwnedBoardGame
    | UnSelectRandomGame


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
                , results = Dict.remove u model.results
                , selectedBoardGame =
                    case model.selectedBoardGame of
                        Just ( us, _ ) ->
                            if List.any (\x -> x == u) us then
                                Nothing

                            else
                                model.selectedBoardGame

                        Nothing ->
                            model.selectedBoardGame
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
                ( { model
                    | results = Dict.union initDict model.results
                    , selectedBoardGame = Nothing
                  }
                , model.usernamesToSearch
                    |> List.map (\u -> Api.getBoardGames (GotBoardGames u) u)
                    |> Cmd.batch
                )

            else
                ( model
                , Cmd.none
                )

        GotBoardGames u res ->
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

        SelectBoardGame bg ->
            ( { model | selectedBoardGame = Just bg }, Cmd.none )

        UnSelectBoardGame ->
            ( { model | selectedBoardGame = Nothing }, Cmd.none )

        RandomlySelectGame ->
            ( model
            , case filteredGames model of
                [] ->
                    Cmd.none

                x :: xs ->
                    Random.generate BoardGameRandomlySelected (Random.uniform x xs)
            )

        BoardGameRandomlySelected bg ->
            ( { model
                | randomlyChosenGame = Just bg
                , filtersVisible = False
              }
            , Cmd.none
            )

        UnSelectRandomGame ->
            ( { model | randomlyChosenGame = Nothing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


selectedBoardGameOverlay : List String -> BoardGame -> Html Msg
selectedBoardGameOverlay u bg =
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
            [ a
                [ href ("https://boardgamegeek.com/boardgame/" ++ bg.bggId)
                , target "_blank"
                , classes [ "underline" ]
                ]
                [ item Icons.pencil (bg.title ++ " (" ++ String.fromInt bg.yearPublished ++ ")") ]
            , item Icons.star (R.round 1 bg.rating)
            , item Icons.users (R.round 0 bg.minPlayers ++ " - " ++ R.round 0 bg.maxPlayers)
            , item Icons.clock
                (if bg.minPlaytime == bg.maxPlaytime then
                    bg.minPlaytime
                        |> Maybe.map (R.round 0)
                        |> Maybe.withDefault "N/A"

                 else
                    Maybe.withDefault "N/A" (Maybe.map (R.round 0) bg.minPlaytime) ++ " - " ++ Maybe.withDefault "N/A" (Maybe.map (R.round 0) bg.maxPlaytime)
                )
            , item Icons.user (String.join ", " u)
            ]
        ]


viewBoardGame : List String -> BoardGame -> Html Msg
viewBoardGame u bg =
    div
        [ classes
            [ "flex-grow"
            , "h-24"
            , "transition-all"
            , "hover:scale-150"
            , "duration-200"
            , "group"
            ]
        , onMouseEnter (SelectBoardGame ( u, bg ))
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


isBoardGameApplicable : Model -> BoardGame -> Bool
isBoardGameApplicable model bg =
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
        && Maybe.withDefault 0 bg.minPlaytime
        >= minPlaytime
        && Maybe.withDefault 0 bg.maxPlaytime
        <= maxPlaytime
        && bg.rating
        >= minRating
        && bg.rating
        <= maxRating


allBoardGames : { r | results : Dict String (WebData (List BoardGame)) } -> List ( List String, BoardGame )
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


filteredGames model =
    model
        |> allBoardGames
        |> List.sortBy (.title << Tuple.second)
        |> List.filter (isBoardGameApplicable model << Tuple.second)


viewBoardGames : Model -> Html Msg
viewBoardGames model =
    let
        boardgames =
            model
                |> filteredGames
                |> List.map (uncurry viewBoardGame)
                |> flip List.append
                    [ div
                        [ classes
                            [ "flex-grow-[200]"
                            , "h-24"
                            ]
                        ]
                        []
                    ]
    in
    if List.length boardgames == 1 then
        div [ classes [ "p-3" ] ] [ text "No results." ]

    else
        div [ classes [ "p-3" ] ]
            [ div []
                [ div
                    [ classes
                        [ "flex"
                        , "flex-wrap"
                        ]
                    ]
                    boardgames
                ]
            ]


view : Model -> Html Msg
view model =
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
                    [ SA.class "h-8 w-8"
                    , SA.stroke "currentColor"
                    , SA.strokeWidth "2"
                    , SA.viewBox "0 0 24 24"
                    , SA.fill "none"
                    ]
                    [ icon ]
                , span [ classes [ "text-left", "text-xl" ] ]
                    [ text txt ]
                ]
    in
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
                [ "fixed"
                , "w-full"
                , "h-full"
                , "transform-gpu"
                , "scale-y-0"
                , "bg-white/75"
                , "z-20"
                , "transition-all"
                , "origin-top"
                , "border-2"
                , "p-6"
                ]
            , classList
                [ ( "scale-y-100", Maybe.isJust model.randomlyChosenGame )
                , ( "scale-y-0", Maybe.isNothing model.randomlyChosenGame )
                ]
            ]
            [ case model.randomlyChosenGame of
                Just ( u, bg ) ->
                    div
                        [ classes
                            [ "flex"
                            , "justify-between"
                            , "h-full"
                            ]
                        ]
                        [ div
                            [ classes
                                [ "flex"
                                , "space-x-5"
                                ]
                            ]
                            [ img
                                [ classes
                                    [ "max-h-full"
                                    ]
                                , src bg.fullSizeUrl
                                ]
                                []
                            , div []
                                [ item Icons.pencil (bg.title ++ " (" ++ String.fromInt bg.yearPublished ++ ")")
                                , item Icons.star (R.round 1 bg.rating)
                                , item Icons.users (R.round 0 bg.minPlayers ++ " - " ++ R.round 0 bg.maxPlayers)
                                , item Icons.clock
                                    (if bg.minPlaytime == bg.maxPlaytime then
                                        bg.minPlaytime
                                            |> Maybe.map (R.round 0)
                                            |> Maybe.withDefault "N/A"

                                     else
                                        Maybe.withDefault "N/A" (Maybe.map (R.round 0) bg.minPlaytime) ++ " - " ++ Maybe.withDefault "N/A" (Maybe.map (R.round 0) bg.maxPlaytime)
                                    )
                                , item Icons.user (String.join ", " u)
                                ]
                            ]
                        , div []
                            [ button [ onClick UnSelectRandomGame ]
                                [ svg
                                    [ SA.class "h-6 w-6"
                                    , SA.stroke "currentColor"
                                    , SA.strokeWidth "2"
                                    , SA.viewBox "0 0 24 24"
                                    ]
                                    [ Icons.x ]
                                ]
                            ]
                        ]

                Nothing ->
                    div [] []
            ]
        , div
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
                    [ form
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
                    , div
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
                                , "transition-colors"
                                , "h-16"
                                ]
                            , classList [ ( "hover:bg-gray-400", not (List.isEmpty model.usernamesToSearch) ) ]
                            , disabled (List.isEmpty model.usernamesToSearch)
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
                                , "h-16"
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
                                                        case r of
                                                            Loading ->
                                                                Just
                                                                    (Icons.spinner
                                                                        [ SA.class "h-6"
                                                                        , SA.class "w-6"
                                                                        ]
                                                                    )

                                                            Success _ ->
                                                                Just
                                                                    (svg
                                                                        [ SA.class "h-6 w-6"
                                                                        , SA.stroke "green"
                                                                        , SA.strokeWidth "2"
                                                                        , SA.viewBox "0 0 24 24"
                                                                        , SA.fill "none"
                                                                        ]
                                                                        [ Icons.check ]
                                                                    )

                                                            Failure _ ->
                                                                Just
                                                                    (svg
                                                                        [ SA.class "h-6 w-6"
                                                                        , SA.stroke "red"
                                                                        , SA.strokeWidth "2"
                                                                        , SA.viewBox "0 0 24 24"
                                                                        , SA.fill "none"
                                                                        ]
                                                                        [ Icons.x ]
                                                                    )

                                                            _ ->
                                                                Nothing
                                                    )
                                            ]
                                        )
                                )
                                model.usernamesToSearch
                        ]
                    , div
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
                    ]
                ]
            ]
        , case model.selectedBoardGame of
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
                    [ selectedBoardGameOverlay u bg ]

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
            , "translate-y-[50%]"
            , "z-10"
            ]
        ]
        [ div
            [ classes
                []
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
                            [ div
                                [ classes
                                    [ "flex"
                                    , "flex-col"
                                    ]
                                ]
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
                            , div
                                [ classes
                                    [ "flex"
                                    , "justify-center"
                                    ]
                                ]
                                [ button
                                    [ onClick RandomlySelectGame
                                    , classes
                                        [ "hover:text-white"
                                        , "bg-green-300"
                                        , "p-3"
                                        , "px-6"
                                        , "rounded-lg"
                                        , "hover:bg-green-500"
                                        ]
                                    ]
                                    [ text "Pick One!" ]
                                ]
                            ]

                  else
                    Nothing
                ]
            )
        ]
