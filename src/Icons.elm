module Icons exposing (..)

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as SA


plus : Svg msg
plus =
    Svg.path
        [ SA.strokeLinecap "round"
        , SA.strokeLinejoin "round"
        , SA.d "M12 4v16m8-8H4"
        ]
        []


search : Svg msg
search =
    Svg.path
        [ SA.strokeLinecap "round"
        , SA.strokeLinejoin "round"
        , SA.d "M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"
        ]
        []


x : Svg msg
x =
    Svg.path
        [ SA.strokeLinecap "round"
        , SA.strokeLinejoin "round"
        , SA.d "M6 18L18 6M6 6l12 12"
        ]
        []


spinner : List (Svg.Attribute msg) -> Html msg
spinner attrs =
    Svg.svg
        ([ SA.class "mr-2 w-8 h-8 text-gray-200 animate-spin dark:text-gray-600 fill-blue-600"
         , SA.viewBox "0 0 100 101"
         , SA.fill "none"
         ]
            ++ attrs
        )
        [ Svg.path [ SA.d "M100 50.5908C100 78.2051 77.6142 100.591 50 100.591C22.3858 100.591 0 78.2051 0 50.5908C0 22.9766 22.3858 0.59082 50 0.59082C77.6142 0.59082 100 22.9766 100 50.5908ZM9.08144 50.5908C9.08144 73.1895 27.4013 91.5094 50 91.5094C72.5987 91.5094 90.9186 73.1895 90.9186 50.5908C90.9186 27.9921 72.5987 9.67226 50 9.67226C27.4013 9.67226 9.08144 27.9921 9.08144 50.5908Z", SA.fill "currentColor" ]
            []
        , Svg.path [ SA.d "M93.9676 39.0409C96.393 38.4038 97.8624 35.9116 97.0079 33.5539C95.2932 28.8227 92.871 24.3692 89.8167 20.348C85.8452 15.1192 80.8826 10.7238 75.2124 7.41289C69.5422 4.10194 63.2754 1.94025 56.7698 1.05124C51.7666 0.367541 46.6976 0.446843 41.7345 1.27873C39.2613 1.69328 37.813 4.19778 38.4501 6.62326C39.0873 9.04874 41.5694 10.4717 44.0505 10.1071C47.8511 9.54855 51.7191 9.52689 55.5402 10.0491C60.8642 10.7766 65.9928 12.5457 70.6331 15.2552C75.2735 17.9648 79.3347 21.5619 82.5849 25.841C84.9175 28.9121 86.7997 32.2913 88.1811 35.8758C89.083 38.2158 91.5421 39.6781 93.9676 39.0409Z", SA.fill "currentFill" ]
            []
        ]


filters : Svg msg
filters =
    Svg.path
        [ SA.strokeLinecap "round"
        , SA.strokeLinejoin "round"
        , SA.d "M12 6V4m0 2a2 2 0 100 4m0-4a2 2 0 110 4m-6 8a2 2 0 100-4m0 4a2 2 0 110-4m0 4v2m0-6V4m6 6v10m6-2a2 2 0 100-4m0 4a2 2 0 110-4m0 4v2m0-6V4"
        ]
        []


users : Svg msg
users =
    Svg.path
        [ SA.strokeLinecap "round"
        , SA.strokeLinejoin "round"
        , SA.d "M12 4.354a4 4 0 110 5.292M15 21H3v-1a6 6 0 0112 0v1zm0 0h6v-1a6 6 0 00-9-5.197M13 7a4 4 0 11-8 0 4 4 0 018 0z"
        ]
        []


clock : Svg msg
clock =
    Svg.path
        [ SA.strokeLinecap "round"
        , SA.strokeLinejoin "round"
        , SA.d "M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z"
        ]
        []


star : Svg msg
star =
    Svg.path
        [ SA.strokeLinecap "round"
        , SA.strokeLinejoin "round"
        , SA.d "M11.049 2.927c.3-.921 1.603-.921 1.902 0l1.519 4.674a1 1 0 00.95.69h4.915c.969 0 1.371 1.24.588 1.81l-3.976 2.888a1 1 0 00-.363 1.118l1.518 4.674c.3.922-.755 1.688-1.538 1.118l-3.976-2.888a1 1 0 00-1.176 0l-3.976 2.888c-.783.57-1.838-.197-1.538-1.118l1.518-4.674a1 1 0 00-.363-1.118l-3.976-2.888c-.784-.57-.38-1.81.588-1.81h4.914a1 1 0 00.951-.69l1.519-4.674z"
        ]
        []


user : Svg msg
user =
    Svg.path
        [ SA.strokeLinecap "round"
        , SA.strokeLinejoin "round"
        , SA.d "M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z"
        ]
        []


pencil : Svg msg
pencil =
    Svg.path
        [ SA.strokeLinecap "round"
        , SA.strokeLinejoin "round"
        , SA.d "M15.232 5.232l3.536 3.536m-2.036-5.036a2.5 2.5 0 113.536 3.536L6.5 21.036H3v-3.572L16.732 3.732z"
        ]
        []
