module ElmAdmin.UI.Invalid exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import ThemeSpec


view : List String -> List ( String, String ) -> Html msg
view duplicatedRoutes invalidRoutes =
    div [ style "color" ThemeSpec.color ]
        [ if not (List.isEmpty duplicatedRoutes) then
            div []
                [ p [] [ text "Duplicated routes:" ]
                , ul [] (duplicatedRoutes |> List.map (\r -> li [] [ text r ]))
                ]

          else
            text ""
        , if not (List.isEmpty invalidRoutes) then
            div []
                [ p [] [ text "Invalid path parameters:" ]
                , ul [] (invalidRoutes |> List.map (\( t, p ) -> li [] [ text <| t ++ " " ++ p ]))
                ]

          else
            text ""
        ]
