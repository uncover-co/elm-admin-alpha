module Admin.UI.Invalid exposing (view)

import Admin.Internal.InvalidRouteData exposing (InvalidRouteData)
import Html exposing (..)
import Html.Attributes exposing (..)
import ThemeSpec


view : List String -> List InvalidRouteData -> Html msg
view duplicatedRoutes invalidRoutes =
    div [ style "color" ThemeSpec.base.fg ]
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
                , ul []
                    (invalidRoutes
                        |> List.map
                            (\{ path, page } ->
                                li [] [ text <| page ++ ": " ++ path ]
                            )
                    )
                ]

          else
            text ""
        ]
