module ElmAdmin.UI.List exposing (view)

import Admin.Shared exposing (Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import W.DataRow
import W.Loading


view :
    { title : Html msg
    , items :
        Maybe
            (List
                { label : Html msg
                , actions : List (Html msg)
                , options : List (W.DataRow.Attribute msg)
                }
            )
    }
    -> Html (Msg msg)
view props =
    section [ class "eadm eadm-card eadm-list" ]
        [ h1 [ class "eadm eadm-list-title" ] [ props.title ]
        , case props.items of
            Just items ->
                if not (List.isEmpty items) then
                    ul [ class "eadm eadm-list-list" ]
                        (items
                            |> List.map
                                (\item ->
                                    li [ class "eadm eadm-list-item" ]
                                        [ W.DataRow.view item.options [ item.label ]
                                        ]
                                )
                        )

                else
                    div
                        [ class "eadm eadm-list-empty" ]
                        [ text "∅" ]

            Nothing ->
                div
                    [ class "eadm eadm-list-empty" ]
                    [ W.Loading.circles [ W.Loading.size 32 ] ]
        ]
        |> Html.map GotMsg
