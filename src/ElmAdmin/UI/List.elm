module ElmAdmin.UI.List exposing (view)

import ElmAdmin.Shared exposing (Msg(..))
import ElmWidgets as W
import ElmWidgets.Attributes as WA
import Html exposing (..)
import Html.Attributes exposing (..)


view :
    { title : Html msg
    , items :
        Maybe
            (List
                { label : Html msg
                , actions : List (Html msg)
                , options : List (W.DataRowAttributes msg -> W.DataRowAttributes msg)
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
                                        [ W.dataRow item.options
                                            { label = item.label
                                            , actions = item.actions
                                            }
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
                    [ W.loadingCircle [ WA.size 32 ] ]
        ]
        |> Html.map GotMsg
