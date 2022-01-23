module ElmAdmin.UI.List exposing (view)

import ElmAdmin.Shared exposing (Msg(..))
import ElmAdmin.UI.Loading
import Html exposing (..)
import Html.Attributes exposing (..)


view :
    { title : Html msg
    , items : Maybe (List { label : Html msg, actions : Html msg })
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
                                        [ div
                                            [ class "eadm eadm-list-item-label" ]
                                            [ item.label ]
                                        , div
                                            [ class "eadm eadm-list-item-actions" ]
                                            [ item.actions ]
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
                    [ ElmAdmin.UI.Loading.view ]
        ]
        |> Html.map GotMsg
