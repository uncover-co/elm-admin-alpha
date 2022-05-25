module Admin.UI.Nav exposing (view)

import Admin.Internal.NavItem
import Html exposing (..)
import Html.Attributes exposing (..)


viewItemGroup :
    String
    ->
        { path : String
        , title : String
        , full : Bool
        , children : List Admin.Internal.NavItem.NavItem
        }
    -> Html msg
viewItemGroup activePath item =
    li
        [ class "eadm eadm-nav-list-item"
        , classList
            [ ( "eadm-m-full"
              , String.startsWith item.path activePath && item.full
              )
            ]
        ]
        [ a
            [ href item.path
            , class "eadm eadm-nav-item eadm-m-group"
            , classList [ ( "eadm-m-active", activePath == item.path ) ]
            ]
            [ text item.title ]
        , viewList activePath item.children
        ]


viewList :
    String
    -> List Admin.Internal.NavItem.NavItem
    -> Html msg
viewList activePath items =
    ul [ class "eadm eadm-nav-list" ]
        (items
            |> List.map
                (\item_ ->
                    case item_ of
                        Admin.Internal.NavItem.NavItemExternal { url, label } ->
                            li [ class "eadm eadm-nav-list-item" ]
                                [ a
                                    [ class "eadm eadm-nav-item"
                                    , href url
                                    , target "_blank"
                                    ]
                                    [ text label ]
                                ]

                        Admin.Internal.NavItem.NavItemInternal i ->
                            viewItemGroup activePath i
                )
        )


view :
    { active : String
    , items : List Admin.Internal.NavItem.NavItem
    }
    -> Html msg
view props =
    nav [ class "eadm eadm-nav" ]
        [ viewList props.active props.items
        ]
