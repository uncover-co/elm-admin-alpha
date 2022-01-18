module ElmAdmin.UI.Nav exposing
    ( UINavItem(..)
    , view
    )

import ElmAdmin.Router exposing (RouteParams, applyParams)
import Html exposing (..)
import Html.Attributes exposing (..)


type alias ItemData model =
    { title : RouteParams -> model -> String
    , path : String
    , pathParams : List String
    , hidden : RouteParams -> model -> Bool
    , disabled : RouteParams -> model -> Bool
    }


type alias ItemDataVisual model =
    { title : RouteParams -> model -> String
    , hidden : RouteParams -> model -> Bool
    , disabled : RouteParams -> model -> Bool
    }


type UINavItem model
    = External String String
    | Single (ItemData model)
    | Group
        { main : ItemData model
        , items : List (UINavItem model)
        }
    | VisualGroup
        { main : ItemDataVisual model
        , items : List (UINavItem model)
        }


viewItemVisualGroup : RouteParams -> model -> ItemDataVisual model -> List (UINavItem model) -> Html msg
viewItemVisualGroup routeParams model item items =
    viewVisible
        routeParams
        model
        item
        (\_ ->
            li
                [ class "eadm eadm-nav-list-item" ]
                [ case item.title routeParams model of
                    "" ->
                        text ""

                    title ->
                        p
                            [ class "eadm eadm-nav-item eadm-m-visual-group eadm-m-group" ]
                            [ text title ]
                , if not (List.isEmpty items) then
                    viewList routeParams model items

                  else
                    text ""
                ]
        )


viewItemGroup : RouteParams -> model -> ItemData model -> List (UINavItem model) -> Html msg
viewItemGroup routeParams model item items =
    viewVisibleWithParams
        routeParams
        model
        item
        (\href_ ->
            li
                [ class "eadm eadm-nav-list-item" ]
                [ a
                    [ href href_
                    , class "eadm eadm-nav-item eadm-m-group"
                    , classList [ ( "eadm-m-active", routeParams.path == item.path ) ]
                    ]
                    [ text (item.title routeParams model) ]
                , viewList routeParams model items
                ]
        )


viewItem : RouteParams -> model -> ItemData model -> Html msg
viewItem routeParams model item =
    viewVisibleWithParams
        routeParams
        model
        item
        (\href_ ->
            li
                [ class "eadm eadm-nav-list-item" ]
                [ a
                    [ href href_
                    , class "eadm eadm-nav-item"
                    , classList [ ( "eadm-m-active", routeParams.path == item.path ) ]
                    ]
                    [ text (item.title routeParams model) ]
                ]
        )


viewVisible :
    RouteParams
    -> model
    ->
        { m
            | hidden : RouteParams -> model -> Bool
            , disabled : RouteParams -> model -> Bool
        }
    -> (() -> Html msg)
    -> Html msg
viewVisible routeParams model item render =
    if not (item.hidden routeParams model) && not (item.disabled routeParams model) then
        render ()

    else
        text ""


viewVisibleWithParams :
    RouteParams
    -> model
    -> ItemData model
    -> (String -> Html msg)
    -> Html msg
viewVisibleWithParams routeParams model item render =
    viewVisible
        routeParams
        model
        item
        (\_ ->
            case applyParams item.path item.pathParams routeParams of
                Just href_ ->
                    render href_

                Nothing ->
                    text ""
        )


viewList : RouteParams -> model -> List (UINavItem model) -> Html msg
viewList routeParams model items =
    ul [ class "eadm eadm-nav-list" ]
        (items
            |> List.map
                (\item_ ->
                    case item_ of
                        External title href_ ->
                            li [ class "eadm eadm-nav-list-item" ]
                                [ a
                                    [ class "eadm eadm-nav-item"
                                    , href href_
                                    , target "_blank"
                                    ]
                                    [ text title ]
                                ]

                        Single item ->
                            viewItem
                                routeParams
                                model
                                item

                        Group group ->
                            viewItemGroup
                                routeParams
                                model
                                group.main
                                group.items

                        VisualGroup group ->
                            viewItemVisualGroup
                                routeParams
                                model
                                group.main
                                group.items
                )
        )


view : RouteParams -> model -> List (UINavItem model) -> Html msg
view routeParams model items =
    nav [ class "eadm eadm-nav" ]
        [ viewList routeParams model items
        ]
