module UI.Nav exposing
    ( UINavItem(..)
    , view
    )

import Dict exposing (Dict)
import ElmAdmin.RouteParams exposing (RouteParams)
import ElmAdmin.Router exposing (applyParams)
import Html exposing (..)
import Html.Attributes exposing (..)


type alias ItemData model =
    { title : String -> RouteParams -> model -> String
    , path : String
    , pathParams : List String
    , hidden : String -> RouteParams -> model -> Bool
    , disabled : String -> RouteParams -> model -> Bool
    }


type alias ItemDataVisual model =
    { title : String -> RouteParams -> model -> String
    , hidden : String -> RouteParams -> model -> Bool
    , disabled : String -> RouteParams -> model -> Bool
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


viewItemVisualGroup : String -> RouteParams -> model -> ItemDataVisual model -> List (UINavItem model) -> Html msg
viewItemVisualGroup activePath routeParams model item items =
    viewVisible activePath
        routeParams
        model
        item
        (\_ ->
            li
                [ class "eadm eadm-nav-list-item" ]
                [ case item.title activePath routeParams model of
                    "" ->
                        text ""

                    title ->
                        p
                            [ class "eadm eadm-nav-item eadm-m-visual-group eadm-m-group" ]
                            [ text title ]
                , if not (List.isEmpty items) then
                    viewList activePath routeParams model items

                  else
                    text ""
                ]
        )


viewItemGroup : String -> RouteParams -> model -> ItemData model -> List (UINavItem model) -> Html msg
viewItemGroup activePath routeParams model item items =
    viewVisibleWithParams activePath
        routeParams
        model
        item
        (\href_ ->
            li
                [ class "eadm eadm-nav-list-item" ]
                [ a
                    [ href href_
                    , class "eadm eadm-nav-item eadm-m-group"
                    , classList [ ( "eadm-m-active", activePath == item.path ) ]
                    ]
                    [ text (item.title activePath routeParams model) ]
                , viewList activePath routeParams model items
                ]
        )


viewItem : String -> RouteParams -> model -> ItemData model -> Html msg
viewItem activePath routeParams model item =
    viewVisibleWithParams activePath
        routeParams
        model
        item
        (\href_ ->
            li
                [ class "eadm eadm-nav-list-item" ]
                [ a
                    [ href href_
                    , class "eadm eadm-nav-item"
                    , classList [ ( "eadm-m-active", activePath == item.path ) ]
                    ]
                    [ text (item.title activePath routeParams model) ]
                ]
        )


viewVisible :
    String
    -> RouteParams
    -> model
    ->
        { m
            | hidden : String -> RouteParams -> model -> Bool
            , disabled : String -> RouteParams -> model -> Bool
        }
    -> (() -> Html msg)
    -> Html msg
viewVisible activePath routeParams model item render =
    if not (item.hidden activePath routeParams model) && not (item.disabled activePath routeParams model) then
        render ()

    else
        text ""


viewVisibleWithParams :
    String
    -> RouteParams
    -> model
    -> ItemData model
    -> (String -> Html msg)
    -> Html msg
viewVisibleWithParams activePath routeParams model item render =
    viewVisible activePath
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


viewList : String -> RouteParams -> model -> List (UINavItem model) -> Html msg
viewList activePath routeParams model items =
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
                                activePath
                                routeParams
                                model
                                item

                        Group group ->
                            viewItemGroup
                                activePath
                                routeParams
                                model
                                group.main
                                group.items

                        VisualGroup group ->
                            viewItemVisualGroup
                                activePath
                                routeParams
                                model
                                group.main
                                group.items
                )
        )


view : String -> RouteParams -> model -> List (UINavItem model) -> Html msg
view activePath routeParams model items =
    nav [ class "eadm eadm-nav" ]
        [ viewList activePath routeParams model items
        ]
