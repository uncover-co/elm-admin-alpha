module UI.Nav exposing
    ( UINavItem(..)
    , view
    )

import ElmAdmin.RouteParams exposing (RouteParams)
import ElmAdmin.Router exposing (applyParams)
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


viewItemVisualGroup : Maybe String -> RouteParams -> model -> ItemDataVisual model -> List (UINavItem model) -> Html msg
viewItemVisualGroup activePath routeParams model item items =
    viewVisible routeParams
        model
        item
        (\_ ->
            li
                [ class "eadm eadm-nav-item m-visual-group m-group" ]
                [ p [] [ text (item.title routeParams model) ]
                , if not (List.isEmpty items) then
                    viewList activePath routeParams model items

                  else
                    text ""
                ]
        )


viewItemGroup : Maybe String -> RouteParams -> model -> ItemData model -> List (UINavItem model) -> Html msg
viewItemGroup activePath routeParams model item items =
    viewVisibleWithParams routeParams
        model
        item
        (\href_ ->
            li
                [ class "eadm eadm-nav-item m-group"
                , classList [ ( "m-active", activePath == Just item.path ) ]
                ]
                [ a [ class "eadm eadm-nav-item-link", href href_ ]
                    [ text (item.title routeParams model) ]
                , viewList activePath routeParams model items
                ]
        )


viewItem : Maybe String -> RouteParams -> model -> ItemData model -> Html msg
viewItem activePath routeParams model item =
    viewVisibleWithParams routeParams
        model
        item
        (\href_ ->
            li
                [ class "eadm eadm-nav-item"
                , classList [ ( "m-active", activePath == Just item.path ) ]
                ]
                [ a [ class "eadm eadm-nav-item-link", href href_ ]
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
    viewVisible routeParams
        model
        item
        (\_ ->
            case applyParams item.path item.pathParams routeParams of
                Just href_ ->
                    render href_

                Nothing ->
                    text ""
        )


viewList : Maybe String -> RouteParams -> model -> List (UINavItem model) -> Html msg
viewList activePath routeParams model items =
    ul [ class "eadm eadm-nav-group" ]
        (items
            |> List.map
                (\item_ ->
                    case item_ of
                        External title href_ ->
                            li [ class "eadm eadm-nav-item" ]
                                [ a
                                    [ class "eadm eadm-nav-item-link"
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


view : Maybe String -> RouteParams -> model -> List (UINavItem model) -> Html msg
view activePath routeParams model items =
    nav [ class "eadm eadm-nav" ]
        [ viewList activePath routeParams model items
        ]
