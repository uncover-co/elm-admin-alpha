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
    }


type UINavItem model
    = External String String
    | Single (ItemData model)
    | Group
        { main : ItemData model
        , items : List (UINavItem model)
        }


viewItem : Maybe String -> RouteParams -> model -> ItemData model -> List (UINavItem model) -> Html msg
viewItem activePath routeParams model item items =
    if not (item.hidden routeParams model) then
        case applyParams item.path item.pathParams routeParams of
            Just href_ ->
                li
                    [ class "eadm eadm-nav-item"
                    , classList
                        [ ( "m-active", activePath == Just item.path )
                        , ( "m-group", not (List.isEmpty items) )
                        ]
                    ]
                    [ a [ class "eadm eadm-nav-item-wrapper", href href_ ]
                        [ text (item.title routeParams model) ]
                    , if not (List.isEmpty items) then
                        viewList activePath routeParams model items

                      else
                        text ""
                    ]

            Nothing ->
                text ""

    else
        text ""


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
                                    [ class "eadm eadm-nav-item-wrapper"
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
                                []

                        Group group ->
                            viewItem
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
