module ElmAdmin exposing
    ( ElmAdmin
    , Page
    , RouteParams
    , admin
    , external
    , format
    , group
    , hidden
    , page
    , resources
    , single
    )

import Browser
import Browser.Navigation exposing (..)
import Dict exposing (Dict)
import ElmAdmin.Model exposing (Model, Msg(..), subscriptions, update, view)
import ElmAdmin.Router exposing (parsePathParams, pathFromString, pathToString)
import Html exposing (..)
import Html.Attributes exposing (..)
import UI.Nav



-- Main


type alias ElmAdmin flags model msg =
    Program flags (Model model) (Msg msg)


type Page model msg
    = Page (ElmAdmin.Model.Page model msg)


type alias RouteParams =
    { pathParams : Dict String String
    , queryParams : Dict String (List String)
    }



-- Navigation


type NavigationItem model msg
    = External String String
    | Hidden (ElmAdmin.Model.Page model msg)
    | Single (NavItemData model msg)
    | Group
        { main : NavItemData model msg
        , items : List (NavigationItem model msg)
        }


type alias NavItemData model msg =
    { page : ElmAdmin.Model.Page model msg
    , pathParams : List String
    , hidden : RouteParams -> model -> Bool
    , title : RouteParams -> model -> String
    }


external : String -> String -> NavigationItem model msg
external =
    External


hidden : Page model msg -> NavigationItem model msg
hidden (Page p) =
    Hidden p


single : String -> Page model msg -> NavigationItem model msg
single title (Page p) =
    Single
        { page = p
        , pathParams = ElmAdmin.Router.parsePathParams p.path
        , hidden = \_ _ -> False
        , title = \_ _ -> title
        }


group :
    String
    ->
        { main : Page model msg
        , items : List (NavigationItem model msg)
        }
    -> NavigationItem model msg
group title { main, items } =
    case main of
        Page p ->
            Group
                { main =
                    { page = p
                    , pathParams = ElmAdmin.Router.parsePathParams p.path
                    , hidden = \_ _ -> False
                    , title = \_ _ -> title
                    }
                , items = items
                }


resources :
    String
    ->
        { index : Page model msg
        , show : Page model msg
        , create : Page model msg
        , update : Page model msg
        }
    -> NavigationItem model msg
resources title props =
    group title
        { main = props.index
        , items =
            [ hidden props.show
            , hidden props.update
            , single "Create" props.create
            ]
        }



-- Navigation Modifiers


hiddenIf : (RouteParams -> model -> Bool) -> NavigationItem model msg -> NavigationItem model msg
hiddenIf fn a =
    case a of
        External _ _ ->
            a

        Hidden _ ->
            a

        Single i ->
            Single { i | hidden = fn }

        Group { main, items } ->
            Group { main = { main | hidden = fn }, items = items }


format : (RouteParams -> model -> String) -> NavigationItem model msg -> NavigationItem model msg
format fn a =
    case a of
        External _ _ ->
            a

        Hidden _ ->
            a

        Single i ->
            Single { i | title = fn }

        Group { main, items } ->
            Group
                { main = { main | title = fn }
                , items = items
                }



-- Pages


page :
    { path : String
    , toResource : RouteParams -> model -> resource
    , init : RouteParams -> model -> resource -> ( model, Cmd msg )
    , update : msg -> RouteParams -> model -> resource -> ( model, Cmd msg )
    , subscriptions : RouteParams -> model -> resource -> Sub msg
    , view : RouteParams -> model -> resource -> Html msg
    }
    -> Page model msg
page props =
    Page
        { path =
            pathFromString props.path
        , init =
            \params model ->
                props.init params model (props.toResource params model)
        , update =
            \msg params model ->
                props.update msg params model (props.toResource params model)
        , subscriptions =
            \params model ->
                props.subscriptions params model (props.toResource params model)
        , view =
            \params model ->
                props.view params model (props.toResource params model)
        }



-- Main


admin :
    { title : String
    , initModel : flags -> Browser.Navigation.Key -> ( model, Cmd msg )
    }
    -> List (NavigationItem model msg)
    -> ElmAdmin flags model msg
admin props navItems =
    let
        pagesDataList_ : List (NavigationItem model msg) -> List (ElmAdmin.Model.Page model msg)
        pagesDataList_ xs =
            xs
                |> List.foldl
                    (\navItem acc ->
                        case navItem of
                            External _ _ ->
                                acc

                            Hidden p ->
                                p :: acc

                            Single navItem_ ->
                                navItem_.page :: acc

                            Group navItem_ ->
                                pagesDataList_ navItem_.items ++ (navItem_.main.page :: acc)
                    )
                    []

        pagesDataList : List (ElmAdmin.Model.Page model msg)
        pagesDataList =
            pagesDataList_ navItems

        pageRoutes : Dict String (List (ElmAdmin.Model.Page model msg))
        pageRoutes =
            ElmAdmin.Router.toCache .path pagesDataList

        pages : Dict String (ElmAdmin.Model.Page model msg)
        pages =
            pagesDataList
                |> List.map
                    (\page_ ->
                        ( "/" ++ String.join "/" page_.path
                        , page_
                        )
                    )
                |> Dict.fromList

        viewNavItems : List (UI.Nav.UINavItem model)
        viewNavItems =
            let
                viewNavItem_ : List (NavigationItem model msg) -> List (UI.Nav.UINavItem model)
                viewNavItem_ navItems_ =
                    navItems_
                        |> List.foldl
                            (\navItem acc ->
                                case navItem of
                                    External label url ->
                                        UI.Nav.External label url :: acc

                                    Hidden _ ->
                                        acc

                                    Single navItem_ ->
                                        UI.Nav.Single
                                            { title = navItem_.title
                                            , path = pathToString navItem_.page.path
                                            , pathParams = parsePathParams navItem_.page.path
                                            , hidden = navItem_.hidden
                                            }
                                            :: acc

                                    Group { main, items } ->
                                        UI.Nav.Group
                                            { main =
                                                { title = main.title
                                                , path = pathToString main.page.path
                                                , pathParams = parsePathParams main.page.path
                                                , hidden = main.hidden
                                                }
                                            , items = viewNavItem_ items
                                            }
                                            :: acc
                            )
                            []
                        |> List.reverse
            in
            viewNavItem_ navItems
    in
    Browser.application
        { init = ElmAdmin.Model.init pageRoutes props.initModel
        , view = view props.title pages viewNavItems
        , update = update pages pageRoutes
        , subscriptions = subscriptions
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }
