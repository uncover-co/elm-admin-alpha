module ElmAdmin exposing
    ( admin, pages, protectedPages, update, updateWithEffect, subscriptions, theme, ElmAdmin
    , single, url, external, group, visualGroup, folderGroup, NavigationItem
    , hidden, disabled, Options
    , RouteParams
    , preferDarkMode, darkTheme, lightTheme, darkModeClass, disableModeSwitch
    )

{-|


# Setup

@docs admin, pages, protectedPages, update, updateWithEffect, subscriptions, theme, ElmAdmin


# Navigation

@docs single, url, external, group, visualGroup, folderGroup, NavigationItem


# Navigation Options

@docs hidden, disabled, Options


# Pages

@docs RouteParams


# Themes

@docs preferDarkMode, darkTheme, lightTheme, darkModeClass, disableModeSwitch

-}

import Browser
import Browser.Navigation exposing (..)
import Dict exposing (Dict)
import ElmAdmin.Application
import ElmAdmin.Internal.Form
import ElmAdmin.Internal.Page exposing (Page, Route)
import ElmAdmin.Router
import ElmAdmin.Shared exposing (Effect(..), Msg(..), SubCmd)
import ElmAdmin.UI.Invalid
import ElmAdmin.UI.Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Set exposing (Set)
import SubCmd
import ThemeSpec
import Url exposing (Url)



-- Main


{-| -}
type alias ElmAdmin flags model msg =
    ElmAdmin.Shared.ElmAdmin flags model msg


{-| -}
type alias RouteParams =
    { path : String
    , pathParams : Dict String String
    , queryParams : Dict String (List String)
    }



-- Navigation


{-| -}
type NavigationItem model msg
    = Invalid String String
    | External String String
    | Url (NavItemHidden model msg)
    | Single (NavItemData model msg)
    | Group
        { main : NavItemData model msg
        , items : List (NavigationItem model msg)
        }
    | VisualGroup
        { main : NavItemDataVisual model
        , items : List (NavigationItem model msg)
        }


type alias NavItemData model msg =
    { route : Route model msg
    , title : RouteParams -> model -> String
    , hidden : RouteParams -> model -> Bool
    , disabled : RouteParams -> model -> Bool
    }


type alias NavItemHidden model msg =
    { route : Route model msg
    , disabled : RouteParams -> model -> Bool
    }


type alias NavItemDataVisual model =
    { title : RouteParams -> model -> String
    , hidden : RouteParams -> model -> Bool
    , disabled : RouteParams -> model -> Bool
    }


{-| Used for creating external links in your sidebar.

    external "Docs" "https://package.elm-lang.org/"

-}
external : String -> String -> NavigationItem model msg
external =
    External


{-| Used for creating pages that will not appear as nav links but are still accessible through their direct url.

    url Home.page

-}
url : String -> Page model msg params -> NavigationItem model msg
url path page =
    case ElmAdmin.Internal.Page.route page path of
        Just route ->
            Url
                { route = route
                , disabled = \_ _ -> False
                }

        Nothing ->
            Invalid (ElmAdmin.Internal.Page.toTitle page) path


{-| Used for creating a single page with a nav link.

    single "/users/:userId" "User" User.page

Note that all page links with params will only appear if they are already present on the current page path.

    -- User.page { path = "/users/:userId", ... }

    single "User" User.page

    -- This page link will appear if the user is on "/settings/:userId"
    -- But if the user is on "/", no ":userId" is available so the link will disappear.

-}
single : String -> String -> Page model msg params -> NavigationItem model msg
single path title page =
    case ElmAdmin.Internal.Page.route page path of
        Just route ->
            Single
                { route = route
                , hidden = \_ _ -> False
                , title = \_ _ -> title
                , disabled = \_ _ -> False
                }

        Nothing ->
            Invalid (ElmAdmin.Internal.Page.toTitle page) path


{-| Used for creating grouped pages. Note that the "group" is also a page and if it is hidden or disabled by any means, then the whole group will follow.

    group "Workspace"
        Workspace.index
        [ single "New" Workspace.create
        , hidden "Update" Workspace.update
        ]

-}
group :
    String
    -> String
    -> Page model msg params
    -> List (NavigationItem model msg)
    -> NavigationItem model msg
group path title page items =
    case ElmAdmin.Internal.Page.route page path of
        Just route ->
            Group
                { main =
                    { route = route
                    , hidden = \_ _ -> False
                    , title = \_ _ -> title
                    , disabled = \_ _ -> False
                    }
                , items = items
                }

        Nothing ->
            Invalid (ElmAdmin.Internal.Page.toTitle page) path


{-| A group that shows its items only if one of them is the current active path.

    folderGroup "Workspaces"
        Workspace.index
        [ single "New" Workspaces.new
        , url "Show" Workspaces.show
        , url "Update" Workspaces.update
        ]

-}
folderGroup :
    String
    -> String
    -> Page model msg params
    -> List (NavigationItem model msg)
    -> NavigationItem model msg
folderGroup path title page items_ =
    let
        paths_ : List (NavigationItem model msg) -> Set String -> Set String
        paths_ items__ acc_ =
            List.foldl
                (\item acc ->
                    case item of
                        Invalid _ _ ->
                            acc

                        External _ _ ->
                            acc

                        Single p ->
                            Set.insert p.route.path acc

                        Url p ->
                            Set.insert p.route.path acc

                        Group { main, items } ->
                            Set.insert main.route.path acc
                                |> paths_ items

                        VisualGroup { items } ->
                            paths_ items acc
                )
                acc_
                items__

        paths =
            paths_ items_ (Set.fromList [ path ])
    in
    group
        path
        title
        page
        [ visualGroup "" items_
            |> hidden
                (\r _ -> not (Set.member r.path paths))
        ]


{-| Creates a visual group (The group label itself is not a nav link).

Tip: This can be useful if you want to disable whole sections of your application but you don't want that to be related to one specific root page.

    visualGroup "Workspace"
        [ single "New" Workspace.create
        , hidden "Update" Workspace.update
        ]

-}
visualGroup : String -> List (NavigationItem model msg) -> NavigationItem model msg
visualGroup title items =
    VisualGroup
        { main =
            { hidden = \_ _ -> False
            , title = \_ _ -> title
            , disabled = \_ _ -> False
            }
        , items = items
        }


{-| Conditionally hide nav links.

    single "Roles" Roles.page
        |> hidden (\_ model -> not (userIsAdmin model))

-}
hidden : (RouteParams -> model -> Bool) -> NavigationItem model msg -> NavigationItem model msg
hidden fn a =
    case a of
        Invalid _ _ ->
            a

        External _ _ ->
            a

        Url _ ->
            a

        Single i ->
            Single { i | hidden = fn }

        Group { main, items } ->
            Group { main = { main | hidden = fn }, items = items }

        VisualGroup { main, items } ->
            VisualGroup { main = { main | hidden = fn }, items = items }


{-| Conditionally disable nav links and their pages. They won't be accessible even through urls.

    single "Roles" Roles.page
        |> disable (\_ model -> not (userIsAdmin model))

-}
disabled : (RouteParams -> model -> Bool) -> NavigationItem model msg -> NavigationItem model msg
disabled fn a =
    case a of
        Invalid _ _ ->
            a

        External _ _ ->
            a

        Url p ->
            Url { p | disabled = fn }

        Single i ->
            let
                route : Route model msg
                route =
                    i.route
            in
            Single { i | disabled = fn, route = { route | disabled = fn } }

        Group { main, items } ->
            let
                route : Route model msg
                route =
                    main.route
            in
            Group
                { main =
                    { main
                        | disabled = fn
                        , route = { route | disabled = fn }
                    }
                , items =
                    List.map (disableSubItem fn) items
                }

        VisualGroup { main, items } ->
            VisualGroup
                { main = { main | disabled = fn }
                , items =
                    List.map (disableSubItem fn) items
                }


{-| Used for propagating a parent disable across children.

It grabs the current disable function and creates a new one that sums both checks.

-}
disableSubItem : (RouteParams -> model -> Bool) -> NavigationItem model msg -> NavigationItem model msg
disableSubItem fn item =
    let
        sumDisabled : (RouteParams -> model -> Bool) -> RouteParams -> model -> Bool
        sumDisabled fn2 routeParams model =
            fn routeParams model || fn2 routeParams model
    in
    case item of
        Invalid _ _ ->
            item

        External _ _ ->
            item

        Url p ->
            Url { p | disabled = sumDisabled p.disabled }

        Single i ->
            let
                route : Route model msg
                route =
                    i.route
            in
            Single
                { i
                    | disabled = sumDisabled i.disabled
                    , route =
                        { route
                            | disabled = sumDisabled route.disabled
                        }
                }

        Group { main, items } ->
            let
                route : Route model msg
                route =
                    main.route
            in
            Group
                { main =
                    { main
                        | disabled = sumDisabled main.disabled
                        , route =
                            { route
                                | disabled = sumDisabled route.disabled
                            }
                    }
                , items =
                    List.map (disableSubItem fn) items
                }

        VisualGroup { main, items } ->
            VisualGroup
                { main =
                    { main
                        | disabled =
                            sumDisabled main.disabled
                    }
                , items =
                    List.map (disableSubItem fn) items
                }



-- Main


{-| -}
type ThemeOptions
    = ThemeOptions ThemeOptionsData


type alias ThemeOptionsData =
    { lightTheme : ThemeSpec.Theme
    , darkTheme : ThemeSpec.Theme
    , preferDarkMode : Bool
    , darkModeStrategy : ThemeSpec.DarkModeStrategy
    , disableModeSwitch : Bool
    }


themeDefaults : ThemeOptions
themeDefaults =
    ThemeOptions
        { preferDarkMode = False
        , lightTheme = ThemeSpec.lightTheme
        , darkTheme = ThemeSpec.darkTheme
        , darkModeStrategy = ThemeSpec.ClassStrategy "eadm-dark"
        , disableModeSwitch = False
        }


{-| Customize your admin theme through a list of options.
-}
theme : List (ThemeOptions -> ThemeOptions) -> Options flags model protectedModel msg -> Options flags model protectedModel msg
theme theme_ (Options options) =
    Options { options | theme = List.foldl (\fn a -> fn a) themeDefaults theme_ }


{-| Starts the admin on dark mode.
-}
preferDarkMode : ThemeOptions -> ThemeOptions
preferDarkMode (ThemeOptions options) =
    ThemeOptions { options | preferDarkMode = True }


{-| Prevents the user from selecting a dark or light mode.
-}
disableModeSwitch : ThemeOptions -> ThemeOptions
disableModeSwitch (ThemeOptions options) =
    ThemeOptions { options | disableModeSwitch = True }


{-| Sets the dark mode strategy. Check out [elm-theme-spec](https://package.elm-lang.org/packages/uncover-co/elm-theme-spec/latest/) for more.

Tip: If you're using tailwind's `dark:` variants you might want to set this to `ThemeSpec.ClassStrategy "dark"`.

-}
darkModeClass : ThemeSpec.DarkModeStrategy -> ThemeOptions -> ThemeOptions
darkModeClass strategy (ThemeOptions options) =
    ThemeOptions { options | darkModeStrategy = strategy }


{-| Sets the theme used on light mode.
-}
lightTheme : ThemeSpec.Theme -> ThemeOptions -> ThemeOptions
lightTheme theme_ (ThemeOptions options) =
    ThemeOptions { options | lightTheme = theme_ }


{-| Sets the theme used on dark mode.
-}
darkTheme : ThemeSpec.Theme -> ThemeOptions -> ThemeOptions
darkTheme theme_ (ThemeOptions options) =
    ThemeOptions { options | lightTheme = theme_ }


{-| -}
type Options flags model protectedModel msg
    = Options (OptionsData flags model protectedModel msg)


type alias OptionsData flags model protectedModel msg =
    { theme : ThemeOptions
    , init : Maybe (flags -> Browser.Navigation.Key -> ( model, SubCmd msg ))
    , update : RouteParams -> msg -> model -> ( model, SubCmd msg )
    , subscriptions : RouteParams -> model -> Sub msg
    , pages : List (NavigationItem model msg)
    , protectedPages : List (NavigationItem protectedModel msg)
    , protectedModel : model -> Maybe protectedModel
    , protectedToModel : model -> protectedModel -> model
    }


defaultOptions : Options flags model protectedModel msg
defaultOptions =
    Options
        { theme = themeDefaults
        , init = Nothing
        , update = \_ _ model -> ( model, SubCmd.none )
        , subscriptions = \_ _ -> Sub.none
        , pages = []
        , protectedPages = []
        , protectedModel = \_ -> Nothing
        , protectedToModel = \model _ -> model
        }


{-| -}
update : (RouteParams -> msg -> model -> ( model, Cmd msg )) -> Options flags model protectedModel msg -> Options flags model protectedModel msg
update update_ (Options options) =
    Options
        { options
            | update =
                \routeParams msg model ->
                    update_ routeParams msg model
                        |> Tuple.mapSecond SubCmd.cmd
        }


{-| -}
updateWithEffect : (RouteParams -> msg -> model -> ( model, SubCmd msg )) -> Options flags model protectedModel msg -> Options flags model protectedModel msg
updateWithEffect update_ (Options options) =
    Options { options | update = update_ }


{-| -}
subscriptions : (RouteParams -> model -> Sub msg) -> Options flags model protectedModel msg -> Options flags model protectedModel msg
subscriptions subscriptions_ (Options options) =
    Options
        { options
            | subscriptions =
                \routeParams model ->
                    subscriptions_ routeParams model
        }


{-| Defines a list of pages for your application.
-}
pages : List (NavigationItem model msg) -> Options flags model protectedModel msg -> Options flags model protectedModel msg
pages pages_ (Options options) =
    Options { options | pages = pages_ }


{-| Defines a list of pages for your application.
-}
protectedPages :
    { fromModel : model -> Maybe protectedModel
    , toModel : model -> protectedModel -> model
    }
    -> List (NavigationItem protectedModel msg)
    -> Options flags model protectedModel msg
    -> Options flags model protectedModel msg
protectedPages { fromModel, toModel } protectedPages_ (Options options) =
    Options
        { options
            | protectedPages = protectedPages_
            , protectedModel = fromModel
            , protectedToModel = toModel
        }


{-| Bootstraps your admin application.

    admin
        { title = "My Admin"
        , init = init
        }
        [ theme [ preferDarkMode ]
        , update myUpdate
        , pages
            [ A.external "Docs" "https://package.elm-lang.org/"
            , A.single "Home" Home.page
            , A.group "Users"
                Users.index
                [ A.single "Create" Users.create
                , A.url Users.show
                , A.url Users.update
                ]
            ]
        ]

-}
admin :
    { title : String, init : flags -> Browser.Navigation.Key -> ( model, Cmd msg ) }
    -> List (Options flags model protectedModel msg -> Options flags model protectedModel msg)
    -> ElmAdmin flags model msg
admin props options_ =
    let
        options : OptionsData flags model protectedModel msg
        options =
            List.foldl (\fn a -> fn a) defaultOptions options_
                |> (\(Options o) -> o)

        theme_ : ThemeOptionsData
        theme_ =
            case options.theme of
                ThemeOptions d ->
                    d

        routes :
            List (NavigationItem m msg)
            -> List (Route m msg)
        routes xs =
            xs
                |> List.foldl
                    (\navItem acc ->
                        case navItem of
                            Invalid _ _ ->
                                acc

                            External _ _ ->
                                acc

                            Url p ->
                                p.route :: acc

                            Single navItem_ ->
                                navItem_.route :: acc

                            Group navItem_ ->
                                routes navItem_.items ++ (navItem_.main.route :: acc)

                            VisualGroup navItem_ ->
                                routes navItem_.items ++ acc
                    )
                    []

        pagesRouteList : List (Route model msg)
        pagesRouteList =
            routes options.pages

        protectedPagesRouteList : List (Route protectedModel msg)
        protectedPagesRouteList =
            routes options.protectedPages

        pageRoutes : Dict String (List (Route model msg))
        pageRoutes =
            ElmAdmin.Router.toCache .pathList pagesRouteList

        protectedPageRoutes : Dict String (List (Route protectedModel msg))
        protectedPageRoutes =
            ElmAdmin.Router.toCache .pathList protectedPagesRouteList

        pages_ : Dict String (Route model msg)
        pages_ =
            pagesRouteList
                |> List.map (\route -> ( route.path, route ))
                |> Dict.fromList

        protectedPages_ : Dict String (Route protectedModel msg)
        protectedPages_ =
            protectedPagesRouteList
                |> List.map (\route -> ( route.path, route ))
                |> Dict.fromList

        viewNavItems : List (ElmAdmin.UI.Nav.UINavItem model)
        viewNavItems =
            viewNavItemsFromNavigatiomItems options.pages

        viewProtectedNavItems : List (ElmAdmin.UI.Nav.UINavItem protectedModel)
        viewProtectedNavItems =
            viewNavItemsFromNavigatiomItems options.protectedPages

        invalidRoutes : List ( String, String )
        invalidRoutes =
            let
                invalidRoutes_ xs_ acc =
                    xs_
                        |> List.foldl
                            (\item acc_ ->
                                case item of
                                    Invalid title_ path ->
                                        ( title_, path ) :: acc_

                                    Group { items } ->
                                        invalidRoutes_ items acc_

                                    VisualGroup { items } ->
                                        invalidRoutes_ items acc_

                                    _ ->
                                        acc_
                            )
                            acc
            in
            invalidRoutes_ options.pages [] ++ invalidRoutes_ options.protectedPages []

        duplicatedRoutes : List String
        duplicatedRoutes =
            let
                invalidRoutes_ xs_ acc =
                    xs_
                        |> List.foldl
                            (\item ( paths, duplicatedPaths ) ->
                                case item of
                                    Invalid _ path ->
                                        if Set.member path paths then
                                            ( paths, Set.insert path duplicatedPaths )

                                        else
                                            ( Set.insert path paths, duplicatedPaths )

                                    External _ _ ->
                                        ( paths, duplicatedPaths )

                                    Url item_ ->
                                        if Set.member item_.route.path paths then
                                            ( paths, Set.insert item_.route.path duplicatedPaths )

                                        else
                                            ( Set.insert item_.route.path paths, duplicatedPaths )

                                    Single item_ ->
                                        if Set.member item_.route.path paths then
                                            ( paths, Set.insert item_.route.path duplicatedPaths )

                                        else
                                            ( Set.insert item_.route.path paths, duplicatedPaths )

                                    Group { main, items } ->
                                        let
                                            acc__ =
                                                if Set.member main.route.path paths then
                                                    ( paths, Set.insert main.route.path duplicatedPaths )

                                                else
                                                    ( Set.insert main.route.path paths, duplicatedPaths )
                                        in
                                        invalidRoutes_ items acc__

                                    VisualGroup { items } ->
                                        invalidRoutes_ items ( paths, duplicatedPaths )
                            )
                            acc

                ( pagePaths, duplicatedPagePaths ) =
                    invalidRoutes_ options.pages ( Set.empty, Set.empty )

                ( protectedPagePaths, duplicatedProtectedPagePaths ) =
                    invalidRoutes_ options.protectedPages ( Set.empty, Set.empty )
            in
            Set.intersect pagePaths protectedPagePaths
                |> Set.union duplicatedPagePaths
                |> Set.union duplicatedProtectedPagePaths
                |> Set.toList
    in
    case ( invalidRoutes, duplicatedRoutes ) of
        ( [], [] ) ->
            Browser.application
                { onUrlChange = OnUrlChange
                , onUrlRequest = OnUrlRequest
                , init =
                    ElmAdmin.Application.init
                        { init = props.init
                        , pageRoutes = pageRoutes
                        , protectedPageRoutes = protectedPageRoutes
                        , protectedModel = options.protectedModel
                        , protectedToModel = options.protectedToModel
                        , preferDarkMode = theme_.preferDarkMode
                        }
                , update =
                    ElmAdmin.Application.update
                        { update = options.update
                        , pages = pages_
                        , protectedPages = protectedPages_
                        , pageRoutes = pageRoutes
                        , protectedPageRoutes = protectedPageRoutes
                        , protectedModel = options.protectedModel
                        , protectedToModel = options.protectedToModel
                        }
                , subscriptions =
                    ElmAdmin.Application.subscriptions
                        { subscriptions = options.subscriptions
                        , pages = pages_
                        , protectedPages = protectedPages_
                        , protectedModel = options.protectedModel
                        , protectedToModel = options.protectedToModel
                        }
                , view =
                    ElmAdmin.Application.view
                        { title = props.title
                        , navItems = viewNavItems
                        , protectedNavItems = viewProtectedNavItems
                        , pages = pages_
                        , protectedPages = protectedPages_
                        , protectedModel = options.protectedModel
                        , theme =
                            { lightTheme = theme_.lightTheme
                            , darkTheme = theme_.darkTheme
                            , darkModeStrategy = theme_.darkModeStrategy
                            , preferDarkMode = theme_.preferDarkMode
                            , disableModeSwitch = theme_.disableModeSwitch
                            }
                        }
                }

        _ ->
            Browser.application
                { onUrlChange = OnUrlChange
                , onUrlRequest = OnUrlRequest
                , init =
                    \flags _ key ->
                        ( { navKey = key
                          , model = props.init flags key |> Tuple.first
                          , routeParams = ElmAdmin.Router.emptyRouteParams
                          , darkMode = True
                          , formModel = ElmAdmin.Internal.Form.empty
                          }
                        , Cmd.none
                        )
                , update = \_ model -> ( model, Cmd.none )
                , subscriptions = \_ -> Sub.none
                , view =
                    \_ ->
                        { title = props.title
                        , body =
                            [ ElmAdmin.UI.Invalid.view
                                duplicatedRoutes
                                invalidRoutes
                            ]
                        }
                }


viewNavItemsFromNavigatiomItems : List (NavigationItem m msg) -> List (ElmAdmin.UI.Nav.UINavItem m)
viewNavItemsFromNavigatiomItems ps =
    let
        go : List (NavigationItem m msg) -> List (ElmAdmin.UI.Nav.UINavItem m)
        go navItems_ =
            navItems_
                |> List.foldl
                    (\navItem acc ->
                        case navItem of
                            Invalid _ _ ->
                                acc

                            External label href_ ->
                                ElmAdmin.UI.Nav.External label href_ :: acc

                            Url _ ->
                                acc

                            Single navItem_ ->
                                ElmAdmin.UI.Nav.Single
                                    { title = navItem_.title
                                    , path = navItem_.route.path
                                    , pathParams = navItem_.route.pathParams
                                    , hidden = navItem_.hidden
                                    , disabled = navItem_.disabled
                                    }
                                    :: acc

                            Group { main, items } ->
                                ElmAdmin.UI.Nav.Group
                                    { main =
                                        { title = main.title
                                        , path = main.route.path
                                        , pathParams = main.route.pathParams
                                        , hidden = main.hidden
                                        , disabled = main.disabled
                                        }
                                    , items = go items
                                    }
                                    :: acc

                            VisualGroup { main, items } ->
                                ElmAdmin.UI.Nav.VisualGroup
                                    { main =
                                        { title = main.title
                                        , hidden = main.hidden
                                        , disabled = main.disabled
                                        }
                                    , items = go items
                                    }
                                    :: acc
                    )
                    []
                |> List.reverse
    in
    go ps
