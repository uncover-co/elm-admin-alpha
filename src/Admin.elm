module Admin exposing
    ( admin, adminWithActions, pages, protectedPages, theme, ElmAdmin, Options
    , external, folderGroup
    , hidden, disabled
    , RouteParams
    , preferDarkMode, darkTheme, lightTheme, darkModeClass, disableModeSwitch
    , Route, alwaysHidden, route
    )

{-|


# Setup

@docs admin, adminWithActions, pages, protectedPages, theme, ElmAdmin, Options


# Navigation

@docs single, url, external, group, visualGroup, folderGroup, NavigationItem


# Navigation Options

@docs hidden, disabled


# Pages

@docs RouteParams


# Themes

@docs preferDarkMode, darkTheme, lightTheme, darkModeClass, disableModeSwitch

-}

import Browser
import Browser.Navigation exposing (..)
import Dict exposing (Dict)
import ElmAdmin.Actions
import ElmAdmin.Application
import ElmAdmin.Internal.InvalidRouteData exposing (InvalidRouteData)
import ElmAdmin.Internal.Page exposing (Page, RouteData, routeData)
import ElmAdmin.Router
import ElmAdmin.Shared exposing (Action, Effect(..), Msg(..))
import ElmAdmin.UI.Invalid
import ElmAdmin.UI.Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Set exposing (Set)
import ThemeProvider
import ThemeSpec



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
type Route model msg
    = Invalid InvalidRouteData
    | External
        { url : String
        , label : String
        }
    | Internal
        { routeData : RouteData model msg
        , title : RouteParams -> model -> String
        , hidden : RouteParams -> model -> Bool
        , disabled : RouteParams -> model -> Bool
        , pages : List (Route model msg)
        }


{-| Used for creating external links in your sidebar.

    external "Docs" "https://package.elm-lang.org/"

-}
external : String -> String -> Route model msg
external url_ label =
    External { url = url_, label = label }


{-| Used for creating a single page with a nav link.

    single "/users/:userId" "User" User.page

Note that all page links with params will only appear if they are already present on the current page path.

    -- User.page { path = "/users/:userId", ... }

    single "User" User.page

    -- This page link will appear if the user is on "/settings/:userId"
    -- But if the user is on "/", no ":userId" is available so the link will disappear.

-}
route :
    String
    ->
        { page : Page model msg params
        , options : List (Route model msg -> Route model msg)
        }
    -> List (Route model msg)
    -> Route model msg
route path props subPages =
    case ElmAdmin.Internal.Page.routeData props.page path of
        Just routeData ->
            Internal
                { pages = subPages
                , routeData = routeData
                , hidden = \_ _ -> False
                , title = \_ _ -> ElmAdmin.Internal.Page.toTitle props.page
                , disabled = \_ _ -> False
                }

        Nothing ->
            Invalid
                { path = path
                , page = ElmAdmin.Internal.Page.toTitle props.page
                , error = ""
                }


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
    -> Page model msg params
    -> List (Route model msg)
    -> Route model msg
folderGroup path page items_ =
    let
        paths_ : List (Route model msg) -> Set String -> Set String
        paths_ items__ acc_ =
            List.foldl
                (\item acc ->
                    case item of
                        Invalid _ ->
                            acc

                        External _ ->
                            acc

                        Internal p ->
                            Set.insert p.routeData.path acc
                                |> paths_ p.pages
                )
                acc_
                items__

        paths =
            paths_ items_ (Set.fromList [ path ])
    in
    route path
        { page = page
        , options = []
        }
        (items_
            |> List.map
                (\p ->
                    hidden
                        (\r _ -> not (Set.member r.path paths))
                        p
                )
        )


{-| Hides the route from the navigation. Making it only accessible through the direct url.
-}
alwaysHidden : Route model msg -> Route model msg
alwaysHidden =
    hidden (\_ _ -> True)


{-| Conditionally hide nav links.

    single "Roles" Roles.page
        |> hidden (\_ model -> not (userIsAdmin model))

-}
hidden : (RouteParams -> model -> Bool) -> Route model msg -> Route model msg
hidden fn a =
    case a of
        Invalid _ ->
            a

        External _ ->
            a

        Internal v ->
            Internal { v | hidden = fn }


{-| Conditionally disable nav links and their pages. They won't be accessible even through urls.

    single "Roles" Roles.page
        |> disable (\_ model -> not (userIsAdmin model))

-}
disabled : (RouteParams -> model -> Bool) -> Route model msg -> Route model msg
disabled fn a =
    case a of
        Invalid _ ->
            a

        External _ ->
            a

        Internal i ->
            let
                routeData : RouteData model msg
                routeData =
                    i.routeData
            in
            Internal
                { i
                    | disabled = fn
                    , routeData = { routeData | disabled = fn }
                    , pages =
                        List.map (disableSubItem fn) i.pages
                }


{-| Used for propagating a parent disable across children.

It grabs the current disable function and creates a new one that sums both checks.

-}
disableSubItem : (RouteParams -> model -> Bool) -> Route model msg -> Route model msg
disableSubItem fn item =
    let
        sumDisabled : (RouteParams -> model -> Bool) -> RouteParams -> model -> Bool
        sumDisabled fn2 routeParams model =
            fn routeParams model || fn2 routeParams model
    in
    case item of
        Invalid _ ->
            item

        External _ ->
            item

        Internal i ->
            let
                routeData : RouteData model msg
                routeData =
                    i.routeData
            in
            Internal
                { i
                    | disabled = sumDisabled i.disabled
                    , routeData =
                        { routeData
                            | disabled = sumDisabled routeData.disabled
                        }
                    , pages = List.map (disableSubItem fn) i.pages
                }



-- Main


{-| -}
type ThemeOptions
    = ThemeOptions ThemeOptionsData


type alias ThemeOptionsData =
    { lightTheme : ThemeProvider.Theme
    , darkTheme : ThemeProvider.Theme
    , preferDarkMode : Bool
    , darkModeStrategy : ThemeProvider.DarkModeStrategy
    , disableModeSwitch : Bool
    }


themeDefaults : ThemeOptions
themeDefaults =
    ThemeOptions
        { preferDarkMode = False
        , lightTheme = ThemeSpec.theme ThemeSpec.lightTheme
        , darkTheme = ThemeSpec.theme ThemeSpec.darkTheme
        , darkModeStrategy = ThemeProvider.ClassStrategy "eadm-dark"
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
darkModeClass : ThemeProvider.DarkModeStrategy -> ThemeOptions -> ThemeOptions
darkModeClass strategy (ThemeOptions options) =
    ThemeOptions { options | darkModeStrategy = strategy }


{-| Sets the theme used on light mode.
-}
lightTheme : ThemeProvider.Theme -> ThemeOptions -> ThemeOptions
lightTheme theme_ (ThemeOptions options) =
    ThemeOptions { options | lightTheme = theme_ }


{-| Sets the theme used on dark mode.
-}
darkTheme : ThemeProvider.Theme -> ThemeOptions -> ThemeOptions
darkTheme theme_ (ThemeOptions options) =
    ThemeOptions { options | lightTheme = theme_ }


{-| -}
type Options flags model protectedModel msg
    = Options (OptionsData model protectedModel msg)


type alias OptionsData model protectedModel msg =
    { theme : ThemeOptions
    , pages : List (Route model msg)
    , protectedPages : List (Route protectedModel msg)
    , protectedModel : model -> Maybe protectedModel
    , protectedToModel : model -> protectedModel -> model
    }


defaultOptions : Options flags model protectedModel msg
defaultOptions =
    Options
        { theme = themeDefaults
        , pages = []
        , protectedPages = []
        , protectedModel = \_ -> Nothing
        , protectedToModel = \model _ -> model
        }


{-| Defines a list of pages for your application.
-}
pages : List (Route model msg) -> Options flags model protectedModel msg -> Options flags model protectedModel msg
pages pages_ (Options options) =
    Options { options | pages = pages_ }


{-| Defines a list of pages for your application.
-}
protectedPages :
    { fromModel : model -> Maybe protectedModel
    , toModel : model -> protectedModel -> model
    }
    -> List (Route protectedModel msg)
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
-}
admin :
    { title : String
    , init : flags -> Browser.Navigation.Key -> ( model, Cmd msg )
    , update : RouteParams -> msg -> model -> ( model, Cmd msg )
    , subscriptions : RouteParams -> model -> Sub msg
    }
    -> List (Options flags model protectedModel msg -> Options flags model protectedModel msg)
    -> ElmAdmin flags model msg
admin props =
    adminWithActions
        { title = props.title
        , init =
            \flags key ->
                props.init flags key
                    |> Tuple.mapSecond ElmAdmin.Actions.cmd
        , update =
            \routeParams msg model ->
                props.update routeParams msg model
                    |> Tuple.mapSecond ElmAdmin.Actions.cmd
        , subscriptions = props.subscriptions
        }


{-| Bootstraps your admin application with actions.
-}
adminWithActions :
    { title : String
    , init : flags -> Browser.Navigation.Key -> ( model, Action msg )
    , update : RouteParams -> msg -> model -> ( model, Action msg )
    , subscriptions : RouteParams -> model -> Sub msg
    }
    -> List (Options flags model protectedModel msg -> Options flags model protectedModel msg)
    -> ElmAdmin flags model msg
adminWithActions props options_ =
    let
        options : OptionsData model protectedModel msg
        options =
            List.foldl (\fn a -> fn a) defaultOptions options_
                |> (\(Options o) -> o)

        theme_ : ThemeOptionsData
        theme_ =
            case options.theme of
                ThemeOptions d ->
                    d

        routeDatas :
            List (Route m msg)
            -> List (RouteData m msg)
        routeDatas xs =
            xs
                |> List.foldl
                    (\navItem acc ->
                        case navItem of
                            Invalid _ ->
                                acc

                            External _ ->
                                acc

                            Internal navItem_ ->
                                routeDatas navItem_.pages ++ (navItem_.routeData :: acc)
                    )
                    []

        pagesRouteList : List (RouteData model msg)
        pagesRouteList =
            routeDatas options.pages

        protectedPagesRouteList : List (RouteData protectedModel msg)
        protectedPagesRouteList =
            routeDatas options.protectedPages

        pageRouteDatas : Dict String (List (RouteData model msg))
        pageRouteDatas =
            ElmAdmin.Router.toCache .pathList pagesRouteList

        protectedPageRouteDatas : Dict String (List (RouteData protectedModel msg))
        protectedPageRouteDatas =
            ElmAdmin.Router.toCache .pathList protectedPagesRouteList

        pages_ : Dict String (RouteData model msg)
        pages_ =
            pagesRouteList
                |> List.map (\routeData -> ( routeData.path, routeData ))
                |> Dict.fromList

        protectedPages_ : Dict String (RouteData protectedModel msg)
        protectedPages_ =
            protectedPagesRouteList
                |> List.map (\routeData -> ( routeData.path, routeData ))
                |> Dict.fromList

        viewNavItems : List (ElmAdmin.UI.Nav.UINavItem model)
        viewNavItems =
            viewNavItemsFromNavigationItems options.pages

        viewProtectedNavItems : List (ElmAdmin.UI.Nav.UINavItem protectedModel)
        viewProtectedNavItems =
            viewNavItemsFromNavigationItems options.protectedPages

        invalidRoutes : List InvalidRouteData
        invalidRoutes =
            let
                invalidRoutes_ xs_ acc =
                    xs_
                        |> List.foldl
                            (\item acc_ ->
                                case item of
                                    Invalid data ->
                                        data :: acc_

                                    External _ ->
                                        acc_

                                    Internal p ->
                                        invalidRoutes_ p.pages acc_
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
                                    Invalid { path } ->
                                        if Set.member path paths then
                                            ( paths, Set.insert path duplicatedPaths )

                                        else
                                            ( Set.insert path paths, duplicatedPaths )

                                    External _ ->
                                        ( paths, duplicatedPaths )

                                    Internal item_ ->
                                        let
                                            acc__ =
                                                if Set.member item_.routeData.path paths then
                                                    ( paths, Set.insert item_.routeData.path duplicatedPaths )

                                                else
                                                    ( Set.insert item_.routeData.path paths, duplicatedPaths )
                                        in
                                        invalidRoutes_ item_.pages acc__
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
                        , pageRouteDatas = pageRouteDatas
                        , protectedPageRouteDatas = protectedPageRouteDatas
                        , protectedModel = options.protectedModel
                        , protectedToModel = options.protectedToModel
                        , preferDarkMode = theme_.preferDarkMode
                        }
                , update =
                    ElmAdmin.Application.update
                        { update = props.update
                        , pages = pages_
                        , protectedPages = protectedPages_
                        , pageRouteDatas = pageRouteDatas
                        , protectedPageRouteDatas = protectedPageRouteDatas
                        , protectedModel = options.protectedModel
                        , protectedToModel = options.protectedToModel
                        }
                , subscriptions =
                    ElmAdmin.Application.subscriptions
                        { subscriptions = props.subscriptions
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
                , init = ElmAdmin.Application.initError props.init
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


viewNavItemsFromNavigationItems : List (Route m msg) -> List (ElmAdmin.UI.Nav.UINavItem m)
viewNavItemsFromNavigationItems ps =
    let
        go : List (Route m msg) -> List (ElmAdmin.UI.Nav.UINavItem m)
        go navItems_ =
            navItems_
                |> List.foldl
                    (\navItem acc ->
                        case navItem of
                            Invalid _ ->
                                acc

                            External data ->
                                ElmAdmin.UI.Nav.External data :: acc

                            Internal navItem_ ->
                                ElmAdmin.UI.Nav.Group
                                    { main =
                                        { title = navItem_.title
                                        , path = navItem_.routeData.path
                                        , pathParams = navItem_.routeData.pathParams
                                        , hidden = navItem_.hidden
                                        , disabled = navItem_.disabled
                                        }
                                    , items = go navItem_.pages
                                    }
                                    :: acc
                    )
                    []
                |> List.reverse
    in
    go ps
