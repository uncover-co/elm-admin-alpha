module ElmAdmin exposing
    ( admin, ElmAdmin
    , single, url, external, group, visualGroup, resources
    , dynamic, params, hidden, disabled, Options
    , page, resourcePage, Page, RouteParams
    , preferDarkMode, preventDarkMode, darkTheme, lightTheme, darkModeClass
    )

{-|


# Setup

@docs admin, ElmAdmin


# Navigation

@docs single, url, external, group, visualGroup, resources


# Navigation Options

@docs dynamic, params, hidden, disabled, Options


# Pages

@docs page, resourcePage, Page, RouteParams


# Themes

@docs preferDarkMode, preventDarkMode, darkTheme, lightTheme, darkModeClass

-}

import Browser
import Browser.Navigation exposing (..)
import Dict exposing (Dict)
import ElmAdmin.Model exposing (Msg(..), subscriptions, update, view)
import ElmAdmin.Router exposing (parsePathParams, pathFromString, pathToString)
import Html exposing (..)
import Html.Attributes exposing (..)
import ThemeSpec
import UI.Nav



-- Main


{-| -}
type alias ElmAdmin flags model msg =
    ElmAdmin.Model.ElmAdmin flags model msg


{-| -}
type Page model msg
    = Page (ElmAdmin.Model.Page model msg)


{-| -}
type alias RouteParams =
    { pathParams : Dict String String
    , queryParams : Dict String (List String)
    }



-- Navigation


type NavigationItem model msg
    = External String String
    | Url (ElmAdmin.Model.Page model msg)
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
    { page : ElmAdmin.Model.Page model msg
    , pathParams : List String
    , hardParams : Dict String String
    , title : String -> RouteParams -> model -> String
    , hidden : String -> RouteParams -> model -> Bool
    , disabled : String -> RouteParams -> model -> Bool
    }


type alias NavItemDataVisual model =
    { title : String -> RouteParams -> model -> String
    , hidden : String -> RouteParams -> model -> Bool
    , disabled : String -> RouteParams -> model -> Bool
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
url : Page model msg -> NavigationItem model msg
url (Page p) =
    Url p


{-| Used for creating a single page with a nav link.

    single "User" User.page

Note that all page links with params will only appear if they are already present on the current page path.

    -- User.page { path = "/users/:userId", ... }

    single "User" User.page

    -- This page link will appear if the user is on "/settings/:userId"
    -- But if the user is on "/", no ":userId" is available so the link will disappear.

-}
single : String -> Page model msg -> NavigationItem model msg
single title (Page p) =
    Single
        { page = p
        , pathParams = ElmAdmin.Router.parsePathParams p.path
        , hardParams = Dict.empty
        , hidden = \_ _ _ -> False
        , title = \_ _ _ -> title
        , disabled = \_ _ _ -> False
        }


{-| Used for creating grouped pages. Note that the "group" is also a page and if it is hidden or disabled by any means, then the whole group will follow.

    group "Workspace"
        { main = Workspace.index
        , items =
            [ single "New" Workspace.create
            , hidden "Update" Workspace.update
            ]
        }

-}
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
                    , hardParams = Dict.empty
                    , pathParams = ElmAdmin.Router.parsePathParams p.path
                    , hidden = \_ _ _ -> False
                    , title = \_ _ _ -> title
                    , disabled = \_ _ _ -> False
                    }
                , items = items
                }


{-| A shorthand for creating a group with the pages commonly used for resources.

    resources "Workspaces"
        { index = Workspace.index
        , show = Workspace.show
        , create = Workspace.create
        , update = Workspace.update
        }

-}
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
    let
        basePath =
            case props.index of
                Page page_ ->
                    pathToString page_.path
    in
    group title
        { main = props.index
        , items =
            [ visualGroup ""
                [ single "Create" props.create
                , url props.update
                , url props.show
                ]
                |> hidden
                    (\p _ _ -> not (String.startsWith p basePath))
            ]
        }


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
            { hidden = \_ _ _ -> False
            , title = \_ _ _ -> title
            , disabled = \_ _ _ -> False
            }
        , items = items
        }


{-| Conditionally hide nav links.

    single "Roles" Roles.page
        |> hidden (\_ model -> not (userIsAdmin model))

-}
hidden : (String -> RouteParams -> model -> Bool) -> NavigationItem model msg -> NavigationItem model msg
hidden fn a =
    case a of
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
disabled : (String -> RouteParams -> model -> Bool) -> NavigationItem model msg -> NavigationItem model msg
disabled fn a =
    case a of
        External _ _ ->
            a

        Url p ->
            Url { p | disabled = fn }

        Single i ->
            let
                page_ : ElmAdmin.Model.Page model msg
                page_ =
                    i.page
            in
            Single { i | disabled = fn, page = { page_ | disabled = fn } }

        Group { main, items } ->
            let
                page_ : ElmAdmin.Model.Page model msg
                page_ =
                    main.page
            in
            Group
                { main =
                    { main
                        | disabled = fn
                        , page = { page_ | disabled = fn }
                    }
                , items = items
                }

        VisualGroup { main, items } ->
            VisualGroup { main = { main | disabled = fn }, items = items }


{-| Dynamic nav link label.

    single "User" User.show
        |> dynamic
            (\{ pathParams } model ->
                Dict.get ":userId" pathParams
                    |> Maybe.andThen (username model)
            )

-}
dynamic : (String -> RouteParams -> model -> String) -> NavigationItem model msg -> NavigationItem model msg
dynamic fn a =
    case a of
        External _ _ ->
            a

        Url _ ->
            a

        Single i ->
            Single { i | title = fn }

        Group { main, items } ->
            Group
                { main = { main | title = fn }
                , items = items
                }

        VisualGroup { main, items } ->
            VisualGroup { main = { main | title = fn }, items = items }


{-| Manually pass in url params to generate fixed nav links for pages that take in pathParams.

    single "User" User.show
        |> params [ ( ":userId", "123" ) ]

-}
params : List ( String, String ) -> NavigationItem model msg -> NavigationItem model msg
params xs a =
    case a of
        External _ _ ->
            a

        Url _ ->
            a

        Single i ->
            Single { i | hardParams = Dict.fromList xs }

        Group { main, items } ->
            Group
                { main = { main | hardParams = Dict.fromList xs }
                , items = items
                }

        VisualGroup _ ->
            a



-- Pages


{-| Creates a page.
-}
page :
    { path : String
    , title : RouteParams -> model -> String
    , init : RouteParams -> model -> ( model, Cmd msg )
    , update : msg -> RouteParams -> model -> ( model, Cmd msg )
    , subscriptions : RouteParams -> model -> Sub msg
    , view : RouteParams -> model -> Html msg
    }
    -> Page model msg
page props =
    Page
        { path = pathFromString props.path
        , disabled = \_ _ _ -> False
        , title = props.title
        , init = props.init
        , update = props.update
        , subscriptions = props.subscriptions
        , view = props.view
        }


{-| Creates a page with quick access to a "resource".
-}
resourcePage :
    { path : String
    , resource : RouteParams -> model -> resource
    , title : RouteParams -> model -> resource -> String
    , init : RouteParams -> model -> resource -> ( model, Cmd msg )
    , update : msg -> RouteParams -> model -> resource -> ( model, Cmd msg )
    , subscriptions : RouteParams -> model -> resource -> Sub msg
    , view : RouteParams -> model -> resource -> Html msg
    }
    -> Page model msg
resourcePage props =
    Page
        { path =
            pathFromString props.path
        , disabled =
            \_ _ _ -> False
        , title =
            \params_ model ->
                props.title params_ model (props.resource params_ model)
        , init =
            \params_ model ->
                props.init params_ model (props.resource params_ model)
        , update =
            \msg params_ model ->
                props.update msg params_ model (props.resource params_ model)
        , subscriptions =
            \params_ model ->
                props.subscriptions params_ model (props.resource params_ model)
        , view =
            \params_ model ->
                props.view params_ model (props.resource params_ model)
        }



-- Main


{-| -}
type Options
    = Options OptionsData


type alias OptionsData =
    { lightTheme : ThemeSpec.Theme
    , darkTheme : ThemeSpec.Theme
    , preferDarkMode : Bool
    , preventDarkMode : Bool
    , darkModeClass : String
    }


defaultOptions : Options
defaultOptions =
    Options
        { preferDarkMode = True
        , preventDarkMode = False
        , lightTheme = ThemeSpec.lightTheme
        , darkTheme = ThemeSpec.darkTheme
        , darkModeClass = "eadm-dark"
        }


{-| Starts the admin on dark mode.
-}
preferDarkMode : Options -> Options
preferDarkMode (Options options) =
    Options { options | preferDarkMode = True }


{-| Removes the dark/light mode functionality.
-}
preventDarkMode : Options -> Options
preventDarkMode (Options options) =
    Options { options | preventDarkMode = True }


{-| Sets the class added to the DOM when dark mode is on.

Tip: If you're using tailwind's `dark:` variants you might want to set this to `"dark"`.

-}
darkModeClass : String -> Options -> Options
darkModeClass class_ (Options options) =
    Options { options | darkModeClass = class_ }


{-| Sets the theme used on light mode.
-}
lightTheme : ThemeSpec.Theme -> Options -> Options
lightTheme theme (Options options) =
    Options { options | lightTheme = theme }


{-| Sets the theme used on dark mode.
-}
darkTheme : ThemeSpec.Theme -> Options -> Options
darkTheme theme (Options options) =
    Options { options | lightTheme = theme }


{-| Bootstraps your admin application.

    admin
        [ A.page "Home" Home.page
        , A.external "Docs" "https://package.elm-lang.org/"
        , A.resources "Users"
            { index = Users.index
            , show = Users.show
            , create = Users.create
            , update = Users.update
            }
        , A.group "Workspaces"
            { main = Workspaces.index
            , items =
                [ A.single "Archive" Workspaces.archive
                ]
            }
        ]
        [ preferDarkMode
        , lightTheme ThemeSpec.lightTheme
        ]
        { title = "My Admin"
        , init = init
        }

-}
admin :
    List (NavigationItem model msg)
    -> List (Options -> Options)
    ->
        { title : String
        , init : flags -> Browser.Navigation.Key -> ( model, Cmd msg )
        }
    -> ElmAdmin flags model msg
admin navItems options_ props =
    let
        options : OptionsData
        options =
            List.foldl (\fn a -> fn a) defaultOptions options_
                |> (\(Options o) -> o)

        pagesDataList_ :
            List (NavigationItem model msg)
            -> List (ElmAdmin.Model.Page model msg)
        pagesDataList_ xs =
            xs
                |> List.foldl
                    (\navItem acc ->
                        case navItem of
                            External _ _ ->
                                acc

                            Url p ->
                                p :: acc

                            Single navItem_ ->
                                navItem_.page :: acc

                            Group navItem_ ->
                                pagesDataList_ navItem_.items ++ (navItem_.main.page :: acc)

                            VisualGroup navItem_ ->
                                pagesDataList_ navItem_.items ++ acc
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
                                    External label href_ ->
                                        UI.Nav.External label href_ :: acc

                                    Url _ ->
                                        acc

                                    Single navItem_ ->
                                        let
                                            path =
                                                applyHardParams navItem_.page.path navItem_.hardParams
                                        in
                                        UI.Nav.Single
                                            { title = navItem_.title
                                            , path = pathToString path
                                            , pathParams = parsePathParams path
                                            , hidden = navItem_.hidden
                                            , disabled = navItem_.disabled
                                            }
                                            :: acc

                                    Group { main, items } ->
                                        let
                                            path =
                                                applyHardParams main.page.path main.hardParams
                                        in
                                        UI.Nav.Group
                                            { main =
                                                { title = main.title
                                                , path = pathToString path
                                                , pathParams = parsePathParams path
                                                , hidden = main.hidden
                                                , disabled = main.disabled
                                                }
                                            , items = viewNavItem_ items
                                            }
                                            :: acc

                                    VisualGroup { main, items } ->
                                        UI.Nav.VisualGroup
                                            { main =
                                                { title = main.title
                                                , hidden = main.hidden
                                                , disabled = main.disabled
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
        { init =
            ElmAdmin.Model.init
                { pageRoutes = pageRoutes
                , initModel = props.init
                , preferDarkMode = options.preferDarkMode
                }
        , update = update pages pageRoutes
        , subscriptions = subscriptions
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        , view =
            view
                { title = props.title
                , preventDarkMode = options.preventDarkMode
                , darkModeClass = options.darkModeClass
                , lightTheme = options.lightTheme
                , darkTheme = options.darkTheme
                }
                pages
                viewNavItems
        }


applyHardParams : List String -> Dict String String -> List String
applyHardParams path hardParams =
    path
        |> List.map
            (\p ->
                if String.startsWith ":" p then
                    Dict.get p hardParams
                        |> Maybe.withDefault p

                else
                    p
            )
