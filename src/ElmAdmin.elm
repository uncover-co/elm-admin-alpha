module ElmAdmin exposing
    ( admin, ElmAdmin
    , single, url, external, group, visualGroup, folderGroup, NavigationItem
    , dynamic, params, hidden, disabled, Options
    , page, fullPage, resourcePage, Page, RouteParams
    , preferDarkMode, preventDarkMode, darkTheme, lightTheme, darkModeClass
    )

{-|


# Setup

@docs admin, ElmAdmin


# Navigation

@docs single, url, external, group, visualGroup, folderGroup, NavigationItem


# Navigation Options

@docs dynamic, params, hidden, disabled, Options


# Pages

@docs page, fullPage, resourcePage, Page, RouteParams


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
import Set exposing (Set)
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
    { path : String
    , pathParams : Dict String String
    , queryParams : Dict String (List String)
    }



-- Navigation


{-| -}
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
    , title : RouteParams -> model -> String
    , hidden : RouteParams -> model -> Bool
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
        , hidden = \_ _ -> False
        , title = \_ _ -> title
        , disabled = \_ _ -> False
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
    -> Page model msg
    -> List (NavigationItem model msg)
    -> NavigationItem model msg
group title main items =
    case main of
        Page p ->
            Group
                { main =
                    { page = p
                    , hardParams = Dict.empty
                    , pathParams = ElmAdmin.Router.parsePathParams p.path
                    , hidden = \_ _ -> False
                    , title = \_ _ -> title
                    , disabled = \_ _ -> False
                    }
                , items = items
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
    -> Page model msg
    -> List (NavigationItem model msg)
    -> NavigationItem model msg
folderGroup title ((Page main_) as main__) items_ =
    let
        paths_ : List (NavigationItem model msg) -> Set String -> Set String
        paths_ items__ acc_ =
            List.foldl
                (\item acc ->
                    case item of
                        External _ _ ->
                            acc

                        Single p ->
                            Set.insert (pathToString p.page.path) acc

                        Url p ->
                            Set.insert (pathToString p.path) acc

                        Group { main, items } ->
                            Set.insert (pathToString main.page.path) acc
                                |> paths_ items

                        VisualGroup { items } ->
                            paths_ items acc
                )
                acc_
                items__

        paths =
            paths_ items_ (Set.fromList [ pathToString main_.path ])
    in
    group
        title
        main__
        [ visualGroup "" items_
            |> hidden
                (\{ path } _ -> not (Set.member path paths))
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
        External _ _ ->
            item

        Url p ->
            Url { p | disabled = sumDisabled p.disabled }

        Single i ->
            let
                page_ : ElmAdmin.Model.Page model msg
                page_ =
                    i.page
            in
            Single
                { i
                    | disabled = sumDisabled i.disabled
                    , page =
                        { page_
                            | disabled = sumDisabled page_.disabled
                        }
                }

        Group { main, items } ->
            let
                page_ : ElmAdmin.Model.Page model msg
                page_ =
                    main.page
            in
            Group
                { main =
                    { main
                        | disabled = sumDisabled main.disabled
                        , page =
                            { page_
                                | disabled = sumDisabled page_.disabled
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


{-| Dynamic nav link label.

    single "User" User.show
        |> dynamic
            (\{ pathParams } model ->
                Dict.get ":userId" pathParams
                    |> Maybe.andThen (username model)
            )

-}
dynamic : (RouteParams -> model -> String) -> NavigationItem model msg -> NavigationItem model msg
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

    usersIndex : Page model msg
    usersIndex =
        viewPage "/users" "All users"
            (\routeParams model ->
                div [] [ ... ]
            )

-}
page :
    String
    -> String
    -> (RouteParams -> model -> Html msg)
    -> Page model msg
page title path view =
    Page
        { path = pathFromString path
        , title = \_ _ -> title
        , init = \_ model -> ( model, Cmd.none )
        , update = \_ _ model -> ( model, Cmd.none )
        , subscriptions = \_ _ -> Sub.none
        , view = view
        , disabled = \_ _ -> False
        }


{-| Creates a page with it's own Elm architecture.

  - Your `init` will run every time the page gets initialized.

  - Your `update` will run _after_ your main update (and any commands will be batched).

  - Your `subscriptions` will be active only while this page is active.

```
usersIndex : Page model msg
usersIndex =
    fullPage "/users"
        { title = \routeParams model -> ...
        , init = \routeParams model -> ...
        , update = \routeParams model -> ...
        , subscriptions = \routeParams model -> ...
        , view = \routeParams model -> ...
        }
```

-}
fullPage :
    String
    ->
        { title : RouteParams -> model -> String
        , init : RouteParams -> model -> ( model, Cmd msg )
        , update : RouteParams -> msg -> model -> ( model, Cmd msg )
        , subscriptions : RouteParams -> model -> Sub msg
        , view : RouteParams -> model -> Html msg
        }
    -> Page model msg
fullPage path props =
    Page
        { path = pathFromString path
        , title = props.title
        , init = props.init
        , update = props.update
        , subscriptions = props.subscriptions
        , view = props.view
        , disabled = \_ _ -> False
        }


{-| Creates a `fullPage` with quick access to a "resource".

    usersShow : Page model msg
    usersShow =
        fullPage "/users/:userId"
            { resource = \{ pathParams } model ->
                pathParams
                    |> Dict.get ":userId"
                    |> Maybe.andThen (getUser model)
            , title = \routeParams model user ->
                user
                    |> Maybe.map .name
                    |> Maybe.withDefault "…"
            , init = \routeParams model user -> ...
            , update = \routeParams model user -> ...
            , subscriptions = \routeParams model user -> ...
            , view = \routeParams model user -> ...
            }

-}
resourcePage :
    String
    ->
        { resource : RouteParams -> model -> resource
        , title : RouteParams -> model -> resource -> String
        , init : RouteParams -> model -> resource -> ( model, Cmd msg )
        , update : RouteParams -> msg -> model -> resource -> ( model, Cmd msg )
        , subscriptions : RouteParams -> model -> resource -> Sub msg
        , view : RouteParams -> model -> resource -> Html msg
        }
    -> Page model msg
resourcePage path props =
    Page
        { path =
            pathFromString path
        , disabled =
            \_ _ -> False
        , title =
            \params_ model ->
                props.title params_ model (props.resource params_ model)
        , init =
            \params_ model ->
                props.init params_ model (props.resource params_ model)
        , update =
            \params_ msg model ->
                props.update params_ msg model (props.resource params_ model)
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
        { preferDarkMode = False
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
        { title = "My Admin"
        , init = init
        , update = update
        , subscriptions = subscriptions
        , options = [ preferDarkMode ]
        , pages =
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
        }

-}
admin :
    { title : String
    , init : flags -> Browser.Navigation.Key -> ( model, Cmd msg )
    , update : RouteParams -> msg -> model -> ( model, Cmd msg )
    , subscriptions : RouteParams -> model -> Sub msg
    , options : List (Options -> Options)
    , pages : List (NavigationItem model msg)
    }
    -> ElmAdmin flags model msg
admin props =
    let
        options : OptionsData
        options =
            List.foldl (\fn a -> fn a) defaultOptions props.options
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
            pagesDataList_ props.pages

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
            viewNavItem_ props.pages
    in
    Browser.application
        { onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        , init =
            ElmAdmin.Model.init
                { pageRoutes = pageRoutes
                , initModel = props.init
                , preferDarkMode = options.preferDarkMode
                }
        , update = update props.update pages pageRoutes
        , subscriptions = subscriptions props.subscriptions pages
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
