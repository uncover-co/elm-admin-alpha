module ElmAdmin.Model exposing
    ( ElmAdmin
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Dict exposing (Dict)
import ElmAdmin.Form exposing (FormModel)
import ElmAdmin.Page exposing (Effect(..), Page)
import ElmAdmin.Router exposing (RouteParams)
import ElmAdmin.Styles
import ElmAdmin.UI.Nav exposing (UINavItem)
import ElmWidgets
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE
import SubModule
import ThemeSpec
import Url exposing (Url)


type alias ElmAdmin flags model msg =
    Program flags (Model model) (Msg msg)


type alias Model model =
    { navKey : Browser.Navigation.Key
    , model : model
    , routeParams : RouteParams
    , darkMode : Bool
    , formModel : FormModel
    }


type Msg msg
    = OnUrlRequest UrlRequest
    | OnUrlChange Url
    | ToggleDarkMode
    | Msg msg
    | GotEffect Effect


oneOfPage :
    Dict String (List (Page model msg))
    -> Url
    -> model
    -> Maybe ( Page model msg, RouteParams )
oneOfPage pageRoutes url model =
    ElmAdmin.Router.oneOf .path pageRoutes url
        |> Maybe.andThen
            (\( p, routeParams_ ) ->
                if p.disabled routeParams_ model then
                    Nothing

                else
                    Just ( p, routeParams_ )
            )


enabledPage : RouteParams -> model -> Page model msg -> Maybe (Page model msg)
enabledPage routeParams model page =
    if page.disabled routeParams model then
        Nothing

    else
        Just page


init :
    { init : flags -> Browser.Navigation.Key -> ( model, Cmd msg )
    , pageRoutes : Dict String (List (Page model msg))
    , protectedPageRoutes : Dict String (List (Page protectedModel msg))
    , protectedModel : model -> Maybe protectedModel
    , protectedToModel : model -> protectedModel -> model
    , preferDarkMode : Bool
    }
    -> flags
    -> Url
    -> Browser.Navigation.Key
    -> ( Model model, Cmd (Msg msg) )
init props flags url navKey =
    let
        ( initialModel_, initialCmd ) =
            props.init flags navKey

        ( initialModel, initPageCmd, activeRouteParams ) =
            case props.protectedModel initialModel_ of
                Just protectedModel ->
                    let
                        activePage : Maybe ( Page protectedModel msg, RouteParams )
                        activePage =
                            oneOfPage props.protectedPageRoutes url protectedModel

                        activeRouteParams_ : Maybe RouteParams
                        activeRouteParams_ =
                            activePage
                                |> Maybe.map Tuple.second
                    in
                    activePage
                        |> Maybe.map
                            (\( p, r ) ->
                                p.init r protectedModel
                                    |> SubModule.initWithEffect
                                        { toMsg = Msg
                                        , effectToMsg = GotEffect
                                        }
                            )
                        |> Maybe.withDefault ( protectedModel, identity )
                        |> (\( protectedModel_, initPageCmd_ ) ->
                                ( props.protectedToModel initialModel_ protectedModel_
                                , initPageCmd_
                                , activeRouteParams_
                                )
                           )

                Nothing ->
                    let
                        activePage : Maybe ( Page model msg, RouteParams )
                        activePage =
                            oneOfPage props.pageRoutes url initialModel_

                        activeRouteParams_ : Maybe RouteParams
                        activeRouteParams_ =
                            activePage
                                |> Maybe.map Tuple.second
                    in
                    activePage
                        |> Maybe.map
                            (\( p, r ) ->
                                p.init r initialModel_
                                    |> SubModule.initWithEffect
                                        { toMsg = Msg
                                        , effectToMsg = GotEffect
                                        }
                            )
                        |> Maybe.withDefault ( initialModel_, identity )
                        |> (\( initialModel__, cmd_ ) ->
                                ( initialModel__
                                , cmd_
                                , activeRouteParams_
                                )
                           )

        routeParams =
            activeRouteParams
                |> Maybe.withDefault ElmAdmin.Router.emptyRouteParams

        adminCmd =
            if activeRouteParams == Nothing && url.path /= "/" then
                Browser.Navigation.replaceUrl navKey "/"

            else
                Cmd.none
    in
    ( { navKey = navKey
      , model = initialModel
      , formModel = ElmAdmin.Form.empty
      , routeParams = routeParams
      , darkMode = props.preferDarkMode
      }
    , Cmd.batch
        [ Cmd.map Msg initialCmd
        , adminCmd
        ]
    )
        |> initPageCmd


update :
    { update : RouteParams -> msg -> model -> ( model, Cmd msg )
    , pages : Dict String (Page model msg)
    , protectedPages : Dict String (Page protectedModel msg)
    , pageRoutes : Dict String (List (Page model msg))
    , protectedPageRoutes : Dict String (List (Page protectedModel msg))
    , protectedModel : model -> Maybe protectedModel
    , protectedToModel : model -> protectedModel -> model
    }
    -> Msg msg
    -> Model model
    -> ( Model model, Cmd (Msg msg) )
update props msg model =
    let
        protectedModel =
            props.protectedModel model.model
    in
    case msg of
        OnUrlRequest request ->
            case request of
                External url ->
                    ( model, Browser.Navigation.load url )

                Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl
                        model.navKey
                        (Url.toString url)
                    )

        OnUrlChange url ->
            let
                activeRoute =
                    case protectedModel of
                        Just protectedModel_ ->
                            oneOfPage props.protectedPageRoutes url protectedModel_
                                |> Maybe.map
                                    (\( page, routeParams ) ->
                                        let
                                            ( m, cmd ) =
                                                enabledPage routeParams protectedModel_ page
                                                    |> Maybe.map
                                                        (\p ->
                                                            p.init routeParams protectedModel_
                                                                |> SubModule.initWithEffect
                                                                    { toMsg = Msg
                                                                    , effectToMsg = GotEffect
                                                                    }
                                                        )
                                                    |> Maybe.withDefault ( protectedModel_, identity )
                                        in
                                        ( props.protectedToModel model.model m, cmd, routeParams )
                                    )

                        Nothing ->
                            oneOfPage props.pageRoutes url model.model
                                |> Maybe.map
                                    (\( page, routeParams ) ->
                                        let
                                            ( m, initPage ) =
                                                enabledPage routeParams model.model page
                                                    |> Maybe.map
                                                        (\p ->
                                                            p.init routeParams model.model
                                                                |> SubModule.initWithEffect
                                                                    { toMsg = Msg
                                                                    , effectToMsg = GotEffect
                                                                    }
                                                        )
                                                    |> Maybe.withDefault ( model.model, identity )
                                        in
                                        ( m, initPage, routeParams )
                                    )
            in
            case activeRoute of
                Just ( model_, initPage, routeParams ) ->
                    ( { model
                        | model = model_
                        , routeParams = routeParams
                        , formModel = ElmAdmin.Form.empty
                      }
                    , Cmd.none
                    )
                        |> initPage

                Nothing ->
                    if url.path == "/" then
                        ( { model
                            | routeParams = ElmAdmin.Router.emptyRouteParams
                            , formModel = ElmAdmin.Form.empty
                          }
                        , Cmd.none
                        )

                    else
                        ( { model | formModel = ElmAdmin.Form.empty }
                        , Browser.Navigation.pushUrl model.navKey "/"
                        )

        ToggleDarkMode ->
            ( { model | darkMode = not model.darkMode }, Cmd.none )

        Msg msg_ ->
            let
                -- First we update the user model using the global update
                ( model_, cmd ) =
                    props.update model.routeParams msg_ model.model
                        |> Tuple.mapSecond (Cmd.map Msg)

                -- Then we check if we should initialize the current active page
                -- There are a few different reasons this should be triggered:
                ( model__, pageInitCmd, formModel ) =
                    case ( protectedModel, props.protectedModel model_ ) of
                        -- if the app changes from unprotected to protected
                        -- and the user already was in a valid route
                        ( Nothing, Just protectedModel_ ) ->
                            Dict.get model.routeParams.path props.protectedPages
                                |> Maybe.andThen (enabledPage model.routeParams protectedModel_)
                                |> Maybe.map
                                    (\page ->
                                        page.init model.routeParams protectedModel_
                                            |> SubModule.initWithEffect
                                                { toMsg = Msg
                                                , effectToMsg = GotEffect
                                                }
                                    )
                                |> Maybe.map
                                    (\( m, c ) ->
                                        ( props.protectedToModel model_ m, c, ElmAdmin.Form.empty )
                                    )
                                |> Maybe.withDefault ( model_, identity, model.formModel )

                        -- if the app changes from protected to unprotected
                        -- and the user already was in a valid route
                        ( Just _, Nothing ) ->
                            Dict.get model.routeParams.path props.pages
                                |> Maybe.andThen (enabledPage model.routeParams model_)
                                |> Maybe.map
                                    (\page ->
                                        page.init model.routeParams model_
                                            |> SubModule.initWithEffect
                                                { toMsg = Msg
                                                , effectToMsg = GotEffect
                                                }
                                    )
                                |> Maybe.map (\( m, c ) -> ( m, c, ElmAdmin.Form.empty ))
                                |> Maybe.withDefault ( model_, identity, model.formModel )

                        -- if the user is in a protected state
                        -- and two pages with the same route exists (one enabled and one disabled)
                        -- and the user switches from one to the other
                        ( Just previousProtectedModel_, Just protectedModel_ ) ->
                            case Dict.get model.routeParams.path props.protectedPages of
                                Just page ->
                                    if page.disabled model.routeParams previousProtectedModel_ && not (page.disabled model.routeParams protectedModel_) then
                                        page.init model.routeParams protectedModel_
                                            |> SubModule.initWithEffect
                                                { toMsg = Msg
                                                , effectToMsg = GotEffect
                                                }
                                            |> (\( m, c ) ->
                                                    ( props.protectedToModel model_ m, c, ElmAdmin.Form.empty )
                                               )

                                    else
                                        ( model_, identity, model.formModel )

                                Nothing ->
                                    ( model_, identity, model.formModel )

                        -- if the user is in an unprotected state
                        -- and two pages with the same route exists (one enabled and one disabled)
                        -- and the user switches from one to the other
                        ( Nothing, Nothing ) ->
                            case Dict.get model.routeParams.path props.pages of
                                Just page ->
                                    if page.disabled model.routeParams model.model && not (page.disabled model.routeParams model_) then
                                        page.init model.routeParams model_
                                            |> SubModule.initWithEffect
                                                { toMsg = Msg
                                                , effectToMsg = GotEffect
                                                }
                                            |> (\( m, c ) -> ( m, c, ElmAdmin.Form.empty ))

                                    else
                                        ( model_, identity, model.formModel )

                                Nothing ->
                                    ( model_, identity, model.formModel )

                ( model___, pageCmd ) =
                    case props.protectedModel model__ of
                        Just protectedModel_ ->
                            Dict.get model.routeParams.path props.protectedPages
                                |> Maybe.andThen (enabledPage model.routeParams protectedModel_)
                                |> Maybe.map
                                    (\page ->
                                        page.update formModel model.routeParams msg_ protectedModel_
                                            |> SubModule.updateWithEffect
                                                { toModel = props.protectedToModel model__
                                                , toMsg = Msg
                                                , effectToMsg = GotEffect
                                                }
                                    )
                                |> Maybe.withDefault ( model__, Cmd.none )

                        Nothing ->
                            Dict.get model.routeParams.path props.pages
                                |> Maybe.andThen (enabledPage model.routeParams model.model)
                                |> Maybe.map
                                    (\page ->
                                        page.update formModel model.routeParams msg_ model__
                                            |> SubModule.updateWithEffect
                                                { toModel = identity
                                                , toMsg = Msg
                                                , effectToMsg = GotEffect
                                                }
                                    )
                                |> Maybe.withDefault ( model__, Cmd.none )
            in
            ( { model | model = model___ }
            , Cmd.batch [ cmd, pageCmd ]
            )
                |> pageInitCmd

        GotEffect _ ->
            ( model, Cmd.none )


subscriptions :
    { subscriptions : RouteParams -> model -> Sub msg
    , pages : Dict String (Page model msg)
    , protectedPages : Dict String (Page protectedModel msg)
    , protectedModel : model -> Maybe protectedModel
    , protectedToModel : model -> protectedModel -> model
    }
    -> Model model
    -> Sub (Msg msg)
subscriptions props model =
    let
        activePageSubscriptions =
            case props.protectedModel model.model of
                Just protectedModel_ ->
                    Dict.get model.routeParams.path props.protectedPages
                        |> Maybe.andThen (enabledPage model.routeParams protectedModel_)
                        |> Maybe.map (\page -> page.subscriptions model.routeParams protectedModel_)
                        |> Maybe.withDefault Sub.none

                Nothing ->
                    Dict.get model.routeParams.path props.pages
                        |> Maybe.andThen (enabledPage model.routeParams model.model)
                        |> Maybe.map (\page -> page.subscriptions model.routeParams model.model)
                        |> Maybe.withDefault Sub.none
    in
    Sub.batch
        [ props.subscriptions model.routeParams model.model
        , activePageSubscriptions
        ]
        |> Sub.map Msg


view :
    { title : String
    , navItems : List (UINavItem model)
    , protectedNavItems : List (UINavItem protectedModel)
    , pages : Dict String (Page model msg)
    , protectedPages : Dict String (Page protectedModel msg)
    , protectedModel : model -> Maybe protectedModel
    , theme :
        { lightTheme : ThemeSpec.Theme
        , darkTheme : ThemeSpec.Theme
        , darkModeStrategy : ThemeSpec.DarkModeStrategy
        , preventDarkMode : Bool
        }
    }
    -> Model model
    -> Browser.Document (Msg msg)
view props model =
    let
        protectedModel =
            props.protectedModel model.model

        ( activePageTitle, activePageView ) =
            case protectedModel of
                Just protectedModel_ ->
                    Dict.get model.routeParams.path props.protectedPages
                        |> Maybe.andThen (enabledPage model.routeParams protectedModel_)
                        |> Maybe.map
                            (\p ->
                                ( h2
                                    [ class "eadm eadm-page-title" ]
                                    [ text (p.title model.routeParams protectedModel_)
                                    ]
                                , p.view model.formModel model.routeParams protectedModel_
                                    |> Html.map Msg
                                )
                            )
                        |> Maybe.withDefault ( text "", text "" )

                Nothing ->
                    Dict.get model.routeParams.path props.pages
                        |> Maybe.andThen (enabledPage model.routeParams model.model)
                        |> Maybe.map
                            (\p ->
                                ( h2
                                    [ class "eadm eadm-page-title" ]
                                    [ text (p.title model.routeParams model.model)
                                    ]
                                , p.view model.formModel model.routeParams model.model
                                    |> Html.map Msg
                                )
                            )
                        |> Maybe.withDefault ( text "", text "" )
    in
    { title = props.title
    , body =
        [ if props.theme.preventDarkMode then
            ThemeSpec.globalProvider props.theme.lightTheme

          else
            ThemeSpec.globalProviderWithDarkMode
                { light = props.theme.lightTheme
                , dark = props.theme.darkTheme
                , strategy = props.theme.darkModeStrategy
                }
        , ElmWidgets.globalStyles
        , ElmAdmin.Styles.globalStyles
        , div [ classList [ ( "eadm-dark", model.darkMode ) ] ]
            [ div [ class "eadm eadm-wrapper" ]
                [ aside [ class "eadm eadm-sidebar" ]
                    [ header [ class "eadm eadm-sidebar-header" ]
                        [ h1 [ class "eadm eadm-title" ]
                            [ a
                                [ class "eadm eadm-link"
                                , href "/"
                                ]
                                [ text props.title ]
                            ]
                        , if not props.theme.preventDarkMode && props.theme.darkModeStrategy /= ThemeSpec.SystemStrategy then
                            button
                                [ class "eadm eadm-sidebar-dark-btn"
                                , HE.onClick ToggleDarkMode
                                ]
                                [ text "☀" ]

                          else
                            text ""
                        ]
                    , case protectedModel of
                        Just protectedModel_ ->
                            ElmAdmin.UI.Nav.view model.routeParams protectedModel_ props.protectedNavItems

                        Nothing ->
                            ElmAdmin.UI.Nav.view model.routeParams model.model props.navItems
                    ]
                , main_ [ class "eadm eadm-main" ]
                    [ activePageTitle
                    , activePageView
                    ]
                ]
            ]
        ]
    }
