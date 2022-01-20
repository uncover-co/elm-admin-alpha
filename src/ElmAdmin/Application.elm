module ElmAdmin.Application exposing
    ( init
    , subscriptions
    , update
    , view
    )

import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Dict exposing (Dict)
import ElmAdmin.Form
import ElmAdmin.Page exposing (Route)
import ElmAdmin.Router exposing (RouteParams)
import ElmAdmin.Shared exposing (Effect(..), Model, Msg(..), SubCmd)
import ElmAdmin.Styles
import ElmAdmin.UI.Nav exposing (UINavItem)
import ElmWidgets
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE
import SubModule
import ThemeSpec
import Url exposing (Url)


routeFromUrl :
    Dict String (List (Route model msg))
    -> Url
    -> Maybe model
    ->
        Maybe
            { route : Route model msg
            , routeParams : RouteParams
            , model : model
            }
routeFromUrl pageRoutes url maybeModel =
    maybeModel
        |> Maybe.andThen
            (\model ->
                ElmAdmin.Router.oneOf .pathList pageRoutes url
                    |> Maybe.andThen
                        (\( route, routeParams ) ->
                            validateEnabled routeParams model route
                        )
            )


routeFromPath :
    RouteParams
    -> model
    -> Dict String (Route model msg)
    ->
        Maybe
            { route : Route model msg
            , routeParams : RouteParams
            , model : model
            }
routeFromPath routeParams model routes =
    Dict.get routeParams.path routes
        |> Maybe.andThen (validateEnabled routeParams model)


validateEnabled :
    RouteParams
    -> model
    -> Route model msg
    ->
        Maybe
            { route : Route model msg
            , routeParams : RouteParams
            , model : model
            }
validateEnabled routeParams model route =
    if route.disabled routeParams model then
        Nothing

    else
        Just
            { route = route
            , routeParams = routeParams
            , model = model
            }


routeInitFromUrl :
    { url : Url
    , model : model
    , pageRoutes : Dict String (List (Route model msg))
    , protectedPageRoutes : Dict String (List (Route protectedModel msg))
    , protectedModel : model -> Maybe protectedModel
    , protectedToModel : model -> protectedModel -> model
    }
    ->
        Maybe
            { model : model
            , routeParams : RouteParams
            , initCmd : ( Model model, Cmd (Msg msg) ) -> ( Model model, Cmd (Msg msg) )
            }
routeInitFromUrl props =
    let
        protectedRoute =
            routeFromUrl props.protectedPageRoutes props.url (props.protectedModel props.model)
                |> Maybe.map
                    (\r ->
                        r.route.page.init r.routeParams r.model
                            |> SubModule.initWithEffect
                                { toMsg = GotMsg
                                , effectToMsg = GotEffect
                                }
                            |> (\( protectedModel_, initCmd_ ) ->
                                    { model = props.protectedToModel props.model protectedModel_
                                    , routeParams = r.routeParams
                                    , initCmd = initCmd_
                                    }
                               )
                    )
    in
    case protectedRoute of
        Just r ->
            Just r

        Nothing ->
            routeFromUrl props.pageRoutes props.url (Just props.model)
                |> Maybe.map
                    (\r ->
                        r.route.page.init r.routeParams r.model
                            |> SubModule.initWithEffect
                                { toMsg = GotMsg
                                , effectToMsg = GotEffect
                                }
                            |> (\( model_, initCmd ) ->
                                    { model = model_
                                    , routeParams = r.routeParams
                                    , initCmd = initCmd
                                    }
                               )
                    )


init :
    { init : flags -> Browser.Navigation.Key -> ( model, Cmd msg )
    , pageRoutes : Dict String (List (Route model msg))
    , protectedPageRoutes : Dict String (List (Route protectedModel msg))
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
        ( initialModel, initialCmd ) =
            props.init flags navKey
    in
    case
        routeInitFromUrl
            { url = url
            , model = initialModel
            , pageRoutes = props.pageRoutes
            , protectedPageRoutes = props.protectedPageRoutes
            , protectedModel = props.protectedModel
            , protectedToModel = props.protectedToModel
            }
    of
        Just routeInit ->
            ( { navKey = navKey
              , model = routeInit.model
              , formModel = ElmAdmin.Form.empty
              , routeParams = routeInit.routeParams
              , darkMode = props.preferDarkMode
              }
            , Cmd.map GotMsg initialCmd
            )
                |> routeInit.initCmd

        Nothing ->
            ( { navKey = navKey
              , model = initialModel
              , formModel = ElmAdmin.Form.empty
              , routeParams = ElmAdmin.Router.emptyRouteParams
              , darkMode = props.preferDarkMode
              }
            , Cmd.batch
                [ Cmd.map GotMsg initialCmd
                , if url.path /= "/" then
                    Browser.Navigation.replaceUrl navKey "/"

                  else
                    Cmd.none
                ]
            )


update :
    { update : RouteParams -> msg -> model -> ( model, SubCmd msg )
    , pages : Dict String (Route model msg)
    , protectedPages : Dict String (Route protectedModel msg)
    , pageRoutes : Dict String (List (Route model msg))
    , protectedPageRoutes : Dict String (List (Route protectedModel msg))
    , protectedModel : model -> Maybe protectedModel
    , protectedToModel : model -> protectedModel -> model
    }
    -> Msg msg
    -> Model model
    -> ( Model model, Cmd (Msg msg) )
update props msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        ToggleDarkMode ->
            ( { model | darkMode = not model.darkMode }, Cmd.none )

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
            case
                routeInitFromUrl
                    { url = url
                    , model = model.model
                    , pageRoutes = props.pageRoutes
                    , protectedPageRoutes = props.protectedPageRoutes
                    , protectedModel = props.protectedModel
                    , protectedToModel = props.protectedToModel
                    }
            of
                Just routeInit ->
                    ( { model
                        | model = routeInit.model
                        , routeParams = routeInit.routeParams
                        , formModel = ElmAdmin.Form.empty
                      }
                    , Cmd.none
                    )
                        |> routeInit.initCmd

                Nothing ->
                    ( { model
                        | routeParams = ElmAdmin.Router.emptyRouteParams
                        , formModel = ElmAdmin.Form.empty
                      }
                    , if url.path /= "/" then
                        Browser.Navigation.pushUrl model.navKey "/"

                      else
                        Cmd.none
                    )

        GotMsg msg_ ->
            let
                ( model_, cmd ) =
                    props.update model.routeParams msg_ model.model
                        |> SubModule.updateWithEffect
                            { toModel = identity
                            , toMsg = GotMsg
                            , effectToMsg = GotEffect
                            }

                routeStuff =
                    case props.protectedModel model_ of
                        Just protectedModel_ ->
                            routeFromPath model.routeParams protectedModel_ props.protectedPages
                                |> Maybe.map
                                    (\r ->
                                        r.route.page.update model.formModel r.routeParams msg r.model
                                            |> SubModule.updateWithEffect
                                                { toModel = props.protectedToModel model_
                                                , toMsg = GotMsg
                                                , effectToMsg = GotEffect
                                                }
                                    )

                        Nothing ->
                            routeFromPath model.routeParams model_ props.pages
                                |> Maybe.map
                                    (\r ->
                                        r.route.page.update model.formModel r.routeParams msg r.model
                                            |> SubModule.updateWithEffect
                                                { toModel = identity
                                                , toMsg = GotMsg
                                                , effectToMsg = GotEffect
                                                }
                                    )

                ( model__, routeCmd ) =
                    routeStuff
                        |> Maybe.withDefault
                            ( model_
                            , Browser.Navigation.pushUrl model.navKey "/"
                            )
            in
            ( { model | model = model__ }
            , Cmd.batch [ cmd, routeCmd ]
            )

        GotEffect effect ->
            case effect of
                SetFormModel formModel ->
                    ( { model | formModel = formModel }, Cmd.none )

        SubmitForm ->
            ( model, Cmd.none )

        UpdateFormField k v ->
            let
                formModel =
                    model.formModel
            in
            ( { model
                | formModel =
                    { formModel
                        | values = Dict.insert k v formModel.values
                    }
              }
            , Cmd.none
            )


subscriptions :
    { subscriptions : RouteParams -> model -> Sub msg
    , pages : Dict String (Route model msg)
    , protectedPages : Dict String (Route protectedModel msg)
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
                        |> Maybe.andThen (validateEnabled model.routeParams protectedModel_)
                        |> Maybe.map (\r -> r.route.page.subscriptions model.routeParams protectedModel_)
                        |> Maybe.withDefault Sub.none

                Nothing ->
                    Dict.get model.routeParams.path props.pages
                        |> Maybe.andThen (validateEnabled model.routeParams model.model)
                        |> Maybe.map (\r -> r.route.page.subscriptions model.routeParams model.model)
                        |> Maybe.withDefault Sub.none
    in
    Sub.batch
        [ props.subscriptions model.routeParams model.model |> Sub.map GotMsg
        , activePageSubscriptions
        ]


view :
    { title : String
    , navItems : List (UINavItem model)
    , protectedNavItems : List (UINavItem protectedModel)
    , pages : Dict String (Route model msg)
    , protectedPages : Dict String (Route protectedModel msg)
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

        protectedPage =
            protectedModel
                |> Maybe.andThen
                    (\protectedModel_ ->
                        routeFromPath model.routeParams protectedModel_ props.protectedPages
                            |> Maybe.map
                                (\r ->
                                    ( h2
                                        [ class "eadm eadm-page-title" ]
                                        [ text (r.route.page.title r.routeParams r.model)
                                        ]
                                    , r.route.page.view model.formModel model.routeParams r.model
                                    )
                                )
                    )

        ( activePageTitle, activePageView ) =
            case protectedPage of
                Just p ->
                    p

                Nothing ->
                    routeFromPath model.routeParams model.model props.pages
                        |> Maybe.map
                            (\r ->
                                ( h2
                                    [ class "eadm eadm-page-title" ]
                                    [ text (r.route.page.title r.routeParams r.model)
                                    ]
                                , r.route.page.view model.formModel r.routeParams r.model
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
                    , ElmAdmin.UI.Nav.view model.routeParams model.model props.navItems
                    , protectedModel
                        |> Maybe.map
                            (\protectedModel_ ->
                                ElmAdmin.UI.Nav.view model.routeParams protectedModel_ props.protectedNavItems
                            )
                        |> Maybe.withDefault (text "")
                    ]
                , main_ [ class "eadm eadm-main" ]
                    [ activePageTitle
                    , activePageView
                    ]
                ]
            ]
        ]
    }
