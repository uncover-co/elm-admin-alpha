module ElmAdmin.Application exposing
    ( init
    , initError
    , subscriptions
    , update
    , view
    )

import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Dict exposing (Dict)
import ElmAdmin.Internal.Form
import ElmAdmin.Internal.Page exposing (Route)
import ElmAdmin.Router exposing (RouteParams)
import ElmAdmin.Shared exposing (Action, Effect(..), Model, Msg(..))
import ElmAdmin.Styles
import ElmAdmin.UI.Nav exposing (UINavItem)
import ElmAdmin.UI.Notification
import ElmWidgets
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE
import Html.Keyed
import SubModule
import Task
import ThemeSpec
import Time
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
            , initCmd : ( Model model msg, Cmd (Msg msg) ) -> ( Model model msg, Cmd (Msg msg) )
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
    { init : flags -> Browser.Navigation.Key -> ( model, Action msg )
    , pageRoutes : Dict String (List (Route model msg))
    , protectedPageRoutes : Dict String (List (Route protectedModel msg))
    , protectedModel : model -> Maybe protectedModel
    , protectedToModel : model -> protectedModel -> model
    , preferDarkMode : Bool
    }
    -> flags
    -> Url
    -> Browser.Navigation.Key
    -> ( Model model msg, Cmd (Msg msg) )
init props flags url navKey =
    let
        initialFormModel =
            ElmAdmin.Internal.Form.empty

        ( initialModel, initialCmd ) =
            props.init flags navKey
                |> SubModule.initWithEffect
                    { toMsg = GotMsg
                    , effectToMsg = GotEffect
                    }
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
              , formModel = initialFormModel
              , routeParams = routeInit.routeParams
              , notification = Nothing
              , darkMode = props.preferDarkMode
              }
            , Cmd.none
            )
                |> routeInit.initCmd
                |> initialCmd

        Nothing ->
            ( { navKey = navKey
              , model = initialModel
              , formModel = initialFormModel
              , routeParams = ElmAdmin.Router.emptyRouteParams
              , notification = Nothing
              , darkMode = props.preferDarkMode
              }
            , Cmd.batch
                [ if url.path /= "/" then
                    Browser.Navigation.replaceUrl navKey "/"

                  else
                    Cmd.none
                ]
            )
                |> initialCmd


initError : (flags -> Browser.Navigation.Key -> ( model, Action msg )) -> flags -> Url -> Browser.Navigation.Key -> ( Model model msg, Cmd (Msg msg) )
initError initModel flags _ key =
    ( { navKey = key
      , model = initModel flags key |> Tuple.first
      , routeParams = ElmAdmin.Router.emptyRouteParams
      , notification = Nothing
      , darkMode = True
      , formModel = ElmAdmin.Internal.Form.empty
      }
    , Cmd.none
    )


update :
    { update : RouteParams -> msg -> model -> ( model, Action msg )
    , pages : Dict String (Route model msg)
    , protectedPages : Dict String (Route protectedModel msg)
    , pageRoutes : Dict String (List (Route model msg))
    , protectedPageRoutes : Dict String (List (Route protectedModel msg))
    , protectedModel : model -> Maybe protectedModel
    , protectedToModel : model -> protectedModel -> model
    }
    -> Msg msg
    -> Model model msg
    -> ( Model model msg, Cmd (Msg msg) )
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
                        , formModel = ElmAdmin.Internal.Form.empty
                      }
                    , Cmd.none
                    )
                        |> routeInit.initCmd

                Nothing ->
                    ( { model
                        | routeParams = ElmAdmin.Router.emptyRouteParams
                        , formModel = ElmAdmin.Internal.Form.empty
                      }
                    , if url.path /= "/" then
                        Browser.Navigation.pushUrl model.navKey "/"

                      else
                        Cmd.none
                    )

        HideNotification posix ->
            case model.notification of
                Just notification ->
                    if Time.posixToMillis notification.expiration < Time.posixToMillis posix then
                        ( { model | notification = Nothing }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetNotificationExpiration posix ->
            ( { model
                | notification =
                    model.notification
                        |> Maybe.map
                            (\notification -> { notification | expiration = posix })
              }
            , Cmd.none
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

                protectedStuff =
                    props.protectedModel model_
                        |> Maybe.andThen
                            (\protectedModel_ ->
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
                            )

                ( model__, routeCmd ) =
                    case protectedStuff of
                        Just protectedStuff_ ->
                            protectedStuff_

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
                UpdateFormModel fn ->
                    ( { model | formModel = fn model.formModel }, Cmd.none )

                ShowNotification status content ->
                    ( { model
                        | notification =
                            let
                                id_ =
                                    case model.notification of
                                        Just { id } ->
                                            id + 1

                                        Nothing ->
                                            0
                            in
                            Just
                                { id = id_
                                , status = status
                                , content = content
                                , expiration = Time.millisToPosix 0
                                }
                      }
                    , Task.perform
                        (\posix ->
                            posix
                                |> Time.posixToMillis
                                |> (+) 3000
                                |> Time.millisToPosix
                                |> SetNotificationExpiration
                        )
                        Time.now
                    )

        SubmitForm ->
            let
                routeStuff =
                    case props.protectedModel model.model of
                        Just protectedModel_ ->
                            routeFromPath model.routeParams protectedModel_ props.protectedPages
                                |> Maybe.map
                                    (\r ->
                                        r.route.page.update model.formModel r.routeParams msg r.model
                                            |> SubModule.updateWithEffect
                                                { toModel = props.protectedToModel model.model
                                                , toMsg = GotMsg
                                                , effectToMsg = GotEffect
                                                }
                                    )

                        Nothing ->
                            routeFromPath model.routeParams model.model props.pages
                                |> Maybe.map
                                    (\r ->
                                        r.route.page.update model.formModel r.routeParams msg r.model
                                            |> SubModule.updateWithEffect
                                                { toModel = identity
                                                , toMsg = GotMsg
                                                , effectToMsg = GotEffect
                                                }
                                    )
            in
            routeStuff
                |> Maybe.map (Tuple.mapFirst (\m -> { model | model = m }))
                |> Maybe.withDefault ( model, Cmd.none )

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
    -> Model model msg
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

        notificationSubscriptions =
            if model.notification /= Nothing then
                Time.every 1000 HideNotification

            else
                Sub.none
    in
    Sub.batch
        [ props.subscriptions model.routeParams model.model |> Sub.map GotMsg
        , activePageSubscriptions
        , notificationSubscriptions
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
        , preferDarkMode : Bool
        , disableModeSwitch : Bool
        }
    }
    -> Model model msg
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
        [ if props.theme.disableModeSwitch then
            if props.theme.preferDarkMode then
                ThemeSpec.globalProvider props.theme.darkTheme

            else
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
            [ case model.notification of
                Just notification ->
                    Html.Keyed.node "div"
                        []
                        [ ( String.fromInt notification.id
                          , ElmAdmin.UI.Notification.view
                                { status = notification.status
                                , content = notification.content
                                }
                                |> Html.map GotMsg
                          )
                        ]

                Nothing ->
                    text ""
            , div [ class "eadm eadm-wrapper" ]
                [ aside [ class "eadm eadm-sidebar" ]
                    [ header [ class "eadm eadm-sidebar-header" ]
                        [ h1 [ class "eadm eadm-title" ]
                            [ a
                                [ class "eadm eadm-link"
                                , href "/"
                                ]
                                [ text props.title ]
                            ]
                        , if not props.theme.disableModeSwitch && props.theme.darkModeStrategy /= ThemeSpec.SystemStrategy then
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
