module Admin.Internal.Application exposing
    ( Admin
    , init
    , initFields
    , subscriptions
    , update
    , view
    )

import Admin.Internal.NavItem
import Admin.Internal.Router exposing (RouterData)
import Admin.Shared exposing (Action, Effect(..), Msg(..))
import Browser
import Browser.Navigation
import Dict exposing (Dict)
import ElmAdmin.Internal.Form exposing (FieldValue(..), Form, FormModel)
import ElmAdmin.Styles
import ElmAdmin.UI.Nav
import ElmAdmin.UI.Notification exposing (NotificationStatus)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Html.Keyed
import Set
import SubModule
import Task
import ThemeProvider
import Time exposing (Posix)
import Url exposing (Url)
import W.Styles


type alias Admin flags model msg =
    Program flags (Model model msg) (Msg msg)


type alias Model model msg =
    { navKey : Browser.Navigation.Key
    , url : Url
    , model : model
    , routerIndex : Maybe Int
    , routerData : Maybe (RouterData model msg)
    , darkMode : Bool
    , formModel : FormModel
    , debounced : Dict String ( Posix, Msg msg )
    , notification :
        Maybe
            { id : Int
            , status : NotificationStatus
            , content : H.Html msg
            , expiration : Posix
            }
    }


init :
    { init : flags -> Browser.Navigation.Key -> ( model, Action msg )
    , update : msg -> model -> ( model, Action msg )
    , routers : List (Admin.Internal.Router.Router model msg)
    }
    -> flags
    -> Url.Url
    -> Browser.Navigation.Key
    -> ( Model model msg, Cmd (Msg msg) )
init props flags url navKey =
    let
        ( userModel, userInit ) =
            props.init flags navKey
                |> SubModule.initWithEffect
                    { toMsg = GotMsg
                    , effectToMsg = GotEffect
                    }

        routerData : Maybe (Admin.Internal.Router.RouterData model msg)
        routerData =
            Admin.Internal.Router.oneOf props.routers url userModel

        routerIndex : Maybe Int
        routerIndex =
            Admin.Internal.Router.routerIndex props.routers userModel

        initialModel : Model model msg
        initialModel =
            { navKey = navKey
            , url = url
            , model = userModel
            , routerIndex = routerIndex
            , routerData = routerData
            , darkMode = True
            , formModel = ElmAdmin.Internal.Form.empty
            , debounced = Dict.empty
            , notification = Nothing
            }
    in
    routerData
        |> Maybe.andThen (\d -> d.init userModel)
        |> Maybe.map (\msg -> update { update = props.update, routers = props.routers } msg initialModel)
        |> Maybe.withDefault ( initialModel, Cmd.none )
        |> userInit


update :
    { update : msg -> model -> ( model, Action msg )
    , routers : List (Admin.Internal.Router.Router model msg)
    }
    -> Msg msg
    -> Model model msg
    -> ( Model model msg, Cmd (Msg msg) )
update props msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        Batch msgList ->
            msgList
                |> List.foldl
                    (\msg_ ( model_, cmd ) ->
                        update props msg_ model_
                            |> Tuple.mapSecond (\cmd_ -> Cmd.batch [ cmd, cmd_ ])
                    )
                    ( model, Cmd.none )

        ToggleDarkMode ->
            ( { model | darkMode = not model.darkMode }, Cmd.none )

        OnUrlRequest request ->
            case request of
                Browser.External url ->
                    ( model, Browser.Navigation.load url )

                Browser.Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.navKey url.path
                    )

        OnUrlChange url ->
            let
                routerData : Maybe (Admin.Internal.Router.RouterData model msg)
                routerData =
                    Admin.Internal.Router.oneOf props.routers url model.model

                model_ : Model model msg
                model_ =
                    { model | routerData = routerData }
            in
            routerData
                |> Maybe.andThen (\d -> d.init model.model)
                |> Maybe.map (\msg_ -> update props msg_ model_)
                |> Maybe.withDefault ( model_, Cmd.none )

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

        UpdateFormModel fn ->
            let
                ( formModel, msg_ ) =
                    fn model.formModel

                model_ : Model model msg
                model_ =
                    { model | formModel = formModel }
            in
            update props msg_ model_

        GotMsg msg_ ->
            let
                ( model_, cmd ) =
                    props.update msg_ model.model
                        |> SubModule.updateWithEffect
                            { toModel = \m -> { model | model = m }
                            , toMsg = GotMsg
                            , effectToMsg = GotEffect
                            }

                routerIndex : Maybe Int
                routerIndex =
                    Admin.Internal.Router.routerIndex props.routers model_.model
            in
            if routerIndex /= model.routerIndex then
                ( { model_ | routerIndex = routerIndex }
                , Cmd.batch
                    [ Browser.Navigation.replaceUrl model.navKey model.url.path
                    , cmd
                    ]
                )

            else
                ( model_, cmd )

        GotEffect effect ->
            case effect of
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

                Debounce key wait msg_ ->
                    ( model
                    , Task.perform
                        (\posix ->
                            SetDebounce key (Time.millisToPosix (Time.posixToMillis posix + wait)) msg_
                        )
                        Time.now
                    )

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

        FetchInitialAutocompleteOption k task ->
            ( model, Task.attempt (GotInitialAutocompleteOption k) task )

        GotInitialAutocompleteOption k response ->
            case response of
                Ok v ->
                    update props (UpdateFormField k (FieldValueRemoteAutocomplete ( v.label, Just v ))) model

                _ ->
                    ( model, Cmd.none )

        FetchAutocompleteOptions fieldId term task ->
            ( model, Task.attempt (GotAutocompleteOptions fieldId term) task )

        GotAutocompleteOptions fieldId term response ->
            let
                formModel : FormModel
                formModel =
                    model.formModel
            in
            ( { model
                | formModel =
                    { formModel
                        | remoteAutocomplete =
                            Dict.insert ( fieldId, term ) response formModel.remoteAutocomplete
                    }
              }
            , Cmd.none
            )

        SetDebounce key posix msg_ ->
            ( { model
                | debounced =
                    Dict.insert
                        key
                        ( posix, msg_ )
                        model.debounced
              }
            , Cmd.none
            )

        UpdateDebounced now_ ->
            let
                now : Int
                now =
                    Time.posixToMillis now_

                triggered : Dict String ( Posix, Msg msg )
                triggered =
                    model.debounced
                        |> Dict.filter (\_ ( t, _ ) -> Time.posixToMillis t <= now)

                remaining =
                    Dict.diff model.debounced triggered
            in
            triggered
                |> Dict.values
                |> List.foldl
                    (\( _, msg_ ) ( model_, cmd ) ->
                        update props msg_ model_
                            |> Tuple.mapSecond (\cmd_ -> Cmd.batch [ cmd, cmd_ ])
                    )
                    ( { model | debounced = remaining }, Cmd.none )


initFields : model -> params -> resource -> Form model msg params resource -> FormModel -> ( FormModel, Msg msg )
initFields model params resource form_ formModel =
    ( { initialized = Set.insert form_.title formModel.initialized
      , remoteAutocomplete = Dict.empty
      , values =
            form_.fields
                |> List.map
                    (\( label, field ) ->
                        let
                            fieldValue =
                                case field of
                                    ElmAdmin.Internal.Form.Text { value } ->
                                        FieldValueString <| value resource

                                    ElmAdmin.Internal.Form.Autocomplete { value } ->
                                        let
                                            value_ : Maybe String
                                            value_ =
                                                value resource
                                        in
                                        FieldValueAutocomplete ( value_ |> Maybe.withDefault "", value_ )

                                    ElmAdmin.Internal.Form.RemoteAutocomplete _ ->
                                        FieldValueRemoteAutocomplete ( "", Nothing )

                                    ElmAdmin.Internal.Form.Checkbox { value } ->
                                        FieldValueBool <| value resource

                                    ElmAdmin.Internal.Form.Radio { value } ->
                                        FieldValueString <| value resource

                                    ElmAdmin.Internal.Form.Select { value } ->
                                        FieldValueString <| value resource

                                    ElmAdmin.Internal.Form.Range { value } ->
                                        FieldValueFloat <| value resource
                        in
                        ( ( form_.title, label ), fieldValue )
                    )
                |> Dict.fromList
                |> (\v_ -> Dict.union v_ formModel.values)
      }
    , form_.fields
        |> List.foldl
            (\( label, field ) acc ->
                case field of
                    ElmAdmin.Internal.Form.RemoteAutocomplete { value, initRequest } ->
                        value resource
                            |> Maybe.map
                                (\id ->
                                    initRequest model params id
                                        |> FetchInitialAutocompleteOption ( form_.title, label )
                                        |> (\m -> m :: acc)
                                )
                            |> Maybe.withDefault acc

                    _ ->
                        acc
            )
            []
        |> Batch
    )


subscriptions :
    { subscriptions : model -> Sub msg
    }
    -> Model model msg
    -> Sub (Msg msg)
subscriptions props model =
    let
        debouncedSubscriptions =
            if Dict.isEmpty model.debounced then
                Sub.none

            else
                Time.every 200 UpdateDebounced

        notificationSubscriptions =
            if model.notification /= Nothing then
                Time.every 1000 HideNotification

            else
                Sub.none
    in
    Sub.batch
        [ props.subscriptions model.model |> Sub.map GotMsg
        , notificationSubscriptions
        , debouncedSubscriptions
        ]


view :
    { title : String
    , theme :
        { lightTheme : ThemeProvider.Theme
        , darkTheme : ThemeProvider.Theme
        , darkModeStrategy : ThemeProvider.DarkModeStrategy
        , preferDarkMode : Bool
        , disableModeSwitch : Bool
        }
    }
    -> Model model msg
    -> Browser.Document (Msg msg)
view props model =
    let
        pageTitle : H.Html x
        pageTitle =
            model.routerData
                |> Maybe.andThen (\d -> d.title model.model)
                |> Maybe.withDefault ""
                |> (\s ->
                        H.h2
                            [ HA.class "eadm eadm-page-title eadm-fade" ]
                            [ H.text s ]
                   )

        pageView : H.Html (Msg msg)
        pageView =
            model.routerData
                |> Maybe.map (\d -> d.view model.formModel model.model)
                |> Maybe.withDefault (H.text "")

        activePath : String
        activePath =
            model.routerData
                |> Maybe.map .path
                |> Maybe.withDefault "/"

        navItems : List Admin.Internal.NavItem.NavItem
        navItems =
            model.routerData
                |> Maybe.andThen (\d -> d.navItems model.model)
                |> Maybe.withDefault []
    in
    { title = props.title
    , body =
        [ if props.theme.disableModeSwitch then
            if props.theme.preferDarkMode then
                ThemeProvider.globalProvider props.theme.darkTheme

            else
                ThemeProvider.globalProvider props.theme.lightTheme

          else
            ThemeProvider.globalProviderWithDarkMode
                { light = props.theme.lightTheme
                , dark = props.theme.darkTheme
                , strategy = props.theme.darkModeStrategy
                }
        , W.Styles.globalStyles
        , ElmAdmin.Styles.globalStyles
        , H.div [ HA.classList [ ( "eadm-dark", model.darkMode ) ] ]
            [ case model.notification of
                Just notification ->
                    Html.Keyed.node "div"
                        []
                        [ ( String.fromInt notification.id
                          , ElmAdmin.UI.Notification.view
                                { status = notification.status
                                , content = notification.content
                                }
                                |> H.map GotMsg
                          )
                        ]

                Nothing ->
                    H.text ""
            , H.div [ HA.class "eadm eadm-wrapper" ]
                [ H.aside [ HA.class "eadm eadm-sidebar" ]
                    [ H.header [ HA.class "eadm eadm-sidebar-header" ]
                        [ H.h1 [ HA.class "eadm eadm-title" ]
                            [ H.a
                                [ HA.class "eadm eadm-link"
                                , HA.href "/"
                                ]
                                [ H.text props.title ]
                            ]
                        , if not props.theme.disableModeSwitch && props.theme.darkModeStrategy /= ThemeProvider.SystemStrategy then
                            H.button
                                [ HA.class "eadm eadm-sidebar-dark-btn"
                                , HE.onClick ToggleDarkMode
                                ]
                                [ H.text "☀" ]

                          else
                            H.text ""
                        ]
                    , ElmAdmin.UI.Nav.view
                        { active = activePath
                        , items = navItems
                        }
                    ]
                , H.main_
                    [ HA.class "eadm eadm-main" ]
                    [ pageTitle
                    , pageView
                    ]
                ]
            ]
        ]
    }



-- validate routes
-- validate pages (?)
-- cache routes
-- cache pages
-- onRouteChange
-- find page
-- update validPages
-- initOneOf
-- onUpdate
-- nothing
-- onView
