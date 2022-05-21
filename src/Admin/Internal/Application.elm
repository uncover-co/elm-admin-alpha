module Admin.Internal.Application exposing
    ( Admin
    , init
    , initFields
    , subscriptions
    , update
    , view
    )

import Admin.Internal.Form exposing (FieldValue(..), Form, FormModel)
import Admin.Internal.NavItem
import Admin.Internal.Router exposing (RouterData)
import Admin.Shared exposing (Action, Effect(..), Msg(..))
import Browser
import Browser.Navigation
import Dict exposing (Dict)
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
    update_ props msg model
        |> initForms props


update_ :
    { update : msg -> model -> ( model, Action msg )
    , routers : List (Admin.Internal.Router.Router model msg)
    }
    -> Msg msg
    -> Model model msg
    -> ( Model model msg, Cmd (Msg msg) )
update_ props msg model =
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

        UpdateFormField formId field value ->
            model.routerData
                |> Maybe.andThen
                    (\page ->
                        Dict.get formId page.forms
                            |> Maybe.map
                                (\formModel ->
                                    let
                                        formModel_ : FormModel
                                        formModel_ =
                                            { formModel | values = Dict.insert field value formModel.values }
                                    in
                                    { model
                                        | routerData =
                                            Just
                                                { page
                                                    | forms = Dict.insert formId formModel_ page.forms
                                                }
                                    }
                                )
                    )
                |> Maybe.map (\m -> ( m, Cmd.none ))
                |> Maybe.withDefault ( model, Cmd.none )

        FetchInitialAutocompleteOption formId field task ->
            ( model, Task.attempt (GotInitialAutocompleteOption formId field) task )

        GotInitialAutocompleteOption formId fieldId response ->
            case response of
                Ok v ->
                    update props (UpdateFormField formId fieldId (FieldValueRemoteAutocomplete ( v.label, Just v ))) model

                _ ->
                    ( model, Cmd.none )

        FetchAutocompleteOptions formId fieldId term task ->
            ( model, Task.attempt (GotAutocompleteOptions formId fieldId term) task )

        GotAutocompleteOptions formId fieldId term response ->
            model.routerData
                |> Maybe.andThen
                    (\page ->
                        Dict.get formId page.forms
                            |> Maybe.map
                                (\formModel ->
                                    let
                                        formModel_ : FormModel
                                        formModel_ =
                                            { formModel | remoteAutocomplete = Dict.insert ( fieldId, term ) response formModel.remoteAutocomplete }
                                    in
                                    { model
                                        | routerData =
                                            Just
                                                { page
                                                    | forms = Dict.insert formId formModel_ page.forms
                                                }
                                    }
                                )
                    )
                |> Maybe.map (\m -> ( m, Cmd.none ))
                |> Maybe.withDefault ( model, Cmd.none )

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


initForms :
    { update : msg -> model -> ( model, Action msg )
    , routers : List (Admin.Internal.Router.Router model msg)
    }
    -> ( Model model msg, Cmd (Msg msg) )
    -> ( Model model msg, Cmd (Msg msg) )
initForms props ( model, cmd ) =
    model.routerData
        |> Maybe.map
            (\page ->
                if Set.isEmpty page.formLoading then
                    ( model, cmd )

                else
                    let
                        initData :
                            { formLoading : List String
                            , formValues : List ( String, Dict String FieldValue )
                            , formInitMsgs : List (Msg msg)
                            }
                        initData =
                            page.formLoading
                                |> Set.foldl
                                    (\id acc ->
                                        case
                                            Dict.get id page.formInits
                                                |> Maybe.andThen (\fn -> fn model.model)
                                        of
                                            Just { values, initMsg } ->
                                                { acc
                                                    | formValues = ( id, values ) :: acc.formValues
                                                    , formInitMsgs = initMsg :: acc.formInitMsgs
                                                }

                                            Nothing ->
                                                { acc | formLoading = id :: acc.formLoading }
                                    )
                                    { formLoading = [], formValues = [], formInitMsgs = [] }

                        forms_ : Dict String FormModel
                        forms_ =
                            initData.formValues
                                |> List.foldl
                                    (\( id, values ) acc ->
                                        Dict.insert id (Admin.Internal.Form.fromValues values) acc
                                    )
                                    page.forms

                        page_ : RouterData model msg
                        page_ =
                            { page
                                | forms = forms_
                                , formLoading =
                                    Set.fromList initData.formLoading
                            }

                        model_ : Model model msg
                        model_ =
                            { model | routerData = Just page_ }
                    in
                    if List.isEmpty initData.formInitMsgs then
                        ( model_, cmd )

                    else
                        update props (Batch initData.formInitMsgs) model_
                            |> Tuple.mapSecond (\cmd_ -> Cmd.batch [ cmd, cmd_ ])
            )
        |> Maybe.withDefault ( model, cmd )


initFields :
    model
    -> params
    -> resource
    -> String
    -> Form model msg params resource
    ->
        { values : Dict String FieldValue
        , initMsg : Msg msg
        }
initFields model params resource formId form_ =
    { values =
        form_.fields
            |> List.map
                (\( label, field ) ->
                    let
                        fieldValue =
                            case field of
                                Admin.Internal.Form.Text { value } ->
                                    FieldValueString <| value resource

                                Admin.Internal.Form.Autocomplete { value } ->
                                    let
                                        value_ : Maybe String
                                        value_ =
                                            value resource
                                    in
                                    FieldValueAutocomplete ( value_ |> Maybe.withDefault "", value_ )

                                Admin.Internal.Form.RemoteAutocomplete _ ->
                                    FieldValueRemoteAutocomplete ( "", Nothing )

                                Admin.Internal.Form.Checkbox { value } ->
                                    FieldValueBool <| value resource

                                Admin.Internal.Form.Radio { value } ->
                                    FieldValueString <| value resource

                                Admin.Internal.Form.Select { value } ->
                                    FieldValueString <| value resource

                                Admin.Internal.Form.Range { value } ->
                                    FieldValueFloat <| value resource
                    in
                    ( label, fieldValue )
                )
            |> Dict.fromList
    , initMsg =
        form_.fields
            |> List.foldl
                (\( label, field ) acc ->
                    case field of
                        Admin.Internal.Form.RemoteAutocomplete { value, initRequest } ->
                            value resource
                                |> Maybe.map
                                    (\id_ ->
                                        initRequest model params id_
                                            |> FetchInitialAutocompleteOption formId label
                                            |> (\m -> m :: acc)
                                    )
                                |> Maybe.withDefault acc

                        _ ->
                            acc
                )
                []
            |> Batch
    }


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
                |> Maybe.map (\d -> d.view d.forms model.model)
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
