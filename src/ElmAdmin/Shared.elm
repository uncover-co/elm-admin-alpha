module ElmAdmin.Shared exposing
    ( Action
    , Effect(..)
    , ElmAdmin
    , Model
    , Msg(..)
    , mapAction
    )

import Browser exposing (UrlRequest)
import Browser.Navigation
import Dict exposing (Dict)
import ElmAdmin.Internal.Form exposing (FieldValue, FormModel)
import ElmAdmin.Router exposing (RouteParams)
import ElmAdmin.UI.Notification exposing (NotificationStatus)
import Html exposing (..)
import Html.Attributes exposing (..)
import SubCmd
import Time exposing (Posix)
import Url exposing (Url)


type alias ElmAdmin flags model msg =
    Program flags (Model model msg) (Msg msg)


type alias Model model msg =
    { navKey : Browser.Navigation.Key
    , model : model
    , routeParams : RouteParams
    , darkMode : Bool
    , formModel : FormModel
    , debounced : Dict String ( Posix, Msg msg )
    , notification :
        Maybe
            { id : Int
            , status : NotificationStatus
            , content : Html msg
            , expiration : Posix
            }
    }


type Msg msg
    = DoNothing
    | Batch (List (Msg msg))
    | ToggleDarkMode
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | GotMsg msg
    | GotEffect (Effect msg)
    | HideNotification Posix
    | SetNotificationExpiration Posix
    | UpdateFormField ( String, String ) FieldValue
    | SetDebounce String Posix (Msg msg)
    | UpdateDebounced Posix


type Effect msg
    = UpdateFormModel (FormModel -> FormModel)
    | ShowNotification NotificationStatus (Html msg)
    | Debounce String Int (Msg msg)


type alias Action msg =
    SubCmd.SubCmd msg (Effect msg)


mapMsg : (msgA -> msgB) -> Msg msgA -> Msg msgB
mapMsg fn msg =
    case msg of
        -- Msg related
        Batch msgs ->
            msgs
                |> List.map (mapMsg fn)
                |> Batch

        GotEffect effect ->
            GotEffect (mapEffect fn effect)

        GotMsg msg_ ->
            GotMsg (fn msg_)

        SetDebounce label time msg_ ->
            SetDebounce label time (mapMsg fn msg_)

        -- Unrelated to Msgs
        DoNothing ->
            DoNothing

        ToggleDarkMode ->
            ToggleDarkMode

        OnUrlRequest a ->
            OnUrlRequest a

        OnUrlChange a ->
            OnUrlChange a

        HideNotification a ->
            HideNotification a

        SetNotificationExpiration a ->
            SetNotificationExpiration a

        UpdateFormField a b ->
            UpdateFormField a b

        UpdateDebounced a ->
            UpdateDebounced a


mapEffect : (msgA -> msgB) -> Effect msgA -> Effect msgB
mapEffect fn effect =
    case effect of
        UpdateFormModel fn_ ->
            UpdateFormModel fn_

        ShowNotification status html ->
            ShowNotification status (Html.map fn html)

        Debounce label interval msg ->
            Debounce label interval (mapMsg fn msg)


mapAction : (msgA -> msgB) -> Action msgA -> Action msgB
mapAction fn action =
    SubCmd.mapBoth fn
        (\effect ->
            case effect of
                UpdateFormModel fn_ ->
                    UpdateFormModel fn_

                ShowNotification status html ->
                    ShowNotification status (Html.map fn html)

                Debounce label interval msg ->
                    Debounce label interval (mapMsg fn msg)
        )
        action
