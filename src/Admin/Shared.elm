module Admin.Shared exposing
    ( Action
    , Effect(..)
    , Msg(..)
    , mapAction
    )

import Admin.Internal.Form exposing (FieldValue)
import Browser exposing (UrlRequest)
import ElmAdmin.UI.Notification exposing (NotificationStatus)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Platform exposing (Task)
import SubCmd
import Time exposing (Posix)
import Url exposing (Url)


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
    | UpdateFormField String String FieldValue
    | SetDebounce String Posix (Msg msg)
    | UpdateDebounced Posix
    | FetchInitialAutocompleteOption String String (Task Http.Error { id : String, label : String })
    | GotInitialAutocompleteOption String String (Result Http.Error { id : String, label : String })
    | FetchAutocompleteOptions String String String (Task Http.Error (List { id : String, label : String }))
    | GotAutocompleteOptions String String String (Result Http.Error (List { id : String, label : String }))


type Effect msg
    = ShowNotification NotificationStatus (Html msg)
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

        UpdateFormField a b c ->
            UpdateFormField a b c

        UpdateDebounced a ->
            UpdateDebounced a

        FetchInitialAutocompleteOption a b c ->
            FetchInitialAutocompleteOption a b c

        GotInitialAutocompleteOption a b c ->
            GotInitialAutocompleteOption a b c

        FetchAutocompleteOptions a b c d ->
            FetchAutocompleteOptions a b c d

        GotAutocompleteOptions a b c d ->
            GotAutocompleteOptions a b c d


mapEffect : (msgA -> msgB) -> Effect msgA -> Effect msgB
mapEffect fn effect =
    case effect of
        ShowNotification status html ->
            ShowNotification status (Html.map fn html)

        Debounce label interval msg ->
            Debounce label interval (mapMsg fn msg)


mapAction : (msgA -> msgB) -> Action msgA -> Action msgB
mapAction fn action =
    SubCmd.mapBoth fn
        (\effect ->
            case effect of
                ShowNotification status html ->
                    ShowNotification status (Html.map fn html)

                Debounce label interval msg ->
                    Debounce label interval (mapMsg fn msg)
        )
        action
