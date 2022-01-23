module ElmAdmin.UI.Notification exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


type NotificationStatus
    = None
    | Highlight
    | Success
    | Warning
    | Danger


statusClass : NotificationStatus -> String
statusClass status =
    case status of
        None ->
            "eadm-m-none"

        Highlight ->
            "eadm-m-highlight"

        Success ->
            "eadm-m-success"

        Warning ->
            "eadm-m-warning"

        Danger ->
            "eadm-m-danger"


view :
    { status : NotificationStatus
    , content : Html msg
    }
    -> Html msg
view props =
    article
        [ class "eadm eadm-notification eadm-fade-slide"
        , class (statusClass props.status)
        ]
        [ div
            [ class "eadm eadm-notification-content" ]
            [ props.content ]
        ]
