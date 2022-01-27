module ElmAdmin.Shared exposing
    ( Action
    , Effect(..)
    , ElmAdmin
    , Model
    , Msg(..)
    )

import Browser exposing (UrlRequest)
import Browser.Navigation
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
    | ToggleDarkMode
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | GotMsg msg
    | GotEffect (Effect msg)
    | HideNotification Posix
    | SetNotificationExpiration Posix
    | SubmitForm
    | UpdateFormField ( String, String ) FieldValue


type Effect msg
    = UpdateFormModel (FormModel -> FormModel)
    | ShowNotification NotificationStatus (Html msg)


type alias Action msg =
    SubCmd.SubCmd msg (Effect msg)
