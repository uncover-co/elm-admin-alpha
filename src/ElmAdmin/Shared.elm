module ElmAdmin.Shared exposing
    ( Effect(..)
    , ElmAdmin
    , Model
    , Msg(..)
    , SubCmd
    )

import Browser exposing (UrlRequest)
import Browser.Navigation
import ElmAdmin.Internal.Form exposing (FieldValue, FormModel)
import ElmAdmin.Router exposing (RouteParams)
import Html exposing (..)
import Html.Attributes exposing (..)
import SubCmd
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
    = DoNothing
    | ToggleDarkMode
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | GotMsg msg
    | GotEffect Effect
    | SubmitForm
    | UpdateFormField String FieldValue


type Effect
    = SetFormModel FormModel


type alias SubCmd msg =
    SubCmd.SubCmd msg Effect
