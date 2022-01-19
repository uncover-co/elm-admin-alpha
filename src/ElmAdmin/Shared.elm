module ElmAdmin.Shared exposing
    ( Effect(..)
    , ElmAdmin
    , Model
    , Msg(..)
    )

import Browser exposing (UrlRequest)
import Browser.Navigation
import ElmAdmin.Form exposing (FieldValue, FormModel)
import ElmAdmin.Router exposing (RouteParams)
import Html exposing (..)
import Html.Attributes exposing (..)
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
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | ToggleDarkMode
    | GotMsg msg
    | GotEffect Effect
    | SubmitForm
    | UpdateFormField String FieldValue


type Effect
    = SetFormModel FormModel
