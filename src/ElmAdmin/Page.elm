module ElmAdmin.Page exposing (Effect(..), Page)

import ElmAdmin.Form exposing (FormModel)
import ElmAdmin.Router exposing (RouteParams)
import Html exposing (..)
import SubCmd exposing (SubCmd)


type alias Page model msg =
    { path : List String
    , title : RouteParams -> model -> String
    , init : RouteParams -> model -> ( model, SubCmd msg Effect )
    , update : FormModel -> RouteParams -> msg -> model -> ( model, SubCmd msg Effect )
    , subscriptions : RouteParams -> model -> Sub msg
    , view : FormModel -> RouteParams -> model -> Html msg
    , disabled : RouteParams -> model -> Bool
    }


type Effect
    = None
