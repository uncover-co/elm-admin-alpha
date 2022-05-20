module Admin.Internal.Page exposing
    ( Page(..)
    , PageData
    , toPageData
    )

import Admin.Internal.Router exposing (RouteParams)
import Admin.Shared exposing (Msg)
import ElmAdmin.Internal.Form exposing (FormModel)
import Html as H


type Page model msg params
    = Page
        { baseTitle : String
        , toParams : RouteParams -> Maybe params
        , title : model -> params -> String
        , nav : model -> params -> String
        , init : model -> params -> Maybe (Msg msg)
        , view : FormModel -> model -> params -> H.Html (Msg msg)
        }


type alias PageData model msg =
    { title : RouteParams -> model -> Maybe String
    , nav : RouteParams -> model -> Maybe String
    , init : RouteParams -> model -> Maybe (Msg msg)
    , view : FormModel -> RouteParams -> model -> H.Html (Msg msg)
    }


toPageData : Page model msg params -> PageData model msg
toPageData (Page p) =
    { title =
        \routeParams model_ ->
            p.toParams routeParams
                |> Maybe.map (\params_ -> p.title model_ params_)
    , nav =
        \routeParams model_ ->
            p.toParams routeParams
                |> Maybe.map (\params_ -> p.nav model_ params_)
    , init =
        \routeParams model_ ->
            p.toParams routeParams
                |> Maybe.andThen (\params_ -> p.init model_ params_)
    , view =
        \formModel routeParams model_ ->
            p.toParams routeParams
                |> Maybe.map (\params_ -> p.view formModel model_ params_)
                |> Maybe.withDefault (H.text "")
    }
