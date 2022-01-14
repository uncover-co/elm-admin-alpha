module ElmAdmin.Navigation exposing
    ( NavItemData
    , NavigationItem(..)
    )

import ElmAdmin.Model
import ElmAdmin.Router exposing (RouteParams)


type NavigationItem model msg
    = External String String
    | Single (NavItemData model msg)
    | Group
        { main : NavItemData model msg
        , items : List (NavigationItem model msg)
        }


type alias NavItemData model msg =
    { page : ElmAdmin.Model.Page model msg
    , pathParams : List String
    , hidden : RouteParams -> model -> Bool
    , title : RouteParams -> model -> String
    }
