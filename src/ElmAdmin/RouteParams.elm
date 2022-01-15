module ElmAdmin.RouteParams exposing
    ( RouteParams
    , empty
    )

import Dict exposing (Dict)


type alias RouteParams =
    { path : String
    , pathParams : Dict String String
    , queryParams : Dict String (List String)
    }


empty : RouteParams
empty =
    { path = "/"
    , pathParams = Dict.empty
    , queryParams = Dict.empty
    }
