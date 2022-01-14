module ElmAdmin.RouteParams exposing
    ( RouteParams
    , empty
    )

import Dict exposing (Dict)


type alias RouteParams =
    { pathParams : Dict String String
    , queryParams : Dict String (List String)
    }


empty : RouteParams
empty =
    { pathParams = Dict.empty
    , queryParams = Dict.empty
    }
