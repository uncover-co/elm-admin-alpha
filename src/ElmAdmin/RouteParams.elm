module ElmAdmin.RouteParams exposing (RouteParams)

import Dict exposing (Dict)


type alias RouteParams =
    { pathParams : Dict String String
    , queryParams : Dict String (List String)
    }
