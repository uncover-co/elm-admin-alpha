module Admin.Internal.InvalidRouteData exposing (InvalidRouteData)


type alias InvalidRouteData =
    { path : String, page : String, error : String }
