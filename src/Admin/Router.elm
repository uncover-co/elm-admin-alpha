module Admin.Router exposing
    ( route, routeLink, external, Route
    , resource
    , hidden
    )

{-|

@docs route, routeLink, external, Route
@docs resource
@docs hidden

-}

import Admin.Internal.Page exposing (Page)
import Admin.Internal.Router exposing (Route(..))
import Admin.Libs.Router exposing (RouteParams)


{-| -}
type alias Route model msg =
    Admin.Internal.Router.Route model msg



-- Options


{-| -}
type Option model msg
    = Attribute (Options model -> Options model)


type alias Options model =
    { hidden : RouteParams -> model -> Bool
    }


defaultAttrs : Options model
defaultAttrs =
    { hidden = \_ _ -> False
    }


applyAttrs : List (Option model msg) -> Options model
applyAttrs attrs =
    List.foldl (\(Attribute fn) a -> fn a) defaultAttrs attrs


{-| -}
hidden : (RouteParams -> model -> Bool) -> Option model msg
hidden v =
    Attribute <| \attrs -> { attrs | hidden = v }



-- Creating Routes


{-| -}
external : { url : String, label : String } -> Route model msg
external =
    External


{-| -}
routeLink : (RouteParams -> model -> { label : String, url : String }) -> Route model msg
routeLink =
    InternalLink


{-| -}
route :
    String
    -> { page : Page model msg params, options : List (Option model msg) }
    -> List (Route model msg)
    -> Route model msg
route path_ props subRoutes =
    let
        options : Options model
        options =
            applyAttrs props.options

        path : String
        path =
            Admin.Libs.Router.normalizePath path_
    in
    Internal
        { path = path
        , page = Admin.Internal.Page.toPageData props.page
        , hidden = options.hidden
        , subRoutes = List.map (appendPath path) subRoutes
        }


{-| -}
resource :
    String
    ->
        { param : String
        , index : Page model msg params
        , show : Page model msg params
        , new : Page model msg params
        , edit : Page model msg params
        , options : List (Option model msg)
        }
    -> List (Route model msg)
    -> Route model msg
resource path props subRoutes =
    route path
        { page = props.index
        , options = props.options
        }
        [ route (path ++ "/new")
            { page = props.new
            , options = []
            }
            []
        , route (path ++ "/" ++ props.param ++ "/edit")
            { page = props.edit
            , options = []
            }
            []
        , route (path ++ "/" ++ props.param)
            { page = props.show
            , options = []
            }
            subRoutes
        ]


appendPath : String -> Route model msg -> Route model msg
appendPath path route_ =
    case route_ of
        External _ ->
            route_

        InternalLink _ ->
            route_

        Internal r ->
            Internal
                { r
                    | path = path ++ r.path
                    , subRoutes = List.map (appendPath path) r.subRoutes
                }
