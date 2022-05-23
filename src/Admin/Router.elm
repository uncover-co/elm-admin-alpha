module Admin.Router exposing
    ( route, external, Route
    , resource
    , protected, hidden, full
    )

{-|

@docs route, external, Route
@docs resource
@docs protected, hidden, full

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
    { hidden : model -> RouteParams -> Bool
    , protected : model -> Bool
    , full : Bool
    }


defaultAttrs : Options model
defaultAttrs =
    { hidden = \_ _ -> False
    , protected = \_ -> False
    , full = False
    }


applyAttrs : List (Option model msg) -> Options model
applyAttrs attrs =
    List.foldl (\(Attribute fn) a -> fn a) defaultAttrs attrs


{-| -}
hidden : (model -> RouteParams -> Bool) -> Option model msg
hidden v =
    Attribute <| \attrs -> { attrs | hidden = v }


{-| -}
protected : (model -> Bool) -> Option model msg
protected v =
    Attribute <| \attrs -> { attrs | protected = v }


{-| -}
full : Option model msg
full =
    Attribute <| \attrs -> { attrs | full = True }



-- Creating Routes


{-| -}
external : { url : String, label : String } -> Route model msg
external =
    External


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
        , full = options.full
        , hidden = options.hidden
        , protected = options.protected
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

        Internal r ->
            Internal
                { r
                    | path = path ++ r.path
                    , subRoutes = List.map (appendPath path) r.subRoutes
                }
