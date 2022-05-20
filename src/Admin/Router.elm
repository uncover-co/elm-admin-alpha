module Admin.Router exposing
    ( route, routeLink, external, Route
    , hidden
    , oneOf, router, protectedRouter, Router, RouterData
    )

{-|

@docs route, routeLink, external, Route
@docs hidden
@docs oneOf, router, protectedRouter, Router, RouterData

-}

import Admin.Internal.NavItem
import Admin.Internal.Page exposing (Page, PageData)
import Admin.Internal.Router exposing (RouteParams, validRoutes)
import Admin.Shared exposing (Msg)
import Dict exposing (Dict)
import ElmAdmin.Internal.Form exposing (FormModel)
import Html as H
import Url exposing (Url)



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
type Route model msg
    = External
        { url : String
        , label : String
        }
    | InternalLink (RouteParams -> model -> { label : String, url : String })
    | Internal
        { path : String
        , page : PageData model msg
        , hidden : RouteParams -> model -> Bool
        , subRoutes : List (Route model msg)
        }


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
            Admin.Internal.Router.normalizePath path_
    in
    Internal
        { path = path
        , page = Admin.Internal.Page.toPageData props.page
        , hidden = options.hidden
        , subRoutes = List.map (appendPath path) subRoutes
        }


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


routePaths : Route model msg -> List String
routePaths route_ =
    case route_ of
        External _ ->
            []

        InternalLink _ ->
            []

        Internal r ->
            r.path :: List.concatMap routePaths r.subRoutes



-- Creating Routers


{-| -}
oneOf : List (Router model msg) -> Url -> model -> Maybe (RouterData model msg)
oneOf routers url model =
    case routers of
        [] ->
            Nothing

        (Router toRouter) :: xs ->
            case toRouter url model of
                Just router_ ->
                    Just router_

                Nothing ->
                    oneOf xs url model



-- Router


{-| -}
type Router model msg
    = Router (Url -> model -> Maybe (RouterData model msg))


{-| -}
type alias RouterData model msg =
    { navItems : model -> Maybe (List Admin.Internal.NavItem.NavItem)
    , path : String
    , title : model -> Maybe String
    , init : model -> Maybe (Msg msg)
    , view : FormModel -> model -> H.Html (Msg msg)
    }


{-| -}
router : List (Route model msg) -> Router model msg
router =
    protectedRouter (Just << identity)


{-| -}
protectedRouter :
    (model -> Maybe subModel)
    -> List (Route subModel msg)
    -> Router model msg
protectedRouter fromModel routes =
    let
        allRoutePaths : List String
        allRoutePaths =
            List.concatMap routePaths routes

        router_ : Admin.Internal.Router.Router
        router_ =
            Admin.Internal.Router.fromList allRoutePaths

        allPages : Dict String (Admin.Internal.Page.PageData subModel msg)
        allPages =
            toInternalPages routes

        defaultValidRoutes : Dict String String
        defaultValidRoutes =
            Admin.Internal.Router.defaultValidRoutes router_

        defaultRouteData : RouterData model msg
        defaultRouteData =
            { path = "/"
            , navItems =
                \model ->
                    fromModel model
                        |> Maybe.map (toNavItems defaultValidRoutes routes Admin.Internal.Router.emptyParams)
            , title = \_ -> Nothing
            , init = \_ -> Nothing
            , view = \_ _ -> H.text ""
            }
    in
    Router
        (\url model_ ->
            case ( Admin.Internal.Router.findByUrl url router_, fromModel model_ ) of
                ( Just route_, Just _ ) ->
                    let
                        params : RouteParams
                        params =
                            Admin.Internal.Router.toParams route_

                        validRoutes : Dict String String
                        validRoutes =
                            Admin.Internal.Router.validRoutes route_ router_

                        maybePage : Maybe (Admin.Internal.Page.PageData subModel msg)
                        maybePage =
                            Dict.get (Admin.Internal.Router.toPathId route_) allPages
                    in
                    maybePage
                        |> Maybe.map
                            (\page_ ->
                                { path = Admin.Internal.Router.toPath route_
                                , navItems =
                                    \model ->
                                        fromModel model
                                            |> Maybe.map (toNavItems validRoutes routes params)
                                , title =
                                    \model ->
                                        fromModel model
                                            |> Maybe.andThen (page_.title params)
                                , init =
                                    \model ->
                                        fromModel model
                                            |> Maybe.andThen (page_.init params)
                                , view =
                                    \formModel model ->
                                        fromModel model
                                            |> Maybe.map (page_.view formModel params)
                                            |> Maybe.withDefault (H.text "")
                                }
                            )

                ( Nothing, Just _ ) ->
                    Just defaultRouteData

                _ ->
                    Nothing
        )



-- Router Helpers


toInternalPages : List (Route model msg) -> Dict String (Admin.Internal.Page.PageData model msg)
toInternalPages routes =
    toInternalPages_ routes Dict.empty


toInternalPages_ : List (Route model msg) -> Dict String (Admin.Internal.Page.PageData model msg) -> Dict String (Admin.Internal.Page.PageData model msg)
toInternalPages_ routes acc =
    routes
        |> List.foldl
            (\route_ acc_ ->
                case route_ of
                    External _ ->
                        acc_

                    InternalLink _ ->
                        acc_

                    Internal r ->
                        let
                            acc__ : Dict String (Admin.Internal.Page.PageData model msg)
                            acc__ =
                                Dict.insert r.path r.page acc_
                        in
                        toInternalPages_ r.subRoutes acc__
            )
            acc


toNavItems : Dict String String -> List (Route model msg) -> RouteParams -> model -> List Admin.Internal.NavItem.NavItem
toNavItems validRoutes routes params model =
    routes
        |> List.filterMap
            (\route_ ->
                case route_ of
                    External r ->
                        Just <| Admin.Internal.NavItem.NavItemExternal r

                    InternalLink r ->
                        Just <| Admin.Internal.NavItem.NavItemInternalLink (r params model)

                    Internal r ->
                        Dict.get r.path validRoutes
                            |> Maybe.andThen
                                (\path ->
                                    if r.hidden params model then
                                        Nothing

                                    else
                                        Just <|
                                            Admin.Internal.NavItem.NavItemInternal
                                                { path = path
                                                , title =
                                                    r.page.nav params model
                                                        |> Maybe.withDefault ""
                                                , children =
                                                    toNavItems validRoutes r.subRoutes params model
                                                }
                                )
            )
