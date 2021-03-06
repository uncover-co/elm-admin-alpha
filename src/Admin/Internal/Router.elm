module Admin.Internal.Router exposing
    ( Route(..)
    , Router
    , RouterData
    , oneOf
    , protectedRouter
    , router
    , routerIndex
    )

import Admin.Internal.Form exposing (FieldValue, FormModel)
import Admin.Internal.NavItem
import Admin.Internal.Page exposing (PageData)
import Admin.Libs.Router exposing (RouteParams)
import Admin.Shared exposing (Msg)
import Dict exposing (Dict)
import Html as H
import Set exposing (Set)
import Url exposing (Url)


type Route model msg
    = External
        { url : String
        , label : String
        }
    | Internal
        { path : String
        , page : PageData model msg
        , full : Bool
        , protected : model -> Bool
        , hidden : model -> RouteParams -> Bool
        , subRoutes : List (Route model msg)
        }


{-| -}
type Router model msg
    = Router ( model -> Bool, Url -> model -> Maybe (RouterData model msg) )


{-| -}
type alias RouterData model msg =
    { navItems : model -> Maybe (List Admin.Internal.NavItem.NavItem)
    , path : String
    , formInits : Dict String (model -> Maybe { values : Dict String FieldValue, initMsg : Msg msg })
    , formLoading : Set String
    , forms : Dict String FormModel
    , title : model -> Maybe String
    , init : model -> Maybe (Msg msg)
    , view : Dict String FormModel -> model -> H.Html (Msg msg)
    }


{-| -}
routerIndex : List (Router model msg) -> model -> Maybe Int
routerIndex routers model =
    routerIndex_ routers model 0


routerIndex_ : List (Router model msg) -> model -> Int -> Maybe Int
routerIndex_ routers model index =
    case routers of
        [] ->
            Nothing

        (Router ( isModel, _ )) :: xs ->
            if isModel model then
                Just index

            else
                routerIndex_ xs model (index + 1)


{-| -}
oneOf : List (Router model msg) -> Url -> model -> Maybe (RouterData model msg)
oneOf routers url model =
    case routers of
        [] ->
            Nothing

        (Router ( _, toRouter )) :: xs ->
            case toRouter url model of
                Just router_ ->
                    Just router_

                Nothing ->
                    oneOf xs url model


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

        router_ : Admin.Libs.Router.Router
        router_ =
            Admin.Libs.Router.fromList allRoutePaths

        allPages : Dict String (Admin.Internal.Page.PageData subModel msg)
        allPages =
            toInternalPages routes

        defaultValidRoutes : Dict String String
        defaultValidRoutes =
            Admin.Libs.Router.defaultValidRoutes router_

        defaultRouteData : RouterData model msg
        defaultRouteData =
            { path = "/"
            , forms = Dict.empty
            , formLoading = Set.empty
            , formInits = Dict.empty
            , navItems =
                \model ->
                    fromModel model
                        |> Maybe.map (toNavItems defaultValidRoutes routes Admin.Libs.Router.emptyParams)
            , title = \_ -> Nothing
            , init = \_ -> Nothing
            , view = \_ _ -> H.text ""
            }
    in
    Router
        ( \model_ -> fromModel model_ /= Nothing
        , \url model_ ->
            case ( Admin.Libs.Router.findByUrl url router_, fromModel model_ ) of
                ( Just route_, Just _ ) ->
                    let
                        params : RouteParams
                        params =
                            Admin.Libs.Router.toParams route_

                        validRoutes : Dict String String
                        validRoutes =
                            Admin.Libs.Router.validRoutes route_ router_

                        maybePage : Maybe (Admin.Internal.Page.PageData subModel msg)
                        maybePage =
                            Dict.get (Admin.Libs.Router.toPathId route_) allPages
                    in
                    maybePage
                        |> Maybe.map
                            (\page_ ->
                                let
                                    forms_ : List ( String, subModel -> Maybe { values : Dict String FieldValue, initMsg : Msg msg } )
                                    forms_ =
                                        page_.forms params
                                in
                                { path = Admin.Libs.Router.toPath route_
                                , forms = Dict.empty
                                , formLoading = Set.fromList <| List.map Tuple.first forms_
                                , formInits =
                                    forms_
                                        |> List.map (Tuple.mapSecond (\fn -> fromModel >> Maybe.andThen fn))
                                        |> Dict.fromList
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


routePaths : Route model msg -> List String
routePaths route_ =
    case route_ of
        External _ ->
            []

        Internal r ->
            r.path :: List.concatMap routePaths r.subRoutes


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

                    Internal r ->
                        Dict.get r.path validRoutes
                            |> Maybe.andThen
                                (\path ->
                                    if r.protected model || r.hidden model params then
                                        Nothing

                                    else
                                        Just <|
                                            Admin.Internal.NavItem.NavItemInternal
                                                { path = path
                                                , title =
                                                    r.page.nav params model
                                                        |> Maybe.withDefault ""
                                                , full = r.full
                                                , children =
                                                    toNavItems validRoutes r.subRoutes params model
                                                }
                                )
            )
