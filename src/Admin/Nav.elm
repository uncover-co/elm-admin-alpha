module Admin.Nav exposing
    ( Route
    , Router
    , external
    , route
    )

import Admin.Internal.Router exposing (RouteParams)
import Html as H


type Page model msg
    = Page model msg


type Options model msg
    = Options model msg



-- Creating Routes


type Route model msg
    = External
        { url : String
        , label : String
        }
    | Internal
        { path : String
        , options : Options model msg
        , page : Page model msg
        , subPages : List (Route model msg)
        }


external : { url : String, label : String } -> Route model msg
external =
    External


route :
    String
    -> { page : Page model msg, options : Options model msg }
    -> List (Route model msg)
    -> Route model msg
route path_ props subPages =
    let
        path : String
        path =
            path_
                |> String.split "/"
                |> List.filter (not << String.isEmpty)
                |> String.join "/"
                |> (++) "/"
    in
    Internal
        { path = path
        , options = props.options
        , page = props.page
        , subPages = List.map (appendPath path) subPages
        }


appendPath : String -> Route model msg -> Route model msg
appendPath path route_ =
    case route_ of
        External _ ->
            route_

        Internal r ->
            Internal
                { r
                    | path = path ++ r.path
                    , subPages = List.map (appendPath path) r.subPages
                }


routePaths : Route model msg -> List String
routePaths route_ =
    case route_ of
        External _ ->
            []

        Internal r ->
            r.path :: List.concatMap routePaths r.subPages



-- Creating Routers


type Router model msg
    = Router
        { init : model -> Maybe msg
        , view : model -> Maybe (H.Html msg)
        }


type alias PageData model msg =
    { nav : Maybe (RouteParams -> model -> String)
    , title : RouteParams -> model -> String
    , init : RouteParams -> model -> ( model, Action msg )
    , view : FormModel -> RouteParams -> model -> Html (Msg msg)
    }


initOneOf : List (Router model msg) -> model -> Maybe msg
initOneOf routers model =
    case routers of
        [] ->
            Nothing

        (Router router) :: xs ->
            case router.init model of
                Just html ->
                    Just html

                Nothing ->
                    initOneOf xs model


viewOneOf : List (Router model msg) -> model -> Maybe (H.Html msg)
viewOneOf routers model =
    case routers of
        [] ->
            Nothing

        (Router router) :: xs ->
            case router.view model of
                Just html ->
                    Just html

                Nothing ->
                    viewOneOf xs model



-- type ProtectedRouter model msg
--     = ProtectedRouter
--         { fromModel : model -> Maybe subModel
--         , router : Admin.Internal.Router.Router
--         }
-- router : List (Route model msg) -> Router model msg
-- router =
--     protectedRouter (Just << identity)
-- protectedRouter :
--     model
--     -> Maybe subModel
--     -> List (Route subModel msg)
--     -> Router model msg
-- protectedRouter props routes =
--     let
--         allRoutePaths : List String
--         allRoutePaths =
--             List.concatMap routePaths routes
--         allPages : Dict String (Page model msg)
--         allPages =
--             Debug.todo ""
--     in
--     ProtectedRouter
--         { fromModel = props.fromModel
--         , toMsg = props.toMsg
--         , router = Admin.Internal.Router.fromList allRoutePaths
--         }
