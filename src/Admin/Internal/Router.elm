module Admin.Internal.Router exposing
    ( empty, insert, fromList, toList, Router
    , findByUrl, findByString, toPathId, toPath, toParams, withParams, Route, RouteParams
    , validRoutes
    , normalizePath
    , defaultValidRoutes, emptyParams
    )

{-|

@docs empty, insert, fromList, toList, Router
@docs findByUrl, findByString, toPathId, toPath, toParams, withParams, Route, RouteParams
@docs validRoutes
@docs normalizePath

-}

import Dict exposing (Dict)
import Set exposing (Set)
import Url exposing (Url)


type Router
    = Router
        { routes : Dict (List String) RouteDefinition
        , paramPaths : Set (List String)
        }


type alias RouteDefinition =
    { path : String
    , pathList : List String
    , requiredParams : Set String
    }


type Route
    = Route
        { definition : RouteDefinition
        , params : RouteParams
        }


type alias RouteParams =
    { pathParams : Dict String String
    , queryParams : Dict String (List String)
    }


toPathId : Route -> String
toPathId (Route route_) =
    route_.definition.path


toPath : Route -> String
toPath (Route route_) =
    toPath_ route_.params.pathParams route_.definition.path


toPath_ : Dict String String -> String -> String
toPath_ pathParams_ path =
    Dict.foldl (\k v acc -> String.replace k v acc) path pathParams_


toParams : Route -> RouteParams
toParams (Route route_) =
    route_.params


emptyParams : RouteParams
emptyParams =
    { pathParams = Dict.empty
    , queryParams = Dict.empty
    }


validRoutes : Route -> Router -> Dict String String
validRoutes (Route route) (Router router) =
    let
        pathParams_ : Set String
        pathParams_ =
            route.params.pathParams
                |> Dict.keys
                |> Set.fromList

        validRoutes_ : Dict String String
        validRoutes_ =
            router.routes
                |> Dict.values
                |> List.filterMap
                    (\r ->
                        if not (Set.isEmpty (Set.diff r.requiredParams pathParams_)) then
                            Nothing

                        else
                            Just
                                ( r.path
                                , toPath_ route.params.pathParams r.path
                                )
                    )
                |> Dict.fromList
    in
    validRoutes_


defaultValidRoutes : Router -> Dict String String
defaultValidRoutes (Router router) =
    router.routes
        |> Dict.values
        |> List.filterMap
            (\r ->
                if not (Set.isEmpty r.requiredParams) then
                    Nothing

                else
                    Just
                        ( r.path
                        , toPath_ Dict.empty r.path
                        )
            )
        |> Dict.fromList


empty : Router
empty =
    Router
        { routes = Dict.empty
        , paramPaths = Set.empty
        }


fromList : List String -> Router
fromList list =
    List.foldl insert empty list


toList : Router -> List String
toList (Router router) =
    router.routes
        |> Dict.values
        |> List.map .path


insert : String -> Router -> Router
insert pathString (Router router) =
    let
        path : List String
        path =
            stringToPath pathString

        pathParams_ : List String
        pathParams_ =
            path
                |> List.filter (String.startsWith ":")

        paramPath : List String
        paramPath =
            path
                |> List.map
                    (\part ->
                        if String.startsWith ":" part then
                            ":param"

                        else
                            part
                    )

        routeDef : RouteDefinition
        routeDef =
            { path = pathToString path
            , pathList = path
            , requiredParams = Set.fromList pathParams_
            }
    in
    Router
        { routes = Dict.insert paramPath routeDef router.routes
        , paramPaths =
            paramPath
                |> List.indexedMap Tuple.pair
                |> List.foldl
                    (\( index, _ ) acc ->
                        Set.insert (List.take (index + 1) paramPath) acc
                    )
                    router.paramPaths
        }


{-| -}
findByUrl : Url -> Router -> Maybe Route
findByUrl url router =
    findByString url.path url.query router


{-| -}
findByString : String -> Maybe String -> Router -> Maybe Route
findByString pathString query router =
    let
        path : List String
        path =
            stringToPath pathString
    in
    getPathDefinition router path
        |> Maybe.andThen
            (\pathDef ->
                pathParams pathDef.pathList path
                    |> Maybe.map
                        (\pathParams_ ->
                            Route
                                { definition = pathDef
                                , params = RouteParams pathParams_ (queryParams query)
                                }
                        )
            )


{-| -}
getPathDefinition : Router -> List String -> Maybe RouteDefinition
getPathDefinition (Router router) path =
    let
        go : List String -> List String -> Maybe RouteDefinition
        go paramsPath unvaluatedPath =
            case Dict.get (paramsPath ++ unvaluatedPath) router.routes of
                Just a ->
                    Just a

                Nothing ->
                    case unvaluatedPath of
                        x :: xs ->
                            let
                                nextParamsPath =
                                    paramsPath ++ [ x ]
                            in
                            if Set.member nextParamsPath router.paramPaths then
                                go nextParamsPath xs

                            else
                                let
                                    nextParamsPath_ =
                                        paramsPath ++ [ ":param" ]
                                in
                                if Set.member nextParamsPath_ router.paramPaths then
                                    go nextParamsPath_ xs

                                else
                                    Nothing

                        [] ->
                            Nothing
    in
    go [] path


{-|

    pathToString [ "users", "123", "new" ]
        == "/users/123/new"

-}
pathToString : List String -> String
pathToString path =
    "/" ++ (path |> String.join "/")


{-| -}
stringToPath : String -> List String
stringToPath pathString =
    pathString
        |> String.split "/"
        |> List.filter (not << String.isEmpty)


normalizePath : String -> String
normalizePath pathString =
    pathString
        |> stringToPath
        |> pathToString


{-| -}
withParams : RouteParams -> Route -> Maybe Route
withParams params (Route route) =
    let
        params_ : Set String
        params_ =
            Set.fromList (Dict.keys params.pathParams)
    in
    if not (Set.isEmpty (Set.diff route.definition.requiredParams params_)) then
        Nothing

    else
        Just
            (Route
                { definition = route.definition
                , params = params
                }
            )


{-|

    pathParams [ "users", ":userId" ] [ "users", "123" ]
        == Just (Dict.fromList [ ( "userId", "123" ) ])

-}
pathParams : List String -> List String -> Maybe (Dict String String)
pathParams defPath path =
    let
        go : List String -> List String -> Maybe (Dict String String) -> Maybe (Dict String String)
        go defPath_ path_ params =
            case ( defPath_, path_ ) of
                ( defPathHead :: defPathTail, pathHead :: pathTail ) ->
                    if String.startsWith ":" defPathHead then
                        params
                            |> Maybe.withDefault Dict.empty
                            |> Dict.insert defPathHead pathHead
                            |> Just
                            |> go defPathTail pathTail

                    else if defPathHead == pathHead then
                        go defPathTail pathTail params

                    else
                        Nothing

                ( [], [] ) ->
                    params

                _ ->
                    Nothing
    in
    go defPath path (Just Dict.empty)


{-|

    queryParams (Just "a=1&a=2&b=true")
        == Dict.fromList [ ( "a", [ "1", "2" ] ), ( "b", "true" ) ]

-}
queryParams : Maybe String -> Dict String (List String)
queryParams qs =
    let
        addQueryParam : String -> String -> Dict String (List String) -> Dict String (List String)
        addQueryParam key value acc =
            Dict.update
                key
                (\xs ->
                    xs
                        |> Maybe.withDefault []
                        |> (\xs_ -> value :: xs_)
                        |> Just
                )
                acc
    in
    qs
        |> Maybe.map
            (\qs_ ->
                qs_
                    |> String.split "&"
                    |> List.foldl
                        (\query acc ->
                            case String.split "=" query of
                                [ key, value ] ->
                                    addQueryParam key value acc

                                _ ->
                                    acc
                        )
                        Dict.empty
            )
        |> Maybe.withDefault Dict.empty
