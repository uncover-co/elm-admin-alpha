module ElmAdmin.Router exposing
    ( RouteParams
    , applyParams
    , emptyRouteParams
    , oneOf
    , parsePathParams
    , pathFromString
    , pathToString
    , toCache
    )

import Dict exposing (Dict)
import Url exposing (Url)


type alias RouteParams =
    { path : String
    , pathParams : Dict String String
    , queryParams : Dict String (List String)
    }


emptyRouteParams : RouteParams
emptyRouteParams =
    { path = "/"
    , pathParams = Dict.empty
    , queryParams = Dict.empty
    }


pathFromString : String -> ( List String, List String )
pathFromString pathString =
    pathString
        |> String.split "/"
        |> List.foldl
            (\s ( path, pathParams_ ) ->
                if s == "" then
                    ( path, pathParams_ )

                else
                    ( s :: path
                    , if String.startsWith ":" s then
                        s :: pathParams_

                      else
                        pathParams_
                    )
            )
            ( [], [] )
        |> Tuple.mapFirst List.reverse


pathToString : List String -> String
pathToString path =
    "/" ++ (path |> String.join "/")


parsePathParams : List String -> List String
parsePathParams =
    List.filter (String.startsWith ":")


applyParams : String -> List String -> RouteParams -> Maybe String
applyParams path pathParams_ routeParams_ =
    case pathParams_ of
        h :: xs ->
            Dict.get h routeParams_.pathParams
                |> Maybe.andThen
                    (\param ->
                        applyParams
                            (String.replace h param path)
                            xs
                            routeParams_
                    )

        [] ->
            Just path


toCache : (a -> List String) -> List a -> Dict String (List a)
toCache toPath xs =
    List.foldl
        (\a acc ->
            case toPath a of
                [] ->
                    Dict.insert "" [ a ] acc

                head :: _ ->
                    Dict.update head
                        (\x ->
                            x
                                |> Maybe.withDefault []
                                |> (::) a
                                |> Just
                        )
                        acc
        )
        Dict.empty
        xs


oneOf : (a -> List String) -> Dict String (List a) -> Url -> Maybe ( a, RouteParams )
oneOf toPath cache url =
    String.split "/" url.path
        |> List.drop 1
        |> List.head
        |> Maybe.andThen (\k -> Dict.get k cache)
        |> Maybe.andThen (\xs -> oneOf_ toPath xs url)


oneOf_ : (a -> List String) -> List a -> Url -> Maybe ( a, RouteParams )
oneOf_ toPath xs url =
    case xs of
        h :: xs_ ->
            case fromUrl (toPath h) url of
                Just routeParams ->
                    Just ( h, routeParams )

                Nothing ->
                    oneOf_ toPath xs_ url

        [] ->
            Nothing


fromUrl : List String -> Url -> Maybe RouteParams
fromUrl target url =
    let
        path =
            url.path
                |> String.split "/"
                |> List.filter (not << String.isEmpty)
    in
    pathParams target path
        |> Maybe.map
            (\pathParams_ ->
                { path = pathToString target
                , pathParams = pathParams_
                , queryParams = queryParams url.query
                }
            )


pathParams : List String -> List String -> Maybe (Dict String String)
pathParams path path_ =
    let
        go : List String -> List String -> Maybe (Dict String String) -> Maybe (Dict String String)
        go xs ys params =
            case ( xs, ys ) of
                ( x :: xs_, y :: ys_ ) ->
                    if String.startsWith ":" x then
                        params
                            |> Maybe.withDefault Dict.empty
                            |> Dict.insert x y
                            |> Just
                            |> go xs_ ys_

                    else if x == y then
                        go xs_ ys_ params

                    else
                        Nothing

                ( [], [] ) ->
                    params

                _ ->
                    Nothing
    in
    go path path_ (Just Dict.empty)


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
