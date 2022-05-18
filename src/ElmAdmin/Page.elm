module ElmAdmin.Page exposing
    ( page, title, nav, hidden, loading, readOnly, Page
    , init, update, subscriptions, view
    , initWithActions, updateWithActions
    , card, form, list
    , params, oneParam, customParam
    , parsedParams, paramsParser, path, query, queryList, custom, ParamsParser
    )

{-|


## Setup

@docs page, title, nav, hidden, loading, readOnly, Page


## Elm Architecture

@docs init, update, subscriptions, view


## Actions

@docs initWithActions, updateWithActions


## Especial Views

@docs card, form, list


## Params

@docs params, oneParam, customParam


### Params Parser

@docs parsedParams, paramsParser, path, query, queryList, custom, ParamsParser

-}

import Dict
import ElmAdmin.Actions
import ElmAdmin.Form
import ElmAdmin.Internal.Page
import ElmAdmin.Router exposing (RouteParams)
import ElmAdmin.Shared exposing (Action, Effect(..), Msg(..))
import Html exposing (Html)
import W.DataRow


{-| -}
type alias Page model msg params =
    ElmAdmin.Internal.Page.Page model msg params


{-| -}
page : String -> Page model msg ()
page =
    ElmAdmin.Internal.Page.page


{-| -}
params :
    (RouteParams -> Maybe params)
    -> Page model msg x
    -> Page model msg params
params =
    ElmAdmin.Internal.Page.params


{-| -}
oneParam :
    String
    -> Page model msg x
    -> Page model msg String
oneParam key =
    ElmAdmin.Internal.Page.params
        (\{ pathParams } -> Dict.get key pathParams)


{-| -}
customParam :
    String
    -> (String -> Maybe params)
    -> Page model msg x
    -> Page model msg params
customParam key parser =
    ElmAdmin.Internal.Page.params
        (\{ pathParams } ->
            Dict.get key pathParams
                |> Maybe.andThen parser
        )


{-| -}
type ParamsParser a
    = ParamsParser (RouteParams -> Maybe a)


{-| -}
parsedParams :
    ParamsParser params
    -> Page model msg x
    -> Page model msg params
parsedParams (ParamsParser resolver) =
    ElmAdmin.Internal.Page.params resolver


{-| -}
paramsParser : a -> ParamsParser a
paramsParser a =
    ParamsParser (\_ -> Just a)


{-| -}
path : String -> ParamsParser (String -> a) -> ParamsParser a
path path_ (ParamsParser resolver) =
    ParamsParser
        (\routeParams ->
            resolver routeParams
                |> Maybe.andThen
                    (\resolver_ ->
                        Dict.get path_ routeParams.pathParams
                            |> Maybe.map resolver_
                    )
        )


{-| -}
custom : String -> (String -> Maybe b) -> ParamsParser (b -> a) -> ParamsParser a
custom path_ parser (ParamsParser resolver) =
    ParamsParser
        (\routeParams ->
            resolver routeParams
                |> Maybe.andThen
                    (\resolver_ ->
                        Dict.get path_ routeParams.pathParams
                            |> Maybe.andThen parser
                            |> Maybe.map resolver_
                    )
        )


{-| -}
query : String -> ParamsParser (Maybe String -> a) -> ParamsParser a
query query_ (ParamsParser resolver) =
    ParamsParser
        (\routeParams ->
            resolver routeParams
                |> Maybe.andThen
                    (\resolver_ ->
                        Dict.get query_ routeParams.queryParams
                            |> Maybe.map List.head
                            |> Maybe.map resolver_
                    )
        )


{-| -}
queryList : String -> ParamsParser (Maybe (List String) -> a) -> ParamsParser a
queryList query_ (ParamsParser resolver) =
    ParamsParser
        (\routeParams ->
            resolver routeParams
                |> Maybe.map
                    (\resolver_ ->
                        Dict.get query_ routeParams.queryParams
                            |> resolver_
                    )
        )


{-| -}
nav :
    (model -> params -> String)
    -> Page model msg params
    -> Page model msg params
nav =
    ElmAdmin.Internal.Page.nav


{-| -}
title :
    (model -> params -> String)
    -> Page model msg params
    -> Page model msg params
title =
    ElmAdmin.Internal.Page.title


{-| -}
init :
    (model -> params -> ( model, Cmd msg ))
    -> Page model msg params
    -> Page model msg params
init init_ =
    ElmAdmin.Internal.Page.init
        (\params_ model ->
            init_ params_ model
                |> Tuple.mapSecond ElmAdmin.Actions.cmd
        )


{-| -}
initWithActions :
    (model -> params -> ( model, Action msg ))
    -> Page model msg params
    -> Page model msg params
initWithActions =
    ElmAdmin.Internal.Page.init


{-| -}
update :
    (Msg msg -> model -> params -> ( model, Cmd msg ))
    -> Page model msg params
    -> Page model msg params
update update_ =
    ElmAdmin.Internal.Page.update
        (\params_ msg model ->
            update_ params_ msg model
                |> Tuple.mapSecond ElmAdmin.Actions.cmd
        )


{-| -}
updateWithActions :
    (Msg msg -> model -> params -> ( model, Action msg ))
    -> Page model msg params
    -> Page model msg params
updateWithActions =
    ElmAdmin.Internal.Page.update


{-| -}
subscriptions : (model -> params -> Sub msg) -> Page model msg params -> Page model msg params
subscriptions =
    ElmAdmin.Internal.Page.subscriptions


{-| -}
view : (model -> params -> Html msg) -> Page model msg params -> Page model msg params
view =
    ElmAdmin.Internal.Page.view


{-| -}
card : (model -> params -> Html msg) -> Page model msg params -> Page model msg params
card =
    ElmAdmin.Internal.Page.card


{-| -}
type alias FormAttributes model params =
    { hidden : model -> params -> Bool
    , loading : model -> params -> Bool
    , readOnly : model -> params -> Bool
    }


{-| -}
form :
    { init : model -> params -> Maybe resource
    , form : ElmAdmin.Form.Form model msg params resource
    , attrs : List (FormAttributes model params -> FormAttributes model params)
    , onSubmit : model -> params -> resource -> msg
    }
    -> Page model msg params
    -> Page model msg params
form =
    ElmAdmin.Internal.Page.form


{-| -}
list :
    { title : Html msg
    , init : model -> params -> Maybe (List resource)
    , toItem :
        model
        -> params
        -> resource
        ->
            { label : Html msg
            , actions : List (Html msg)
            , options : List (W.DataRow.Attribute msg)
            }
    }
    -> Page model msg params
    -> Page model msg params
list =
    ElmAdmin.Internal.Page.list



-- Attributes


{-| -}
hidden : (model -> params -> Bool) -> { a | hidden : model -> params -> Bool } -> { a | hidden : model -> params -> Bool }
hidden v a =
    { a | hidden = v }


{-| -}
loading : (model -> params -> Bool) -> { a | loading : model -> params -> Bool } -> { a | loading : model -> params -> Bool }
loading v a =
    { a | loading = v }


{-| -}
readOnly : (model -> params -> Bool) -> { a | readOnly : model -> params -> Bool } -> { a | readOnly : model -> params -> Bool }
readOnly v a =
    { a | readOnly = v }
