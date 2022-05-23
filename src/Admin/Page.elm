module Admin.Page exposing
    ( page, params, Page
    , title, nav, init
    , view, card, list, form
    , oneParam, customParam
    , parsedParams, paramsParser, path, custom, query, queryList, ParamsParser
    )

{-|

@docs page, params, Page
@docs title, nav, init
@docs view, card, list, form
@docs oneParam, customParam
@docs parsedParams, paramsParser, path, custom, query, queryList, ParamsParser

-- TODO: hidden, loading, readOnly

-}

import Admin.Internal.Application
import Admin.Internal.Form exposing (FieldValue, FormModel)
import Admin.Internal.Page exposing (Page(..))
import Admin.Libs.Router exposing (RouteParams)
import Admin.Shared exposing (Msg(..))
import Admin.UI.Form
import Admin.UI.List
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import W.DataRow


{-| -}
type alias Page model msg params =
    Admin.Internal.Page.Page model msg params


{-| -}
page : String -> Admin.Internal.Page.Page model msg ()
page title_ =
    Admin.Internal.Page.Page
        { baseTitle = title_
        , toParams = \_ -> Just ()
        , title = \_ _ -> title_
        , nav = \_ _ -> title_
        , init = \_ _ -> Nothing
        , view = \_ _ _ -> H.text ""
        , forms = []
        }



-- Params


{-| -}
params :
    (RouteParams -> Maybe params)
    -> Page model msg x
    -> Page model msg params
params toParams (Page p) =
    Page
        { toParams = toParams
        , baseTitle = p.baseTitle
        , title = \_ _ -> p.baseTitle
        , nav = \_ _ -> p.baseTitle
        , init = \_ _ -> Nothing
        , view = \_ _ _ -> H.text ""
        , forms = []
        }


{-| -}
nav :
    (model -> params -> String)
    -> Page model msg params
    -> Page model msg params
nav v (Page p) =
    Page { p | nav = v }


{-| -}
title :
    (model -> params -> String)
    -> Page model msg params
    -> Page model msg params
title v (Page p) =
    Page { p | title = v }


{-| -}
init :
    (model -> params -> msg)
    -> Page model msg params
    -> Page model msg params
init v (Page p) =
    Page { p | init = withInit p.init (\model params_ -> GotMsg (v model params_)) }


{-| -}
view :
    (model -> params -> H.Html msg)
    -> Page model msg params
    -> Page model msg params
view v (Page p) =
    Page { p | view = withView p.view (\_ model params_ -> v model params_ |> H.map GotMsg) }


{-| -}
card :
    (model -> params -> H.Html msg)
    -> Page model msg params
    -> Page model msg params
card v (Page p) =
    Page
        { p
            | view =
                withView p.view
                    (\_ model params_ ->
                        H.div
                            [ HA.class "eadm eadm-card" ]
                            [ v model params_ ]
                            |> H.map GotMsg
                    )
        }


{-| -}
list :
    { title : H.Html msg
    , init : model -> params -> Maybe (List a)
    , toItem :
        model
        -> params
        -> a
        ->
            { label : H.Html msg
            , actions : List (H.Html msg)
            , options : List (W.DataRow.Attribute msg)
            }
    }
    -> Page model msg params
    -> Page model msg params
list props (Page p) =
    Page
        { p
            | view =
                withView p.view
                    (\_ model params_ ->
                        Admin.UI.List.view
                            { title = props.title
                            , items =
                                props.init model params_
                                    |> Maybe.map (List.map (props.toItem model params_))
                            }
                    )
        }


type alias FormAttributes model params =
    { id : Maybe String
    , hidden : model -> params -> Bool
    , loading : model -> params -> Bool
    , readOnly : model -> params -> Bool
    }


formDefaults : FormAttributes model params
formDefaults =
    { id = Nothing
    , hidden = \_ _ -> False
    , loading = \_ _ -> False
    , readOnly = \_ _ -> False
    }


{-| -}
form :
    { init : model -> params -> Maybe resource
    , form : Admin.Internal.Form.Form model msg params resource
    , attrs : List (FormAttributes model params -> FormAttributes model params)
    , onSubmit : model -> params -> resource -> msg
    }
    -> Page model msg params
    -> Page model msg params
form props (Page p) =
    let
        attrs =
            List.foldl (\fn a -> fn a) formDefaults props.attrs

        formId : String
        formId =
            attrs.id
                |> Maybe.withDefault
                    ("eadm-form-" ++ (String.fromInt <| List.length p.forms))

        forms_ :
            List
                ( String
                , params
                  -> model
                  ->
                    Maybe
                        { values : Dict String FieldValue
                        , initMsg : Msg msg
                        }
                )
        forms_ =
            ( formId
            , \params_ model ->
                props.init model params_
                    |> Maybe.map
                        (\resource ->
                            Admin.Internal.Application.initFields model
                                params_
                                resource
                                formId
                                props.form
                        )
            )
                :: p.forms

        view_ =
            withView p.view
                (\forms model params_ ->
                    Admin.UI.Form.view
                        { formId = formId
                        , model = model
                        , params = params_
                        , form = props.form
                        , isLoading = attrs.loading model params_
                        , isHidden = attrs.hidden model params_
                        , isReadOnly = attrs.readOnly model params_
                        , onSubmit = props.onSubmit
                        }
                        (Dict.get formId forms)
                )
    in
    Page
        { p
            | view = view_
            , forms = forms_
        }


withInit :
    (model -> params -> Maybe (Msg msg))
    -> (model -> params -> Msg msg)
    -> model
    -> params
    -> Maybe (Msg msg)
withInit before after model params_ =
    before model params_
        |> Maybe.map (\msg -> Batch [ msg, after model params_ ])
        |> Maybe.withDefault (after model params_)
        |> Just


withView :
    (Dict String FormModel -> model -> params -> H.Html (Msg msg))
    -> (Dict String FormModel -> model -> params -> H.Html (Msg msg))
    -> Dict String FormModel
    -> model
    -> params
    -> H.Html (Msg msg)
withView before after forms model params_ =
    H.div []
        [ before forms model params_
        , H.div
            [ HA.class "eadm eadm-view eadm-fade-slide" ]
            [ after forms model params_ ]
        ]


{-| -}
oneParam :
    String
    -> Page model msg x
    -> Page model msg String
oneParam key =
    params
        (\{ pathParams } -> Dict.get key pathParams)


{-| -}
customParam :
    String
    -> (String -> Maybe params)
    -> Page model msg x
    -> Page model msg params
customParam key parser =
    params
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
    params resolver


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
