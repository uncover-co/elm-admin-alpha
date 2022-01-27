module ElmAdmin.Internal.Page exposing
    ( Page
    , PageData
    , Route
    , card
    , form
    , init
    , list
    , nav
    , page
    , params
    , route
    , subscriptions
    , title
    , toTitle
    , update
    , view
    )

import Dict exposing (Dict)
import ElmAdmin.Internal.Form exposing (FormModel)
import ElmAdmin.Router exposing (RouteParams, pathFromString)
import ElmAdmin.Shared exposing (Action, Effect(..), Msg(..))
import ElmAdmin.UI.Form
import ElmAdmin.UI.List
import ElmWidgets as W
import Html as H exposing (Html)
import Html.Attributes as HA
import Set
import SubCmd
import Time exposing (Month(..))



-- Model


type alias Route model msg =
    { page : PageData model msg
    , path : String
    , pathList : List String
    , pathParams : List String
    , disabled : RouteParams -> model -> Bool
    }


type alias PageData model msg =
    { nav : Maybe (RouteParams -> model -> String)
    , title : RouteParams -> model -> String
    , init : RouteParams -> model -> ( model, Action msg )
    , update : FormModel -> RouteParams -> Msg msg -> model -> ( model, Action msg )
    , subscriptions : RouteParams -> model -> Sub (Msg msg)
    , view : FormModel -> RouteParams -> model -> Html (Msg msg)
    }


{-| -}
type Page model msg params
    = Page
        { title : String
        , toParams : RouteParams -> Maybe params
        , sampleParams : Maybe (Dict String String)
        , forms : Dict String Int
        , page :
            { nav : Maybe (model -> params -> String)
            , title : model -> params -> String
            , init : model -> params -> ( model, Action msg )
            , update : FormModel -> Msg msg -> model -> params -> ( model, Action msg )
            , subscriptions : model -> params -> Sub (Msg msg)
            , view : FormModel -> model -> params -> Html (Msg msg)
            }
        }



-- Unwrapping to PageData


route : Page model msg params -> String -> Maybe (Route model msg)
route page_ path =
    let
        ( pathList, pathParams ) =
            pathFromString path
    in
    validPageAtPath page_ pathParams
        |> Maybe.andThen validatePageForms
        |> Maybe.map
            (\(Page p) ->
                { disabled = \_ _ -> False
                , path = path
                , pathList = pathList
                , pathParams = pathParams
                , page =
                    { nav =
                        p.page.nav
                            |> Maybe.map
                                (\nav_ ->
                                    \routeParams model_ ->
                                        p.toParams routeParams
                                            |> Maybe.map (\params_ -> nav_ model_ params_)
                                            |> Maybe.withDefault ""
                                )
                    , title =
                        \routeParams model_ ->
                            p.toParams routeParams
                                |> Maybe.map (\params_ -> p.page.title model_ params_)
                                |> Maybe.withDefault ""
                    , init =
                        \routeParams model_ ->
                            p.toParams routeParams
                                |> Maybe.map (\params_ -> p.page.init model_ params_)
                                |> Maybe.withDefault ( model_, SubCmd.none )
                    , update =
                        \formModel_ routeParams msg model_ ->
                            p.toParams routeParams
                                |> Maybe.map (\params_ -> p.page.update formModel_ msg model_ params_)
                                |> Maybe.withDefault ( model_, SubCmd.none )
                    , subscriptions =
                        \routeParams model_ ->
                            p.toParams routeParams
                                |> Maybe.map (\params_ -> p.page.subscriptions model_ params_)
                                |> Maybe.withDefault Sub.none
                    , view =
                        \formModel_ routeParams model_ ->
                            p.toParams routeParams
                                |> Maybe.map (\params_ -> p.page.view formModel_ model_ params_)
                                |> Maybe.withDefault (H.text "")
                    }
                }
            )


{-| As default we give the params a string that would be parsed as string, int and float.
If the user needs something more specific they can give it to us as sampleParams.
-}
sampleParam : String
sampleParam =
    "123456790"


{-| We do a runtime check at the start of the appliation to make sure the page is placed in a route with proper params.
-}
validPageAtPath : Page model msg params -> List String -> Maybe (Page model msg params)
validPageAtPath (Page p) pathParamList =
    let
        sampleParams_ =
            p.sampleParams
                |> Maybe.map
                    (\validParams ->
                        pathParamList
                            |> List.map
                                (\param ->
                                    ( param
                                    , Dict.get param validParams
                                        |> Maybe.withDefault sampleParam
                                    )
                                )
                            |> Dict.fromList
                    )
                |> Maybe.withDefault
                    (pathParamList
                        |> List.map (\param -> ( param, sampleParam ))
                        |> Dict.fromList
                    )
    in
    p.toParams
        { path = ""
        , pathParams = sampleParams_
        , queryParams = Dict.empty
        }
        |> Maybe.map (\_ -> Page p)


{-| Pages can't have two identical forms.
Form Id's are generated as a hash of both form title + fields.
So if we have two forms with the exact same Id chances are they are the same – even if they were built separately.
-}
validatePageForms : Page model msg params -> Maybe (Page model msg params)
validatePageForms (Page p) =
    let
        duplicates =
            Dict.filter (\_ v -> v > 1) p.forms
    in
    if Dict.isEmpty duplicates then
        Just (Page p)

    else
        Nothing



-- Page builders


page : String -> Page model msg ()
page title_ =
    Page
        { title = title_
        , toParams = \_ -> Just ()
        , sampleParams = Nothing
        , forms = Dict.empty
        , page =
            { nav = Nothing
            , title = \_ _ -> title_
            , init = \model _ -> ( model, SubCmd.none )
            , update = \_ _ model _ -> ( model, SubCmd.none )
            , subscriptions = \_ _ -> Sub.none
            , view = \_ _ _ -> H.text ""
            }
        }


params :
    (RouteParams -> Maybe params)
    -> Page model msg x
    -> Page model msg params
params toParams (Page p) =
    Page
        { title = p.title
        , toParams = toParams
        , sampleParams = p.sampleParams
        , forms = p.forms
        , page =
            { nav = Nothing
            , title = \_ _ -> p.title
            , init = \model _ -> ( model, SubCmd.none )
            , update = \_ _ model _ -> ( model, SubCmd.none )
            , subscriptions = \_ _ -> Sub.none
            , view = \_ _ _ -> H.text ""
            }
        }


toTitle : Page model msg params -> String
toTitle (Page p) =
    p.title


nav :
    (model -> params -> String)
    -> Page model msg params
    -> Page model msg params
nav nav_ (Page p) =
    let
        page_ =
            p.page
    in
    Page { p | page = { page_ | nav = Just nav_ } }


title :
    (model -> params -> String)
    -> Page model msg params
    -> Page model msg params
title title_ (Page p) =
    let
        page_ =
            p.page
    in
    Page { p | page = { page_ | title = title_ } }


init :
    (model -> params -> ( model, Action msg ))
    -> Page model msg params
    -> Page model msg params
init init_ (Page p) =
    let
        page_ =
            p.page

        init__ =
            withInit p.page.init init_
    in
    Page { p | page = { page_ | init = init__ } }


update : (Msg msg -> model -> params -> ( model, Action msg )) -> Page model msg params -> Page model msg params
update update_ (Page p) =
    let
        page_ =
            p.page

        update__ =
            withUpdate p.page.update (\_ -> update_)
    in
    Page { p | page = { page_ | update = update__ } }


subscriptions : (model -> params -> Sub msg) -> Page model msg params -> Page model msg params
subscriptions subscriptions_ (Page p) =
    let
        page_ =
            p.page

        subscriptions__ =
            withSubscriptions p.page.subscriptions
                (\model params_ ->
                    subscriptions_ model params_
                        |> Sub.map GotMsg
                )
    in
    Page { p | page = { page_ | subscriptions = subscriptions__ } }


view : (model -> params -> Html msg) -> Page model msg params -> Page model msg params
view view_ (Page p) =
    let
        page_ =
            p.page

        view__ =
            withView p.page.view
                (\_ model params_ ->
                    view_ model params_
                        |> H.map GotMsg
                )
    in
    Page { p | page = { page_ | view = view__ } }


card : (model -> params -> Html msg) -> Page model msg params -> Page model msg params
card view_ (Page p) =
    let
        page_ =
            p.page

        view__ =
            withView p.page.view
                (\_ model params_ ->
                    H.div [ HA.class "eadm eadm-card" ]
                        [ view_ model params_
                        ]
                        |> H.map GotMsg
                )
    in
    Page { p | page = { page_ | view = view__ } }


form :
    { init : model -> params -> Maybe resource
    , form : ElmAdmin.Internal.Form.Form model msg params resource
    , onSubmit : resource -> msg
    }
    -> Page model msg params
    -> Page model msg params
form props (Page p) =
    let
        page_ =
            p.page

        init_ =
            withInit p.page.init
                (\model params_ ->
                    props.init model params_
                        |> Maybe.map
                            (\resource ->
                                ( model
                                , SubCmd.effect
                                    (ElmAdmin.Internal.Form.initFields resource props.form
                                        |> UpdateFormModel
                                    )
                                )
                            )
                        |> Maybe.withDefault ( model, SubCmd.none )
                )

        update_ =
            withUpdate p.page.update
                (\formModel _ model params_ ->
                    if not (Set.member props.form.title formModel.initialized) then
                        props.init model params_
                            |> Maybe.map
                                (\resource ->
                                    ( model
                                    , SubCmd.effect
                                        (ElmAdmin.Internal.Form.initFields resource props.form
                                            |> UpdateFormModel
                                        )
                                    )
                                )
                            |> Maybe.withDefault ( model, SubCmd.none )

                    else
                        ( model, SubCmd.none )
                )

        view_ =
            withView p.page.view
                (\formModel model params_ ->
                    if Set.member props.form.title formModel.initialized then
                        ElmAdmin.UI.Form.view
                            formModel
                            model
                            params_
                            props.form
                            props.onSubmit

                    else
                        ElmAdmin.UI.Form.viewLoading props.form
                )
    in
    Page
        { p
            | forms =
                p.forms
                    |> Dict.update props.form.title
                        (\r ->
                            r
                                |> Maybe.withDefault 0
                                |> (+) 1
                                |> Just
                        )
            , page =
                { page_
                    | init = init_
                    , update = update_
                    , view = view_
                }
        }


list :
    { title : Html msg
    , init : model -> params -> Maybe (List a)
    , toItem :
        model
        -> a
        ->
            { label : Html msg
            , actions : List (Html msg)
            , options : List (W.DataRowAttributes msg -> W.DataRowAttributes msg)
            }
    }
    -> Page model msg params
    -> Page model msg params
list props (Page p) =
    let
        page_ =
            p.page

        view_ =
            withView p.page.view
                (\_ model params_ ->
                    ElmAdmin.UI.List.view
                        { title = props.title
                        , items =
                            props.init model params_
                                |> Maybe.map (List.map (props.toItem model))
                        }
                )
    in
    Page { p | page = { page_ | view = view_ } }



-- Page builder helpers


withInit :
    (model -> params -> ( model, Action msg ))
    -> (model -> params -> ( model, Action msg ))
    -> model
    -> params
    -> ( model, Action msg )
withInit before after model params_ =
    before model params_
        |> (\( model_, cmd ) ->
                after model_ params_
                    |> Tuple.mapSecond (\c -> SubCmd.batch [ c, cmd ])
           )


withUpdate :
    (FormModel -> Msg msg -> model -> params -> ( model, Action msg ))
    -> (FormModel -> Msg msg -> model -> params -> ( model, Action msg ))
    -> FormModel
    -> Msg msg
    -> model
    -> params
    -> ( model, Action msg )
withUpdate before after formModel msg model params_ =
    before formModel msg model params_
        |> (\( model_, cmd ) ->
                after formModel msg model_ params_
                    |> Tuple.mapSecond (\c -> SubCmd.batch [ c, cmd ])
           )


withSubscriptions :
    (model -> params -> Sub (Msg msg))
    -> (model -> params -> Sub (Msg msg))
    -> model
    -> params
    -> Sub (Msg msg)
withSubscriptions before after model params_ =
    Sub.batch
        [ before model params_
        , after model params_
        ]


withView :
    (FormModel -> model -> params -> Html (Msg msg))
    -> (FormModel -> model -> params -> Html (Msg msg))
    -> FormModel
    -> model
    -> params
    -> Html (Msg msg)
withView before after formData model params_ =
    H.div []
        [ before formData model params_
        , H.div [ HA.class "eadm eadm-view" ]
            [ after formData model params_
            ]
        ]
