module ElmAdmin.Internal.Page exposing
    ( Page
    , PageData
    , Route
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
        , page :
            { nav : Maybe (params -> model -> String)
            , title : params -> model -> String
            , init : params -> model -> ( model, Action msg )
            , update : FormModel -> params -> Msg msg -> model -> ( model, Action msg )
            , subscriptions : params -> model -> Sub (Msg msg)
            , view : FormModel -> params -> model -> Html (Msg msg)
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
                                            |> Maybe.map (\params_ -> nav_ params_ model_)
                                            |> Maybe.withDefault ""
                                )
                    , title =
                        \routeParams model_ ->
                            p.toParams routeParams
                                |> Maybe.map (\params_ -> p.page.title params_ model_)
                                |> Maybe.withDefault ""
                    , init =
                        \routeParams model_ ->
                            p.toParams routeParams
                                |> Maybe.map (\params_ -> p.page.init params_ model_)
                                |> Maybe.withDefault ( model_, SubCmd.none )
                    , update =
                        \formModel_ routeParams msg model_ ->
                            p.toParams routeParams
                                |> Maybe.map (\params_ -> p.page.update formModel_ params_ msg model_)
                                |> Maybe.withDefault ( model_, SubCmd.none )
                    , subscriptions =
                        \routeParams model_ ->
                            p.toParams routeParams
                                |> Maybe.map (\params_ -> p.page.subscriptions params_ model_)
                                |> Maybe.withDefault Sub.none
                    , view =
                        \formModel_ routeParams model_ ->
                            p.toParams routeParams
                                |> Maybe.map (\params_ -> p.page.view formModel_ params_ model_)
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



-- Page builders


page : String -> Page model msg ()
page title_ =
    Page
        { title = title_
        , toParams = \_ -> Just ()
        , sampleParams = Nothing
        , page =
            { nav = Nothing
            , title = \_ _ -> title_
            , init = \_ model -> ( model, SubCmd.none )
            , update = \_ _ _ model -> ( model, SubCmd.none )
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
        , page =
            { nav = Nothing
            , title = \_ _ -> p.title
            , init = \_ model -> ( model, SubCmd.none )
            , update = \_ _ _ model -> ( model, SubCmd.none )
            , subscriptions = \_ _ -> Sub.none
            , view = \_ _ _ -> H.text ""
            }
        }


toTitle : Page model msg params -> String
toTitle (Page p) =
    p.title


nav :
    (params -> model -> String)
    -> Page model msg params
    -> Page model msg params
nav nav_ (Page p) =
    let
        page_ =
            p.page
    in
    Page { p | page = { page_ | nav = Just nav_ } }


title :
    (params -> model -> String)
    -> Page model msg params
    -> Page model msg params
title title_ (Page p) =
    let
        page_ =
            p.page
    in
    Page { p | page = { page_ | title = title_ } }


init :
    (params -> model -> ( model, Action msg ))
    -> Page model msg params
    -> Page model msg params
init init_ (Page p) =
    let
        page_ =
            p.page

        init__ =
            withInit p.page.init
                (\routeParams model ->
                    init_ routeParams model
                )
    in
    Page { p | page = { page_ | init = init__ } }


update : (params -> Msg msg -> model -> ( model, Action msg )) -> Page model msg params -> Page model msg params
update update_ (Page p) =
    let
        page_ =
            p.page

        update__ =
            withUpdate p.page.update
                (\_ routeParams msg model ->
                    update_ routeParams msg model
                )
    in
    Page { p | page = { page_ | update = update__ } }


subscriptions : (params -> model -> Sub msg) -> Page model msg params -> Page model msg params
subscriptions subscriptions_ (Page p) =
    let
        page_ =
            p.page

        subscriptions__ =
            withSubscriptions p.page.subscriptions
                (\routeParams model ->
                    subscriptions_ routeParams model
                        |> Sub.map GotMsg
                )
    in
    Page { p | page = { page_ | subscriptions = subscriptions__ } }


view : (params -> model -> Html msg) -> Page model msg params -> Page model msg params
view view_ (Page p) =
    let
        page_ =
            p.page

        view__ =
            withView p.page.view
                (\_ routeParams model ->
                    view_ routeParams model
                        |> H.map GotMsg
                )
    in
    Page { p | page = { page_ | view = view__ } }


form :
    { init : params -> model -> Maybe resource
    , form : ElmAdmin.Internal.Form.Form model msg resource
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
                (\routeParams model ->
                    props.init routeParams model
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
                (\formModel routeParams _ model ->
                    if not (Set.member props.form.title formModel.initialized) then
                        props.init routeParams model
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
                (\formModel _ model ->
                    if Set.member props.form.title formModel.initialized then
                        ElmAdmin.UI.Form.view
                            formModel
                            model
                            props.form
                            props.onSubmit

                    else
                        ElmAdmin.UI.Form.viewLoading props.form
                )
    in
    Page
        { p
            | page =
                { page_
                    | init = init_
                    , update = update_
                    , view = view_
                }
        }


list :
    { title : Html msg
    , init : params -> model -> Maybe (List a)
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
                (\_ routeParams model ->
                    ElmAdmin.UI.List.view
                        { title = props.title
                        , items =
                            props.init routeParams model
                                |> Maybe.map (List.map (props.toItem model))
                        }
                )
    in
    Page { p | page = { page_ | view = view_ } }



-- Page builder helpers


withInit :
    (params -> model -> ( model, Action msg ))
    -> (params -> model -> ( model, Action msg ))
    -> params
    -> model
    -> ( model, Action msg )
withInit before after routeParams model =
    before routeParams model
        |> (\( model_, cmd ) ->
                after routeParams model_
                    |> Tuple.mapSecond (\c -> SubCmd.batch [ c, cmd ])
           )


withUpdate :
    (FormModel -> params -> Msg msg -> model -> ( model, Action msg ))
    -> (FormModel -> params -> Msg msg -> model -> ( model, Action msg ))
    -> FormModel
    -> params
    -> Msg msg
    -> model
    -> ( model, Action msg )
withUpdate before after formModel routeParams msg model =
    before formModel routeParams msg model
        |> (\( model_, cmd ) ->
                after formModel routeParams msg model_
                    |> Tuple.mapSecond (\c -> SubCmd.batch [ c, cmd ])
           )


withSubscriptions :
    (params -> model -> Sub (Msg msg))
    -> (params -> model -> Sub (Msg msg))
    -> params
    -> model
    -> Sub (Msg msg)
withSubscriptions before after routeParams model =
    Sub.batch
        [ before routeParams model
        , after routeParams model
        ]


withView :
    (FormModel -> params -> model -> Html (Msg msg))
    -> (FormModel -> params -> model -> Html (Msg msg))
    -> FormModel
    -> params
    -> model
    -> Html (Msg msg)
withView before after formData routeParams model =
    H.div []
        [ before formData routeParams model
        , H.div [ HA.class "eadm eadm-view" ]
            [ after formData routeParams model
            ]
        ]
