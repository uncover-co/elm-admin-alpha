module Admin.Page exposing
    ( page, params, Page
    , title, nav, init
    , view, card, list, form
    )

{-|

@docs page, params, Page
@docs title, nav, init
@docs view, card, list, form

-}

import Admin.Internal.Application
import Admin.Internal.Page exposing (Page(..))
import Admin.Libs.Router exposing (RouteParams)
import Admin.Shared exposing (Msg(..))
import ElmAdmin.Internal.Form exposing (FormModel)
import ElmAdmin.UI.Form
import ElmAdmin.UI.List
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
        }


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
                        ElmAdmin.UI.List.view
                            { title = props.title
                            , items =
                                props.init model params_
                                    |> Maybe.map (List.map (props.toItem model params_))
                            }
                    )
        }


type alias FormAttributes model params =
    { hidden : model -> params -> Bool
    , loading : model -> params -> Bool
    , readOnly : model -> params -> Bool
    }


formDefaults : FormAttributes model params
formDefaults =
    { hidden = \_ _ -> False
    , loading = \_ _ -> False
    , readOnly = \_ _ -> False
    }


{-| -}
form :
    { init : model -> params -> Maybe resource
    , form : ElmAdmin.Internal.Form.Form model msg params resource
    , attrs : List (FormAttributes model params -> FormAttributes model params)
    , onSubmit : model -> params -> resource -> msg
    }
    -> Page model msg params
    -> Page model msg params
form props (Page p) =
    let
        attrs =
            List.foldl (\fn a -> fn a) formDefaults props.attrs

        init_ =
            withInit p.init
                (\model params_ ->
                    props.init model params_
                        |> Maybe.map
                            (\resource ->
                                Admin.Internal.Application.initFields model params_ resource props.form
                                    |> UpdateFormModel
                            )
                        |> Maybe.withDefault DoNothing
                )

        view_ =
            withView p.view
                (\formModel model params_ ->
                    ElmAdmin.UI.Form.view
                        { formModel = formModel
                        , model = model
                        , params = params_
                        , form = props.form
                        , isLoading = attrs.loading model params_
                        , isHidden = attrs.hidden model params_
                        , isReadOnly = attrs.readOnly model params_
                        , onSubmit = props.onSubmit
                        }
                )
    in
    Page
        { p
            | init = init_
            , view = view_
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
    (FormModel -> model -> params -> H.Html (Msg msg))
    -> (FormModel -> model -> params -> H.Html (Msg msg))
    -> FormModel
    -> model
    -> params
    -> H.Html (Msg msg)
withView before after formModel model params_ =
    H.div []
        [ before formModel model params_
        , H.div
            [ HA.class "eadm eadm-view eadm-fade-slide" ]
            [ after formModel model params_ ]
        ]
