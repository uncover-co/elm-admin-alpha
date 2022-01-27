module ElmAdmin.UI.Form exposing (..)

import Dict
import ElmAdmin.Internal.Form exposing (Field(..), FieldValue(..), Form, FormModel)
import ElmAdmin.Shared exposing (Msg(..))
import ElmWidgets as W
import ElmWidgets.Attributes as WA
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE
import Json.Decode as D


view : FormModel -> model -> params -> Form model msg params resource -> (resource -> msg) -> Html (Msg msg)
view formModel model params form onSubmit =
    Html.form
        [ class "eadm eadm-card"
        , HE.preventDefaultOn "submit"
            ((\_ ->
                D.succeed
                    (form.resolver formModel model
                        |> Maybe.map (GotMsg << onSubmit)
                        |> Maybe.withDefault DoNothing
                    )
             )
                |> D.lazy
                |> D.map (\msg -> ( msg, True ))
            )
        ]
        [ p [ class "eadm eadm-form-title" ] [ text form.title ]
        , ul [ class "eadm eadm-form-fields" ]
            (form.fields
                |> List.reverse
                |> List.map
                    (\( label, field ) ->
                        case ( field, Dict.get ( form.title, label ) formModel.values ) of
                            ( Text f, Just (FieldValueString v) ) ->
                                ifVisible f.attrs.hidden
                                    formModel
                                    model
                                    params
                                    form
                                    (\_ ->
                                        W.field []
                                            { label = text label
                                            , input =
                                                W.textInput []
                                                    { value = v
                                                    , onInput =
                                                        ElmAdmin.Internal.Form.FieldValueString
                                                            >> UpdateFormField ( form.title, label )
                                                    }
                                            }
                                    )

                            ( Autocomplete f, Just (FieldValueAutocomplete ( search, value )) ) ->
                                ifVisible f.attrs.hidden
                                    formModel
                                    model
                                    params
                                    form
                                    (\_ ->
                                        W.field []
                                            { label = text label
                                            , input =
                                                W.autocomplete []
                                                    { id = label
                                                    , search = search
                                                    , value = value
                                                    , options = f.options model
                                                    , toLabel = identity
                                                    , onInput =
                                                        \search_ value_ ->
                                                            ( search_, value_ )
                                                                |> ElmAdmin.Internal.Form.FieldValueAutocomplete
                                                                |> UpdateFormField ( form.title, label )
                                                    , onEnter =
                                                        f.attrs.onEnter
                                                            |> Maybe.map (\fn -> GotMsg (fn search))
                                                    }
                                            }
                                    )

                            ( Checkbox f, Just (FieldValueBool v) ) ->
                                ifVisible f.attrs.hidden
                                    formModel
                                    model
                                    params
                                    form
                                    (\_ ->
                                        W.field []
                                            { label = text label
                                            , input =
                                                W.checkbox []
                                                    { value = v
                                                    , onInput =
                                                        ElmAdmin.Internal.Form.FieldValueBool
                                                            >> UpdateFormField ( form.title, label )
                                                    }
                                            }
                                    )

                            ( Radio f, Just (FieldValueString v) ) ->
                                ifVisible f.attrs.hidden
                                    formModel
                                    model
                                    params
                                    form
                                    (\_ ->
                                        W.field []
                                            { label = text label
                                            , input =
                                                W.radioButtons []
                                                    { value = v
                                                    , options = f.options model
                                                    , toLabel = identity
                                                    , toValue = identity
                                                    , onInput =
                                                        ElmAdmin.Internal.Form.FieldValueString
                                                            >> UpdateFormField ( form.title, label )
                                                    }
                                            }
                                    )

                            ( Select f, Just (FieldValueString v) ) ->
                                ifVisible f.attrs.hidden
                                    formModel
                                    model
                                    params
                                    form
                                    (\_ ->
                                        W.field []
                                            { label = text label
                                            , input =
                                                W.select []
                                                    { value = v
                                                    , options = f.options model
                                                    , toLabel = identity
                                                    , toValue = identity
                                                    , onInput =
                                                        ElmAdmin.Internal.Form.FieldValueString
                                                            >> UpdateFormField ( form.title, label )
                                                    }
                                            }
                                    )

                            ( Range f, Just (FieldValueFloat v) ) ->
                                ifVisible f.attrs.hidden
                                    formModel
                                    model
                                    params
                                    form
                                    (\_ ->
                                        W.field []
                                            { label = text label
                                            , input =
                                                W.rangeInput []
                                                    { value = v
                                                    , min = f.min
                                                    , max = f.max
                                                    , step = f.step
                                                    , onInput =
                                                        ElmAdmin.Internal.Form.FieldValueFloat
                                                            >> UpdateFormField ( form.title, label )
                                                    }
                                            }
                                    )

                            _ ->
                                text ""
                    )
            )
        , section [ class "eadm eadm-form-footer" ]
            [ div [ class "eadm eadm-form-footer-inner" ]
                [ W.primaryButton []
                    { label = text "Confirm"
                    , onClick = DoNothing
                    }
                ]
            ]
        ]


viewLoading : Form model msg params resource -> Html (Msg msg)
viewLoading form =
    Html.div [ class "eadm eadm-card" ]
        [ p [ class "eadm eadm-form-title" ] [ text form.title ]
        , section
            [ class "eadm eadm-form-loading" ]
            [ W.loadingCircle [ WA.size 32 ] ]
        ]


ifVisible :
    Maybe (model -> params -> resource -> Bool)
    -> FormModel
    -> model
    -> params
    -> Form model msg params resource
    -> (() -> Html (Msg msg))
    -> Html (Msg msg)
ifVisible hidden formModel model params form html =
    if
        hidden
            |> Maybe.andThen
                (\fn ->
                    form.resolver formModel model
                        |> Maybe.map (fn model params)
                )
            |> Maybe.withDefault False
    then
        text ""

    else
        html ()
