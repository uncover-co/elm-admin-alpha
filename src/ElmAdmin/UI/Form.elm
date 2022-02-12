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
import Set


view :
    { formModel : FormModel
    , model : model
    , params : params
    , form : Form model msg params resource
    , isLoading : Bool
    , isHidden : Bool
    , isReadOnly : Bool
    , onSubmit : model -> params -> resource -> msg
    }
    -> Html (Msg msg)
view props =
    if props.isHidden then
        text ""

    else if props.isLoading then
        viewLoading props.form

    else
        viewForm props


viewForm :
    { formModel : FormModel
    , model : model
    , params : params
    , form : Form model msg params resource
    , isLoading : Bool
    , isHidden : Bool
    , isReadOnly : Bool
    , onSubmit : model -> params -> resource -> msg
    }
    -> Html (Msg msg)
viewForm ({ formModel, model, params, form, onSubmit } as props) =
    form.resolver formModel model
        |> Maybe.map
            (\( resource, errors ) ->
                Html.form
                    [ class "eadm eadm-card"
                    , HE.preventDefaultOn "submit"
                        ((\_ ->
                            if Dict.isEmpty errors then
                                onSubmit model params resource
                                    |> GotMsg
                                    |> D.succeed

                            else
                                D.succeed DoNothing
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
                                            ifVisible
                                                f.attrs.hidden
                                                model
                                                params
                                                resource
                                                (\_ ->
                                                    W.field
                                                        [ Dict.get label errors
                                                            |> Maybe.map
                                                                (\err ->
                                                                    if Set.member ( form.title, label ) formModel.validated then
                                                                        WA.danger err

                                                                    else
                                                                        WA.none
                                                                )
                                                            |> Maybe.withDefault WA.none
                                                        ]
                                                        { label = text label
                                                        , input =
                                                            W.textInput
                                                                [ WA.readOnly (props.isReadOnly || f.attrs.readOnly model params resource)
                                                                , WA.onBlur (SetValidatedField ( form.title, label ))
                                                                ]
                                                                { value = v
                                                                , onInput =
                                                                    ElmAdmin.Internal.Form.FieldValueString
                                                                        >> UpdateFormField ( form.title, label )
                                                                }
                                                        }
                                                )

                                        ( Autocomplete f, Just (FieldValueAutocomplete ( search, value )) ) ->
                                            ifVisible f.attrs.hidden
                                                model
                                                params
                                                resource
                                                (\_ ->
                                                    W.field
                                                        [ Dict.get label errors
                                                            |> Maybe.map WA.danger
                                                            |> Maybe.withDefault WA.none
                                                        ]
                                                        { label = text label
                                                        , input =
                                                            W.autocomplete
                                                                [ WA.readOnly (props.isReadOnly || f.attrs.readOnly model params resource)
                                                                , WA.onBlur (SetValidatedField ( form.title, label ))
                                                                , f.attrs.onEnter
                                                                    |> Maybe.map (\fn -> WA.onEnter (GotMsg (fn search)))
                                                                    |> Maybe.withDefault WA.none
                                                                ]
                                                                { id = label
                                                                , search = search
                                                                , value = value
                                                                , options = f.options model
                                                                , toLabel = identity
                                                                , onInput =
                                                                    \search_ value_ ->
                                                                        case f.attrs.onSearch of
                                                                            Just onSearch ->
                                                                                Batch
                                                                                    [ ( search_, value_ )
                                                                                        |> ElmAdmin.Internal.Form.FieldValueAutocomplete
                                                                                        |> UpdateFormField ( form.title, label )
                                                                                    , GotMsg (onSearch search_)
                                                                                    ]

                                                                            Nothing ->
                                                                                ( search_, value_ )
                                                                                    |> ElmAdmin.Internal.Form.FieldValueAutocomplete
                                                                                    |> UpdateFormField ( form.title, label )
                                                                }
                                                        }
                                                )

                                        ( Checkbox f, Just (FieldValueBool v) ) ->
                                            ifVisible f.attrs.hidden
                                                model
                                                params
                                                resource
                                                (\_ ->
                                                    W.field []
                                                        { label = text label
                                                        , input =
                                                            W.checkbox
                                                                [ WA.readOnly (props.isReadOnly || f.attrs.readOnly model params resource)
                                                                ]
                                                                { value = v
                                                                , onInput =
                                                                    ElmAdmin.Internal.Form.FieldValueBool
                                                                        >> UpdateFormField ( form.title, label )
                                                                }
                                                        }
                                                )

                                        ( Radio f, Just (FieldValueString v) ) ->
                                            ifVisible f.attrs.hidden
                                                model
                                                params
                                                resource
                                                (\_ ->
                                                    W.field []
                                                        { label = text label
                                                        , input =
                                                            W.radioButtons
                                                                [ WA.readOnly (props.isReadOnly || f.attrs.readOnly model params resource)
                                                                ]
                                                                { id = form.title ++ "-" ++ label
                                                                , value = v
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
                                                model
                                                params
                                                resource
                                                (\_ ->
                                                    W.field []
                                                        { label = text label
                                                        , input =
                                                            W.select
                                                                [ WA.readOnly (props.isReadOnly || f.attrs.readOnly model params resource)
                                                                ]
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
                                                model
                                                params
                                                resource
                                                (\_ ->
                                                    W.field []
                                                        { label = text label
                                                        , input =
                                                            W.rangeInput
                                                                [ WA.readOnly (props.isReadOnly || f.attrs.readOnly model params resource)
                                                                ]
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
            )
        |> Maybe.withDefault (text "")


viewLoading : Form model msg params resource -> Html (Msg msg)
viewLoading form =
    Html.div [ class "eadm eadm-card" ]
        [ p [ class "eadm eadm-form-title" ] [ text form.title ]
        , section
            [ class "eadm eadm-form-loading" ]
            [ W.loadingCircle [ WA.size 32 ] ]
        ]


ifVisible : (model -> params -> resource -> Bool) -> model -> params -> resource -> (() -> Html (Msg msg)) -> Html (Msg msg)
ifVisible hidden_ m p r html =
    if hidden_ m p r then
        text ""

    else
        html ()
