module ElmAdmin.UI.Form exposing (..)

import Dict
import ElmAdmin.Internal.Form exposing (Field(..), FieldValue(..), Form, FormModel)
import ElmAdmin.Shared exposing (Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE
import Json.Decode as D
import Set
import W.Button
import W.Field
import W.InputAutocomplete
import W.InputCheckbox
import W.InputRadio
import W.InputSelect
import W.InputSlider
import W.InputText
import W.Loading


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
    form.resolver formModel model params
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
                                                    W.Field.view
                                                        (Dict.get label errors
                                                            |> Maybe.map
                                                                (\err ->
                                                                    if Set.member ( form.title, label ) formModel.validated then
                                                                        [ W.Field.danger err ]

                                                                    else
                                                                        []
                                                                )
                                                            |> Maybe.withDefault []
                                                        )
                                                        { label = text label
                                                        , input =
                                                            W.InputText.view
                                                                [ W.InputText.readOnly (props.isReadOnly || f.attrs.readOnly model params resource)
                                                                , W.InputText.onBlur (SetValidatedField ( form.title, label ))
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
                                                    W.Field.view
                                                        (Dict.get label errors
                                                            |> Maybe.map (\error -> [ W.Field.danger error ])
                                                            |> Maybe.withDefault []
                                                        )
                                                        { label = text label
                                                        , input =
                                                            W.InputAutocomplete.view
                                                                ([ W.InputAutocomplete.readOnly (props.isReadOnly || f.attrs.readOnly model params resource)
                                                                 , W.InputAutocomplete.onBlur (SetValidatedField ( form.title, label ))
                                                                 ]
                                                                    ++ (f.attrs.onEnter
                                                                            |> Maybe.map (\fn -> [ W.InputAutocomplete.onEnter (GotMsg (fn model params resource search)) ])
                                                                            |> Maybe.withDefault []
                                                                       )
                                                                )
                                                                { id = label
                                                                , search = search
                                                                , value = value
                                                                , options = f.options model params
                                                                , toLabel = identity
                                                                , onInput =
                                                                    \search_ value_ ->
                                                                        case f.attrs.onSearch of
                                                                            Just onSearch ->
                                                                                Batch
                                                                                    [ ( search_, value_ )
                                                                                        |> ElmAdmin.Internal.Form.FieldValueAutocomplete
                                                                                        |> UpdateFormField ( form.title, label )
                                                                                    , GotMsg (onSearch model params resource search_)
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
                                                    W.Field.view []
                                                        { label = text label
                                                        , input =
                                                            W.InputCheckbox.view
                                                                [ W.InputCheckbox.readOnly (props.isReadOnly || f.attrs.readOnly model params resource)
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
                                                    W.Field.view []
                                                        { label = text label
                                                        , input =
                                                            W.InputRadio.view
                                                                [ W.InputRadio.readOnly (props.isReadOnly || f.attrs.readOnly model params resource)
                                                                ]
                                                                { id = form.title ++ "-" ++ label
                                                                , value = v
                                                                , options = f.options model params
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
                                                    W.Field.view []
                                                        { label = text label
                                                        , input =
                                                            W.InputSelect.view
                                                                [ W.InputSelect.readOnly (props.isReadOnly || f.attrs.readOnly model params resource)
                                                                ]
                                                                { value = v
                                                                , options = f.options model params
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
                                                    W.Field.view []
                                                        { label = text label
                                                        , input =
                                                            W.InputSlider.view
                                                                [ W.InputSlider.readOnly (props.isReadOnly || f.attrs.readOnly model params resource)
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
                            [ W.Button.view [ W.Button.primary ]
                                { label = "Confirm"
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
            [ W.Loading.circles [ W.Loading.size 32 ] ]
        ]


ifVisible : (model -> params -> resource -> Bool) -> model -> params -> resource -> (() -> Html (Msg msg)) -> Html (Msg msg)
ifVisible hidden_ m p r html =
    if hidden_ m p r then
        text ""

    else
        html ()
