module Admin.UI.Form exposing (..)

import Admin.Internal.Form exposing (Field(..), FieldValue(..), Form, FormModel)
import Admin.Shared exposing (Effect(..), Msg(..))
import Dict
import Html as H exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
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
    { formId : String
    , model : model
    , params : params
    , form : Form model msg params resource
    , isLoading : Bool
    , isHidden : Bool
    , isReadOnly : Bool
    , onSubmit : model -> params -> resource -> msg
    }
    -> Maybe FormModel
    -> Html (Msg msg)
view props formModel =
    if props.isHidden then
        text ""

    else if props.isLoading then
        viewLoading props.form

    else
        formModel
            |> Maybe.map (viewForm props)
            |> Maybe.withDefault (text "")


viewForm :
    { formId : String
    , model : model
    , params : params
    , form : Form model msg params resource
    , isLoading : Bool
    , isHidden : Bool
    , isReadOnly : Bool
    , onSubmit : model -> params -> resource -> msg
    }
    -> FormModel
    -> Html (Msg msg)
viewForm ({ model, params, form, onSubmit } as props) formModel =
    form.resolver formModel model params
        |> Maybe.map
            (\( resource, errors ) ->
                H.form
                    [ HA.class "eadm eadm-card"
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
                    [ p [ HA.class "eadm eadm-form-title" ] [ text form.title ]
                    , ul [ HA.class "eadm eadm-form-fields" ]
                        (form.fields
                            |> List.reverse
                            |> List.map
                                (\( label, field ) ->
                                    case ( field, Dict.get label formModel.values ) of
                                        ( Text f, Just (FieldValueString v) ) ->
                                            ifVisible
                                                f.attrs.hidden
                                                model
                                                params
                                                resource
                                                (\_ ->
                                                    W.Field.view
                                                        (Dict.get label errors
                                                            |> Maybe.map (\_ -> [])
                                                            |> Maybe.withDefault []
                                                        )
                                                        { label = text label
                                                        , input =
                                                            W.InputText.view
                                                                [ W.InputText.readOnly (props.isReadOnly || f.attrs.readOnly model params resource)
                                                                ]
                                                                { value = v
                                                                , onInput =
                                                                    Admin.Internal.Form.FieldValueString
                                                                        >> UpdateFormField props.formId label
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
                                                                (W.InputAutocomplete.readOnly (props.isReadOnly || f.attrs.readOnly model params resource)
                                                                    :: (f.attrs.onEnter
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
                                                                                        |> Admin.Internal.Form.FieldValueAutocomplete
                                                                                        |> UpdateFormField props.formId label
                                                                                    , GotMsg (onSearch model params resource search_)
                                                                                    ]

                                                                            Nothing ->
                                                                                ( search_, value_ )
                                                                                    |> Admin.Internal.Form.FieldValueAutocomplete
                                                                                    |> UpdateFormField props.formId label
                                                                }
                                                        }
                                                )

                                        ( RemoteAutocomplete f, Just (FieldValueRemoteAutocomplete ( search, value )) ) ->
                                            ifVisible f.attrs.hidden
                                                model
                                                params
                                                resource
                                                (\_ ->
                                                    let
                                                        options : Maybe (List { id : String, label : String })
                                                        options =
                                                            Dict.get ( label, search ) formModel.remoteAutocomplete
                                                                |> Maybe.map Result.toMaybe
                                                                |> Maybe.withDefault (value |> Maybe.map List.singleton)
                                                    in
                                                    W.Field.view
                                                        (Dict.get label errors
                                                            |> Maybe.map (\error -> [ W.Field.danger error ])
                                                            |> Maybe.withDefault []
                                                        )
                                                        { label = text label
                                                        , input =
                                                            W.InputAutocomplete.view
                                                                [ W.InputAutocomplete.readOnly
                                                                    (props.isReadOnly || f.attrs.readOnly model params resource)
                                                                ]
                                                                { id = label
                                                                , search = search
                                                                , value = value
                                                                , options = options
                                                                , toLabel = .label
                                                                , onInput =
                                                                    \search_ value__ ->
                                                                        if String.length search_ >= 3 then
                                                                            Batch
                                                                                [ FetchAutocompleteOptions
                                                                                    props.formId
                                                                                    label
                                                                                    search_
                                                                                    (f.searchRequest model params search_)
                                                                                , ( search_, value__ )
                                                                                    |> Admin.Internal.Form.FieldValueRemoteAutocomplete
                                                                                    |> UpdateFormField props.formId label
                                                                                ]

                                                                        else
                                                                            ( search_, value__ )
                                                                                |> Admin.Internal.Form.FieldValueRemoteAutocomplete
                                                                                |> UpdateFormField props.formId label
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
                                                                    Admin.Internal.Form.FieldValueBool
                                                                        >> UpdateFormField props.formId label
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
                                                                    Admin.Internal.Form.FieldValueString
                                                                        >> UpdateFormField props.formId label
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
                                                                    Admin.Internal.Form.FieldValueString
                                                                        >> UpdateFormField props.formId label
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
                                                                    Admin.Internal.Form.FieldValueFloat
                                                                        >> UpdateFormField props.formId label
                                                                }
                                                        }
                                                )

                                        _ ->
                                            text ""
                                )
                        )
                    , section [ HA.class "eadm eadm-form-footer" ]
                        [ div [ HA.class "eadm eadm-form-footer-inner" ]
                            [ W.Button.view [ W.Button.primary ]
                                { label = H.text "Confirm"
                                , onClick = DoNothing
                                }
                            ]
                        ]
                    ]
            )
        |> Maybe.withDefault (text "")


viewLoading : Form model msg params resource -> Html (Msg msg)
viewLoading form =
    H.div [ HA.class "eadm eadm-card" ]
        [ p [ HA.class "eadm eadm-form-title" ] [ text form.title ]
        , section
            [ HA.class "eadm eadm-form-loading" ]
            [ W.Loading.circles [ W.Loading.size 32 ] ]
        ]


ifVisible : (model -> params -> resource -> Bool) -> model -> params -> resource -> (() -> Html (Msg msg)) -> Html (Msg msg)
ifVisible hidden_ m p r html =
    if hidden_ m p r then
        text ""

    else
        html ()
