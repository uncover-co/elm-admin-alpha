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


view : FormModel -> model -> Form model msg resource -> (resource -> msg) -> Html (Msg msg)
view formModel model form onSubmit =
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
                |> List.map
                    (\( label, field ) ->
                        case ( field, Dict.get label formModel.values ) of
                            ( TextField _, Just (FieldValueString v) ) ->
                                W.field []
                                    { label = text label
                                    , input =
                                        W.textInput []
                                            { value = v
                                            , onInput =
                                                ElmAdmin.Internal.Form.FieldValueString
                                                    >> UpdateFormField label
                                            }
                                    }

                            ( AutocompleteField props, Just (FieldValueAutocomplete ( search, value )) ) ->
                                W.field []
                                    { label = text label
                                    , input =
                                        W.autocomplete []
                                            { id = label
                                            , search = search
                                            , value = value
                                            , options = props.options model
                                            , toLabel = identity
                                            , onInput =
                                                \search_ value_ ->
                                                    ( search_, value_ )
                                                        |> ElmAdmin.Internal.Form.FieldValueAutocomplete
                                                        |> UpdateFormField label
                                            , onEnter =
                                                props.attrs.onEnter
                                                    |> Maybe.map (\fn -> GotMsg (fn search))
                                            }
                                    }

                            ( CheckboxField _, Just (FieldValueBool v) ) ->
                                W.field []
                                    { label = text label
                                    , input =
                                        W.checkbox []
                                            { value = v
                                            , onInput =
                                                ElmAdmin.Internal.Form.FieldValueBool
                                                    >> UpdateFormField label
                                            }
                                    }

                            ( RangeField { options }, Just (FieldValueFloat v) ) ->
                                W.field []
                                    { label = text label
                                    , input =
                                        W.rangeInput []
                                            { value = v
                                            , min = options.min
                                            , max = options.max
                                            , step = options.step
                                            , onInput =
                                                ElmAdmin.Internal.Form.FieldValueFloat
                                                    >> UpdateFormField label
                                            }
                                    }

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


viewLoading : Form model msg resource -> Html (Msg msg)
viewLoading form =
    Html.div [ class "eadm eadm-card" ]
        [ p [ class "eadm eadm-form-title" ] [ text form.title ]
        , section
            [ class "eadm eadm-form-loading" ]
            [ W.loadingCircle [ WA.size 32 ] ]
        ]
