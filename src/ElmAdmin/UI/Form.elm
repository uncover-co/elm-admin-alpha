module ElmAdmin.UI.Form exposing (..)

import Dict
import ElmAdmin.Internal.Form exposing (Field(..), FieldValue(..), Form, FormModel)
import ElmAdmin.Shared exposing (Msg(..))
import ElmWidgets as W
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit)


view : FormModel -> Form resource -> Html (Msg msg)
view formModel form =
    Html.form [ class "eadm eadm-card", onSubmit SubmitForm ]
        [ ul [ class "eadm eadm-form-fields" ]
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
