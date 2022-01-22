module ElmAdmin.UI.Form exposing (..)

import Dict
import ElmAdmin.Internal.Form exposing (Field(..), FieldValue(..), Fields, FormModel)
import ElmAdmin.Shared exposing (Msg(..))
import ElmWidgets as W
import Html exposing (..)
import Html.Attributes exposing (..)


view : FormModel -> Fields resource -> Html (Msg msg)
view formModel fields =
    Html.article [ class "eadm eadm-card" ]
        [ ul [ class "eadm eadm-form-fields" ]
            (fields.fields
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
                    , onClick = SubmitForm
                    }
                ]
            ]
        ]
