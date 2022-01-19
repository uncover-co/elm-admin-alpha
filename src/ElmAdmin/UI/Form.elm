module ElmAdmin.UI.Form exposing (..)

import Dict
import ElmAdmin.Form exposing (Field(..), FieldValue(..), Fields, FormModel)
import ElmAdmin.Shared exposing (Msg(..))
import ElmWidgets as W
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE


view : FormModel -> Fields resource -> Html (Msg msg)
view formModel fields =
    Html.form
        [ HE.onSubmit SubmitForm
        ]
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
                                            ElmAdmin.Form.FieldValueString
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
                                            ElmAdmin.Form.FieldValueBool
                                                >> UpdateFormField label
                                        }
                                }

                        _ ->
                            text ""
                )
        )
