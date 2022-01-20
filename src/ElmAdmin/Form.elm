module ElmAdmin.Form exposing
    ( fields, Fields, Field
    , textField, TextFieldOptions
    , checkboxField, CheckboxFieldOptions
    , rangeField, RangeFieldOptions
    )

{-|

@docs fields, Fields, Field

@docs textField, TextFieldOptions

@docs checkboxField, CheckboxFieldOptions

@docs rangeField, RangeFieldOptions

-}

import ElmAdmin.Internal.Form exposing (Field(..), FieldValue(..), FieldsBuilder)


{-| -}
type alias Field resource =
    ElmAdmin.Internal.Form.Field resource


{-| -}
type alias Fields resource =
    ElmAdmin.Internal.Form.Fields resource


{-| -}
fields : a -> FieldsBuilder resource a
fields =
    ElmAdmin.Internal.Form.fields



-- TextField


{-| -}
type alias TextFieldOptions =
    ElmAdmin.Internal.Form.TextFieldOptions


{-| -}
textField :
    String
    -> (resource -> String)
    -> List (TextFieldOptions -> TextFieldOptions)
    -> FieldsBuilder resource (String -> a)
    -> FieldsBuilder resource a
textField =
    ElmAdmin.Internal.Form.textField



-- CheckboxField


{-| -}
type alias CheckboxFieldOptions =
    { required : Bool
    }


{-| -}
checkboxField :
    String
    -> (resource -> Bool)
    -> List (CheckboxFieldOptions -> CheckboxFieldOptions)
    -> FieldsBuilder resource (Bool -> a)
    -> FieldsBuilder resource a
checkboxField =
    ElmAdmin.Internal.Form.checkboxField



-- RangeField


{-| -}
type alias RangeFieldOptions =
    { required : Bool
    , min : Float
    , max : Float
    , step : Float
    }


{-| -}
rangeField :
    String
    -> (resource -> Float)
    -> List (RangeFieldOptions -> RangeFieldOptions)
    -> FieldsBuilder resource (Float -> a)
    -> FieldsBuilder resource a
rangeField =
    ElmAdmin.Internal.Form.rangeField
