module ElmAdmin.Form exposing
    ( form, Form, Field
    , textField, TextFieldOptions
    , checkboxField, CheckboxFieldOptions
    , rangeField, RangeFieldOptions
    )

{-|

@docs form, Form, Field

@docs textField, TextFieldOptions

@docs checkboxField, CheckboxFieldOptions

@docs rangeField, RangeFieldOptions

-}

import ElmAdmin.Internal.Form exposing (Field(..), FieldValue(..), FormBuilder)


{-| -}
type alias Field resource =
    ElmAdmin.Internal.Form.Field resource


{-| -}
type alias Form resource =
    ElmAdmin.Internal.Form.Form resource


{-| -}
form : a -> FormBuilder resource a
form =
    ElmAdmin.Internal.Form.form



-- TextField


{-| -}
type alias TextFieldOptions =
    ElmAdmin.Internal.Form.TextFieldOptions


{-| -}
textField :
    String
    -> (resource -> String)
    -> List (TextFieldOptions -> TextFieldOptions)
    -> FormBuilder resource (String -> a)
    -> FormBuilder resource a
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
    -> FormBuilder resource (Bool -> a)
    -> FormBuilder resource a
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
    -> FormBuilder resource (Float -> a)
    -> FormBuilder resource a
rangeField =
    ElmAdmin.Internal.Form.rangeField
