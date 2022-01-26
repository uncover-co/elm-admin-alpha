module ElmAdmin.Form exposing
    ( form, Form, Field
    , textField, TextFieldOptions
    , autocompleteField, AutocompleteFieldAttributes
    , checkboxField, CheckboxFieldOptions
    , rangeField, RangeFieldOptions
    )

{-|

@docs form, Form, Field

@docs textField, TextFieldOptions

@docs autocompleteField, AutocompleteFieldAttributes

@docs checkboxField, CheckboxFieldOptions

@docs rangeField, RangeFieldOptions

-}

import ElmAdmin.Internal.Form exposing (Field(..), FieldValue(..), FormBuilder)


{-| -}
type alias Field model msg resource =
    ElmAdmin.Internal.Form.Field model msg resource


{-| -}
type alias Form model msg resource =
    ElmAdmin.Internal.Form.Form model msg resource


{-| -}
form : String -> a -> FormBuilder model msg resource a
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
    -> FormBuilder model msg resource (String -> a)
    -> FormBuilder model msg resource a
textField =
    ElmAdmin.Internal.Form.textField



-- AutocompleteField


{-| -}
type alias AutocompleteFieldAttributes msg =
    ElmAdmin.Internal.Form.AutocompleteFieldAttributes msg


{-| -}
autocompleteField :
    { label : String
    , value : resource -> Maybe x
    , options : model -> Maybe (List x)
    , optionToLabel : x -> String
    , attrs : List (AutocompleteFieldAttributes msg -> AutocompleteFieldAttributes msg)
    }
    -> FormBuilder model msg resource (Maybe x -> a)
    -> FormBuilder model msg resource a
autocompleteField =
    ElmAdmin.Internal.Form.autocompleteField



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
    -> FormBuilder model msg resource (Bool -> a)
    -> FormBuilder model msg resource a
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
    -> FormBuilder model msg resource (Float -> a)
    -> FormBuilder model msg resource a
rangeField =
    ElmAdmin.Internal.Form.rangeField
