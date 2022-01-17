module ElmAdmin.Form exposing
    ( Form
    , FormModel
    , TextFieldOptions
    , checkboxField
    , empty
    , form
    , init
    , rangeField
    , textField
    )

import Dict exposing (Dict)


type alias FormModel =
    Dict String FieldValue


type FieldValue
    = FieldValueString String
    | FieldValueFloat Float
    | FieldValueBool Bool


type alias Form r =
    FormBuilder r r


type alias FormBuilder resource a =
    { fields : List (Field resource)
    , resolver : FormModel -> Maybe a
    }


form : a -> FormBuilder resource a
form a =
    { fields = []
    , resolver = \_ -> Just a
    }


init : resource -> Form resource -> FormModel
init resource form_ =
    form_.fields
        |> List.map
            (\field ->
                case field of
                    TextField { label, fromResource } ->
                        ( label, FieldValueString <| fromResource resource )

                    CheckboxField { label, fromResource } ->
                        ( label, FieldValueBool <| fromResource resource )

                    RangeField { label, fromResource } ->
                        ( label, FieldValueFloat <| fromResource resource )
            )
        |> Dict.fromList


empty : FormModel
empty =
    Dict.empty



-- TextField


type alias TextFieldOptions =
    { required : Bool
    }


textFieldDefaults : TextFieldOptions
textFieldDefaults =
    { required = True
    }


type Field resource
    = TextField
        { label : String
        , fromResource : resource -> String
        , options : TextFieldOptions
        }
    | RangeField
        { label : String
        , fromResource : resource -> Float
        , options : RangeFieldOptions
        }
    | CheckboxField
        { label : String
        , fromResource : resource -> Bool
        , options : CheckboxOptions
        }


textField :
    String
    -> (resource -> String)
    -> List (TextFieldOptions -> TextFieldOptions)
    -> FormBuilder resource (String -> a)
    -> FormBuilder resource a
textField label fromResource options_ f =
    let
        options =
            List.foldl (\fn a -> fn a) textFieldDefaults options_
    in
    { fields =
        TextField
            { label = label
            , fromResource = fromResource
            , options = options
            }
            :: f.fields
    , resolver =
        \fields ->
            f.resolver fields
                |> Maybe.andThen
                    (\resolver ->
                        case Dict.get label fields of
                            Just (FieldValueString v) ->
                                Just (resolver v)

                            _ ->
                                Nothing
                    )
    }



-- CheckboxField


type alias CheckboxOptions =
    { required : Bool
    }


checkboxFieldDefaults : CheckboxOptions
checkboxFieldDefaults =
    { required = True
    }


checkboxField :
    String
    -> (resource -> Bool)
    -> List (CheckboxOptions -> CheckboxOptions)
    -> FormBuilder resource (Bool -> a)
    -> FormBuilder resource a
checkboxField label fromResource options_ f =
    let
        options =
            List.foldl (\fn a -> fn a) checkboxFieldDefaults options_
    in
    { fields =
        CheckboxField
            { label = label
            , fromResource = fromResource
            , options = options
            }
            :: f.fields
    , resolver =
        \fields ->
            f.resolver fields
                |> Maybe.andThen
                    (\resolver ->
                        case Dict.get label fields of
                            Just (FieldValueBool v) ->
                                Just (resolver v)

                            _ ->
                                Nothing
                    )
    }



-- RangeField


type alias RangeFieldOptions =
    { required : Bool
    , min : Float
    , max : Float
    , step : Float
    }


rangeFieldDefaults : RangeFieldOptions
rangeFieldDefaults =
    { required = True
    , min = 0
    , max = 10
    , step = 1
    }


rangeField :
    String
    -> (resource -> Float)
    -> List (RangeFieldOptions -> RangeFieldOptions)
    -> FormBuilder resource (Float -> a)
    -> FormBuilder resource a
rangeField label fromResource options_ f =
    let
        options =
            List.foldl (\fn a -> fn a) rangeFieldDefaults options_
    in
    { fields =
        RangeField
            { label = label
            , fromResource = fromResource
            , options = options
            }
            :: f.fields
    , resolver =
        \fields ->
            f.resolver fields
                |> Maybe.andThen
                    (\resolver ->
                        case Dict.get label fields of
                            Just (FieldValueFloat v) ->
                                Just (resolver v)

                            _ ->
                                Nothing
                    )
    }
