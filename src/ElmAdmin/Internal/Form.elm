module ElmAdmin.Internal.Form exposing
    ( Field(..)
    , FieldValue(..)
    , Fields
    , FieldsBuilder
    , FormModel
    , TextFieldOptions
    , checkboxField
    , empty
    , fields
    , initFields
    , rangeField
    , textField
    )

import Dict exposing (Dict)


type alias FormModel =
    { initialized : Bool
    , values : Dict String FieldValue
    }


type FieldValue
    = FieldValueString String
    | FieldValueFloat Float
    | FieldValueBool Bool


type alias Fields r =
    FieldsBuilder r r


type alias FieldsBuilder resource a =
    { fields : List ( String, Field resource )
    , resolver : FormModel -> Maybe a
    }


fields : a -> FieldsBuilder resource a
fields a =
    { fields = []
    , resolver = \_ -> Just a
    }


initFields : resource -> Fields resource -> FormModel
initFields resource form_ =
    { initialized = True
    , values =
        form_.fields
            |> List.map
                (Tuple.mapSecond
                    (\field ->
                        case field of
                            TextField { fromResource } ->
                                FieldValueString <| fromResource resource

                            CheckboxField { fromResource } ->
                                FieldValueBool <| fromResource resource

                            RangeField { fromResource } ->
                                FieldValueFloat <| fromResource resource
                    )
                )
            |> Dict.fromList
    }


empty : FormModel
empty =
    { initialized = False
    , values = Dict.empty
    }



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
        { fromResource : resource -> String
        , options : TextFieldOptions
        }
    | RangeField
        { fromResource : resource -> Float
        , options : RangeFieldOptions
        }
    | CheckboxField
        { fromResource : resource -> Bool
        , options : CheckboxOptions
        }


textField :
    String
    -> (resource -> String)
    -> List (TextFieldOptions -> TextFieldOptions)
    -> FieldsBuilder resource (String -> a)
    -> FieldsBuilder resource a
textField label fromResource options_ f =
    let
        options =
            List.foldl (\fn a -> fn a) textFieldDefaults options_
    in
    { fields =
        ( label
        , TextField
            { fromResource = fromResource
            , options = options
            }
        )
            :: f.fields
    , resolver =
        \formModel ->
            f.resolver formModel
                |> Maybe.andThen
                    (\resolver ->
                        case Dict.get label formModel.values of
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
    -> FieldsBuilder resource (Bool -> a)
    -> FieldsBuilder resource a
checkboxField label fromResource options_ f =
    let
        options =
            List.foldl (\fn a -> fn a) checkboxFieldDefaults options_
    in
    { fields =
        ( label
        , CheckboxField
            { fromResource = fromResource
            , options = options
            }
        )
            :: f.fields
    , resolver =
        \formModel ->
            f.resolver formModel
                |> Maybe.andThen
                    (\resolver ->
                        case Dict.get label formModel.values of
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
    -> FieldsBuilder resource (Float -> a)
    -> FieldsBuilder resource a
rangeField label fromResource options_ f =
    let
        options =
            List.foldl (\fn a -> fn a) rangeFieldDefaults options_
    in
    { fields =
        ( label
        , RangeField
            { fromResource = fromResource
            , options = options
            }
        )
            :: f.fields
    , resolver =
        \formModel ->
            f.resolver formModel
                |> Maybe.andThen
                    (\resolver ->
                        case Dict.get label formModel.values of
                            Just (FieldValueFloat v) ->
                                Just (resolver v)

                            _ ->
                                Nothing
                    )
    }
