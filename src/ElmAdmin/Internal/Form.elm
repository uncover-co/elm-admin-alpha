module ElmAdmin.Internal.Form exposing
    ( Field(..)
    , FieldValue(..)
    , Form
    , FormBuilder
    , FormModel
    , TextFieldOptions
    , checkboxField
    , empty
    , form
    , initFields
    , rangeField
    , textField
    )

import Dict exposing (Dict)
import Set exposing (Set)


type alias FormModel =
    { initialized : Set String
    , values : Dict String FieldValue
    }


type FieldValue
    = FieldValueString String
    | FieldValueFloat Float
    | FieldValueBool Bool


type alias Form r =
    FormBuilder r r


type alias FormBuilder resource a =
    { id : String
    , fields : List ( String, Field resource )
    , resolver : FormModel -> Maybe a
    }


form : a -> FormBuilder resource a
form a =
    { id = ""
    , fields = []
    , resolver = \_ -> Just a
    }


initFields : resource -> Form resource -> FormModel -> FormModel
initFields resource form_ formModel =
    { initialized =
        Set.insert form_.id formModel.initialized
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
    { initialized = Set.empty
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
    -> FormBuilder resource (String -> a)
    -> FormBuilder resource a
textField label fromResource options_ f =
    let
        options =
            List.foldl (\fn a -> fn a) textFieldDefaults options_
    in
    { id = label ++ f.id
    , fields =
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
    -> FormBuilder resource (Bool -> a)
    -> FormBuilder resource a
checkboxField label fromResource options_ f =
    let
        options =
            List.foldl (\fn a -> fn a) checkboxFieldDefaults options_
    in
    { id = label ++ f.id
    , fields =
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
    -> FormBuilder resource (Float -> a)
    -> FormBuilder resource a
rangeField label fromResource options_ f =
    let
        options =
            List.foldl (\fn a -> fn a) rangeFieldDefaults options_
    in
    { id = label ++ f.id
    , fields =
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
