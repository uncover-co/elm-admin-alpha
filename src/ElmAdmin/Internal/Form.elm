module ElmAdmin.Internal.Form exposing
    ( Field(..)
    , FieldValue(..)
    , Form
    , FormBuilder
    , FormModel
    , empty
    , initFields
    )

import Dict exposing (Dict)
import Set exposing (Set)


type alias FormModel =
    { initialized : Set String
    , validated : Set ( String, String )
    , values : Dict ( String, String ) FieldValue
    }


type FieldValue
    = FieldValueString String
    | FieldValueFloat Float
    | FieldValueBool Bool
    | FieldValueAutocomplete ( String, Maybe String )


type alias Form model msg params resource =
    FormBuilder model msg params resource resource


type alias FormBuilder model msg params resource a =
    { title : String
    , fields : List ( String, Field model msg params resource )
    , resolver : FormModel -> model -> params -> Maybe ( a, Dict String String )
    }


type Field model msg params resource
    = Text
        { value : resource -> String
        , attrs :
            { required : Bool
            , hidden : model -> params -> resource -> Bool
            , readOnly : model -> params -> resource -> Bool
            }
        }
    | Autocomplete
        { value : resource -> Maybe String
        , options : model -> params -> Maybe (List String)
        , attrs :
            { required : Bool
            , hidden : model -> params -> resource -> Bool
            , readOnly : model -> params -> resource -> Bool
            , onSearch : Maybe (model -> params -> resource -> String -> msg)
            , onEnter : Maybe (model -> params -> resource -> String -> msg)
            }
        }
    | Checkbox
        { value : resource -> Bool
        , attrs :
            { hidden : model -> params -> resource -> Bool
            , readOnly : model -> params -> resource -> Bool
            }
        }
    | Select
        { value : resource -> String
        , options : model -> params -> List String
        , attrs :
            { hidden : model -> params -> resource -> Bool
            , readOnly : model -> params -> resource -> Bool
            }
        }
    | Radio
        { value : resource -> String
        , options : model -> params -> List String
        , attrs :
            { hidden : model -> params -> resource -> Bool
            , readOnly : model -> params -> resource -> Bool
            }
        }
    | Range
        { value : resource -> Float
        , min : Float
        , max : Float
        , step : Float
        , attrs :
            { hidden : model -> params -> resource -> Bool
            , readOnly : model -> params -> resource -> Bool
            }
        }


initFields : resource -> Form msg model params resource -> FormModel -> FormModel
initFields resource form_ formModel =
    { initialized =
        Set.insert form_.title formModel.initialized
    , validated = formModel.validated
    , values =
        form_.fields
            |> List.map
                (\( label, field ) ->
                    let
                        fieldValue =
                            case field of
                                Text { value } ->
                                    FieldValueString <| value resource

                                Autocomplete { value } ->
                                    value resource
                                        |> Maybe.map (\v -> FieldValueAutocomplete ( v, value resource ))
                                        |> Maybe.withDefault (FieldValueAutocomplete ( "", Nothing ))

                                Checkbox { value } ->
                                    FieldValueBool <| value resource

                                Radio { value } ->
                                    FieldValueString <| value resource

                                Select { value } ->
                                    FieldValueString <| value resource

                                Range { value } ->
                                    FieldValueFloat <| value resource
                    in
                    ( ( form_.title, label ), fieldValue )
                )
            |> Dict.fromList
            |> (\v_ -> Dict.union v_ formModel.values)
    }


empty : FormModel
empty =
    { initialized = Set.empty
    , validated = Set.empty
    , values = Dict.empty
    }
