module ElmAdmin.Internal.Form exposing
    ( AutocompleteFieldAttributes
    , Field(..)
    , FieldValue(..)
    , Form
    , FormBuilder
    , FormModel
    , RadioButtonsFieldOptions
    , TextFieldOptions
    , autocompleteField
    , checkboxField
    , empty
    , form
    , initFields
    , radioButtonsField
    , rangeField
    , textField
    )

import Dict exposing (Dict)
import ElmAdmin.Libs.List
import Set exposing (Set)


type alias FormModel =
    { initialized : Set String
    , values : Dict String FieldValue
    }


type FieldValue
    = FieldValueString String
    | FieldValueFloat Float
    | FieldValueBool Bool
    | FieldValueAutocomplete ( String, Maybe String )


type alias Form model msg resource =
    FormBuilder model msg resource resource


type alias FormBuilder model msg resource a =
    { title : String
    , fields : List ( String, Field model msg resource )
    , resolver : FormModel -> model -> Maybe a
    }


form : String -> a -> FormBuilder model msg resource a
form title a =
    { title = title
    , fields = []
    , resolver = \_ _ -> Just a
    }


initFields : resource -> Form msg model resource -> FormModel -> FormModel
initFields resource form_ formModel =
    { initialized =
        Set.insert form_.title formModel.initialized
    , values =
        form_.fields
            |> List.map
                (Tuple.mapSecond
                    (\field ->
                        case field of
                            TextField { fromResource } ->
                                FieldValueString <| fromResource resource

                            AutocompleteField { fromResource } ->
                                fromResource resource
                                    |> Maybe.map (\v -> FieldValueAutocomplete ( v, fromResource resource ))
                                    |> Maybe.withDefault (FieldValueAutocomplete ( "", Nothing ))

                            CheckboxField { fromResource } ->
                                FieldValueBool <| fromResource resource

                            RangeField { fromResource } ->
                                FieldValueFloat <| fromResource resource

                            RadioButtonsField { fromResource } ->
                                FieldValueString <| fromResource resource
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


type Field model msg resource
    = TextField
        { fromResource : resource -> String
        , options : TextFieldOptions
        }
    | AutocompleteField
        { fromResource : resource -> Maybe String
        , options : model -> Maybe (List String)
        , attrs : AutocompleteFieldAttributes msg
        }
    | RangeField
        { fromResource : resource -> Float
        , options : RangeFieldOptions
        }
    | CheckboxField
        { fromResource : resource -> Bool
        , options : CheckboxOptions
        }
    | RadioButtonsField
        { fromResource : resource -> String
        , options : RadioButtonsFieldOptions
        }


textField :
    String
    -> (resource -> String)
    -> List (TextFieldOptions -> TextFieldOptions)
    -> FormBuilder model msg resource (String -> a)
    -> FormBuilder model msg resource a
textField label fromResource options_ f =
    let
        options =
            List.foldl (\fn a -> fn a) textFieldDefaults options_
    in
    { title = f.title
    , fields =
        ( label
        , TextField
            { fromResource = fromResource
            , options = options
            }
        )
            :: f.fields
    , resolver =
        \formModel model ->
            f.resolver formModel model
                |> Maybe.andThen
                    (\resolver ->
                        case Dict.get label formModel.values of
                            Just (FieldValueString v) ->
                                Just (resolver v)

                            _ ->
                                Nothing
                    )
    }



-- AutocompleteField


type alias AutocompleteFieldAttributes msg =
    { onEnter : Maybe (String -> msg) }


autocompleteFieldDefaults : AutocompleteFieldAttributes msg
autocompleteFieldDefaults =
    { onEnter = Nothing }


autocompleteField :
    { label : String
    , value : resource -> Maybe x
    , options : model -> Maybe (List x)
    , optionToLabel : x -> String
    , attrs : List (AutocompleteFieldAttributes msg -> AutocompleteFieldAttributes msg)
    }
    -> FormBuilder model msg resource (Maybe x -> a)
    -> FormBuilder model msg resource a
autocompleteField props f =
    let
        attrs : AutocompleteFieldAttributes msg
        attrs =
            List.foldl (\fn a -> fn a) autocompleteFieldDefaults props.attrs

        fromResource : resource -> Maybe String
        fromResource resource =
            props.value resource
                |> Maybe.map props.optionToLabel

        options : model -> Maybe (List String)
        options model =
            props.options model
                |> Maybe.map (List.map props.optionToLabel)
    in
    { title = f.title
    , fields =
        ( props.label
        , AutocompleteField
            { fromResource = fromResource
            , options = options
            , attrs = attrs
            }
        )
            :: f.fields
    , resolver =
        \formModel model ->
            f.resolver formModel model
                |> Maybe.andThen
                    (\resolver ->
                        case Dict.get props.label formModel.values of
                            Just (FieldValueAutocomplete ( _, v )) ->
                                let
                                    value =
                                        case ( v, props.options model ) of
                                            ( Just v_, Just options_ ) ->
                                                ElmAdmin.Libs.List.find
                                                    (\option -> props.optionToLabel option == v_)
                                                    options_

                                            _ ->
                                                Nothing
                                in
                                Just (resolver value)

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
    -> FormBuilder model msg resource (Bool -> a)
    -> FormBuilder model msg resource a
checkboxField label fromResource options_ f =
    let
        options =
            List.foldl (\fn a -> fn a) checkboxFieldDefaults options_
    in
    { title = f.title
    , fields =
        ( label
        , CheckboxField
            { fromResource = fromResource
            , options = options
            }
        )
            :: f.fields
    , resolver =
        \formModel model ->
            f.resolver formModel model
                |> Maybe.andThen
                    (\resolver ->
                        case Dict.get label formModel.values of
                            Just (FieldValueBool v) ->
                                Just (resolver v)

                            _ ->
                                Nothing
                    )
    }



-- RadioButtonsField


type alias RadioButtonsFieldOptions =
    {}


radioButtonsFieldDefaults : RadioButtonsFieldOptions
radioButtonsFieldDefaults =
    {}


radioButtonsField :
    String
    -> (resource -> String)
    -> List (RadioButtonsFieldOptions -> RadioButtonsFieldOptions)
    -> FormBuilder model msg resource (String -> a)
    -> FormBuilder model msg resource a
radioButtonsField label fromResource options_ f =
    let
        options =
            List.foldl (\fn a -> fn a) radioButtonsFieldDefaults options_
    in
    { title = f.title
    , fields =
        ( label
        , RadioButtonsField
            { fromResource = fromResource
            , options = options
            }
        )
            :: f.fields
    , resolver =
        \formModel model ->
            f.resolver formModel model
                |> Maybe.andThen
                    (\resolver ->
                        case Dict.get label formModel.values of
                            Just (FieldValueString v) ->
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
    -> FormBuilder model msg resource (Float -> a)
    -> FormBuilder model msg resource a
rangeField label fromResource options_ f =
    let
        options =
            List.foldl (\fn a -> fn a) rangeFieldDefaults options_
    in
    { title = f.title
    , fields =
        ( label
        , RangeField
            { fromResource = fromResource
            , options = options
            }
        )
            :: f.fields
    , resolver =
        \formModel model ->
            f.resolver formModel model
                |> Maybe.andThen
                    (\resolver ->
                        case Dict.get label formModel.values of
                            Just (FieldValueFloat v) ->
                                Just (resolver v)

                            _ ->
                                Nothing
                    )
    }
