module Admin.Form exposing
    ( form, Form, FormField
    , text, TextAttributes
    , autocomplete, AutocompleteAttributes
    , remoteAutocomplete, RemoteAutocompleteAttributes
    , checkbox, CheckboxAttributes
    , radio, RadioAttributes
    , select, SelectAttributes
    , range, RangeAttributes
    , readOnly, readOnlyIf, required, hidden, hiddenIf, onEnter, onSearch
    )

{-|

@docs form, Form, FormField


# Form Fields


## Text

@docs text, TextAttributes


## Autocomplete

@docs autocomplete, AutocompleteAttributes


## Remote Autocomplete

@docs remoteAutocomplete, RemoteAutocompleteAttributes


## Checkbox

@docs checkbox, CheckboxAttributes


## Radio

@docs radio, RadioAttributes


## Select

@docs select, SelectAttributes


## Range

@docs range, RangeAttributes


# Attributes

@docs readOnly, readOnlyIf, required, hidden, hiddenIf, onEnter, onSearch

-}

import Admin.Internal.Form exposing (Field(..), FieldValue(..), FormBuilder)
import Admin.Libs.List
import Dict
import Http
import Platform exposing (Task)


{-| -}
type alias Form model msg params resource =
    Admin.Internal.Form.Form model msg params resource


{-| -}
type alias FormField model msg params resource a b =
    FormBuilder model msg params resource (a -> b)
    -> FormBuilder model msg params resource b


{-| -}
form : String -> a -> FormBuilder model msg params resource a
form title a =
    { title = title
    , fields = []
    , resolver = \_ _ _ -> Just ( a, Dict.empty )
    }



-- TextField


{-| -}
type alias TextAttributes model params resource =
    { required : Bool
    , hidden : model -> params -> resource -> Bool
    , readOnly : model -> params -> resource -> Bool
    }


textDefaults : TextAttributes model params resource
textDefaults =
    { required = True
    , hidden = \_ _ _ -> False
    , readOnly = \_ _ _ -> False
    }


{-| -}
text :
    String
    -> (resource -> String)
    -> List (TextAttributes model params resource -> TextAttributes model params resource)
    -> FormField model msg params resource String a
text label value attrs_ f =
    let
        attrs =
            List.foldl (\fn a -> fn a) textDefaults attrs_
    in
    { title = f.title
    , fields =
        ( label
        , Text
            { value = value
            , attrs = attrs
            }
        )
            :: f.fields
    , resolver =
        \formModel model params ->
            f.resolver formModel model params
                |> Maybe.andThen
                    (\( resolver, errors ) ->
                        case Dict.get label formModel.values of
                            Just (FieldValueString v) ->
                                if attrs.required && v == "" then
                                    Just ( resolver v, Dict.insert label "Can't be blank" errors )

                                else
                                    Just ( resolver v, errors )

                            _ ->
                                Nothing
                    )
    }



-- AutocompleteField


{-| -}
type alias AutocompleteAttributes model msg params resource =
    { required : Bool
    , hidden : model -> params -> resource -> Bool
    , readOnly : model -> params -> resource -> Bool
    , onEnter : Maybe (model -> params -> resource -> String -> msg)
    , onSearch : Maybe (model -> params -> resource -> String -> msg)
    }


autocompleteDefaults : AutocompleteAttributes model msg params resource
autocompleteDefaults =
    { required = False
    , hidden = \_ _ _ -> False
    , readOnly = \_ _ _ -> False
    , onEnter = Nothing
    , onSearch = Nothing
    }


{-| -}
autocomplete :
    { label : String
    , value : resource -> Maybe x
    , options : model -> params -> Maybe (List x)
    , optionToLabel : x -> String
    , attrs : List (AutocompleteAttributes model msg params resource -> AutocompleteAttributes model msg params resource)
    }
    -> FormField model msg params resource (Maybe x) a
autocomplete props f =
    let
        attrs : AutocompleteAttributes model msg params resource
        attrs =
            List.foldl (\fn a -> fn a) autocompleteDefaults props.attrs

        value : resource -> Maybe String
        value resource =
            props.value resource
                |> Maybe.map props.optionToLabel

        options : model -> params -> Maybe (List String)
        options model params =
            props.options model params
                |> Maybe.map (List.map props.optionToLabel)
    in
    { title = f.title
    , fields =
        ( props.label
        , Autocomplete
            { value = value
            , options = options
            , attrs = attrs
            }
        )
            :: f.fields
    , resolver =
        \formModel model params ->
            f.resolver formModel model params
                |> Maybe.andThen
                    (\( resolver, errors ) ->
                        case Dict.get props.label formModel.values of
                            Just (FieldValueAutocomplete ( _, v )) ->
                                let
                                    value_ =
                                        case ( v, props.options model params ) of
                                            ( Just v_, Just options_ ) ->
                                                Admin.Libs.List.find
                                                    (\option -> props.optionToLabel option == v_)
                                                    options_

                                            _ ->
                                                Nothing

                                    errors_ =
                                        if attrs.required && value_ == Nothing then
                                            Dict.insert props.label "Selecting a valid option is required." errors

                                        else
                                            errors
                                in
                                Just ( resolver value_, errors_ )

                            _ ->
                                Nothing
                    )
    }



-- RemoteAutocompleteField


{-| -}
type alias RemoteAutocompleteAttributes model params resource =
    { required : Bool
    , hidden : model -> params -> resource -> Bool
    , readOnly : model -> params -> resource -> Bool
    }


remoteAutocompleteDefaults : RemoteAutocompleteAttributes model params resource
remoteAutocompleteDefaults =
    { required = False
    , hidden = \_ _ _ -> False
    , readOnly = \_ _ _ -> False
    }


{-| -}
remoteAutocomplete :
    { label : String
    , value : resource -> Maybe String
    , initRequest : model -> params -> String -> Task Http.Error { id : String, label : String }
    , searchRequest : model -> params -> String -> Task Http.Error (List { id : String, label : String })
    , attrs : List (RemoteAutocompleteAttributes model params resource -> RemoteAutocompleteAttributes model params resource)
    }
    -> FormField model msg params resource (Maybe String) a
remoteAutocomplete props f =
    let
        attrs : RemoteAutocompleteAttributes model params resource
        attrs =
            List.foldl (\fn a -> fn a) remoteAutocompleteDefaults props.attrs
    in
    { title = f.title
    , fields =
        ( props.label
        , RemoteAutocomplete
            { value = props.value
            , initRequest = props.initRequest
            , searchRequest = props.searchRequest
            , attrs = attrs
            }
        )
            :: f.fields
    , resolver =
        \formModel model params ->
            f.resolver formModel model params
                |> Maybe.andThen
                    (\( resolver, errors ) ->
                        case Dict.get props.label formModel.values of
                            Just (FieldValueRemoteAutocomplete ( _, v )) ->
                                Just ( resolver (Maybe.map .id v), errors )

                            _ ->
                                Nothing
                    )
    }



-- CheckboxField


{-| -}
type alias CheckboxAttributes model params resource =
    { hidden : model -> params -> resource -> Bool
    , readOnly : model -> params -> resource -> Bool
    }


checkboxDefaults : CheckboxAttributes model params resource
checkboxDefaults =
    { hidden = \_ _ _ -> False
    , readOnly = \_ _ _ -> False
    }


{-| -}
checkbox :
    String
    -> (resource -> Bool)
    -> List (CheckboxAttributes model params resource -> CheckboxAttributes model params resource)
    -> FormField model msg params resource Bool a
checkbox label value attrs_ f =
    let
        attrs =
            List.foldl (\fn a -> fn a) checkboxDefaults attrs_
    in
    { title = f.title
    , fields =
        ( label
        , Checkbox
            { value = value
            , attrs = attrs
            }
        )
            :: f.fields
    , resolver =
        \formModel model params ->
            f.resolver formModel model params
                |> Maybe.andThen
                    (\( resolver, errors ) ->
                        case Dict.get label formModel.values of
                            Just (FieldValueBool v) ->
                                Just ( resolver v, errors )

                            _ ->
                                Nothing
                    )
    }



-- Radio


{-| -}
type alias RadioAttributes model params resource =
    { hidden : model -> params -> resource -> Bool
    , readOnly : model -> params -> resource -> Bool
    }


radioDefaults : RadioAttributes model params resource
radioDefaults =
    { hidden = \_ _ _ -> False
    , readOnly = \_ _ _ -> False
    }


{-| -}
radio :
    { label : String
    , value : resource -> x
    , options : model -> params -> List x
    , optionToLabel : x -> String
    , attrs : List (RadioAttributes model params resource -> RadioAttributes model params resource)
    }
    -> FormField model msg params resource x a
radio props f =
    let
        attrs =
            List.foldl (\fn a -> fn a) radioDefaults props.attrs
    in
    { title = f.title
    , fields =
        ( props.label
        , Radio
            { value = props.value >> props.optionToLabel
            , options = \model params -> props.options model params |> List.map props.optionToLabel
            , attrs = attrs
            }
        )
            :: f.fields
    , resolver =
        \formModel model params ->
            f.resolver formModel model params
                |> Maybe.andThen
                    (\( resolver, errors ) ->
                        case Dict.get props.label formModel.values of
                            Just (FieldValueString v) ->
                                props.options model params
                                    |> Admin.Libs.List.find
                                        (\option -> props.optionToLabel option == v)
                                    |> Maybe.map (\v_ -> ( resolver v_, errors ))

                            _ ->
                                Nothing
                    )
    }



-- Radio


{-| -}
type alias SelectAttributes model params resource =
    { hidden : model -> params -> resource -> Bool
    , readOnly : model -> params -> resource -> Bool
    }


selectDefaults : SelectAttributes model params resource
selectDefaults =
    { hidden = \_ _ _ -> False
    , readOnly = \_ _ _ -> False
    }


{-| -}
select :
    { label : String
    , value : resource -> x
    , options : model -> params -> List x
    , optionToLabel : x -> String
    , attrs : List (SelectAttributes model params resource -> SelectAttributes model params resource)
    }
    -> FormField model msg params resource x a
select props f =
    let
        attrs =
            List.foldl (\fn a -> fn a) selectDefaults props.attrs
    in
    { title = f.title
    , fields =
        ( props.label
        , Select
            { value = props.value >> props.optionToLabel
            , options = \model params -> props.options model params |> List.map props.optionToLabel
            , attrs = attrs
            }
        )
            :: f.fields
    , resolver =
        \formModel model params ->
            f.resolver formModel model params
                |> Maybe.andThen
                    (\( resolver, errors ) ->
                        case Dict.get props.label formModel.values of
                            Just (FieldValueString v) ->
                                props.options model params
                                    |> Admin.Libs.List.find
                                        (\option -> props.optionToLabel option == v)
                                    |> Maybe.map (\v_ -> ( resolver v_, errors ))

                            _ ->
                                Nothing
                    )
    }



-- RangeField


{-| -}
type alias RangeAttributes model params resource =
    { hidden : model -> params -> resource -> Bool
    , readOnly : model -> params -> resource -> Bool
    }


rangeDefaults : RangeAttributes model params resource
rangeDefaults =
    { hidden = \_ _ _ -> False
    , readOnly = \_ _ _ -> False
    }


{-| -}
range :
    { label : String
    , value : resource -> Float
    , min : Float
    , max : Float
    , step : Float
    , attrs : List (RangeAttributes model params resource -> RangeAttributes model params resource)
    }
    -> FormField model msg params resource Float a
range props f =
    let
        attrs =
            List.foldl (\fn a -> fn a) rangeDefaults props.attrs
    in
    { title = f.title
    , fields =
        ( props.label
        , Range
            { value = props.value
            , min = props.min
            , max = props.max
            , step = props.step
            , attrs = attrs
            }
        )
            :: f.fields
    , resolver =
        \formModel model params ->
            f.resolver formModel model params
                |> Maybe.andThen
                    (\( resolver, errors ) ->
                        case Dict.get props.label formModel.values of
                            Just (FieldValueFloat v) ->
                                Just ( resolver v, errors )

                            _ ->
                                Nothing
                    )
    }



-- Attributes


{-| -}
hiddenIf :
    (model -> params -> resource -> Bool)
    -> { m | hidden : model -> params -> resource -> Bool }
    -> { m | hidden : model -> params -> resource -> Bool }
hiddenIf v a =
    { a | hidden = v }


{-| -}
hidden :
    { m | hidden : model -> params -> resource -> Bool }
    -> { m | hidden : model -> params -> resource -> Bool }
hidden a =
    { a | hidden = \_ _ _ -> True }


{-| -}
readOnlyIf :
    (model -> params -> resource -> Bool)
    -> { m | readOnly : model -> params -> resource -> Bool }
    -> { m | readOnly : model -> params -> resource -> Bool }
readOnlyIf v a =
    { a | readOnly = v }


{-| -}
readOnly :
    { m | readOnly : model -> params -> resource -> Bool }
    -> { m | readOnly : model -> params -> resource -> Bool }
readOnly a =
    { a | readOnly = \_ _ _ -> True }


{-| -}
required :
    { m | required : Bool }
    -> { m | required : Bool }
required a =
    { a | required = True }


{-| -}
onEnter : (model -> params -> resource -> String -> msg) -> { m | onEnter : Maybe (model -> params -> resource -> String -> msg) } -> { m | onEnter : Maybe (model -> params -> resource -> String -> msg) }
onEnter v a =
    { a | onEnter = Just v }


{-| -}
onSearch : (model -> params -> resource -> String -> msg) -> { m | onSearch : Maybe (model -> params -> resource -> String -> msg) } -> { m | onSearch : Maybe (model -> params -> resource -> String -> msg) }
onSearch v a =
    { a | onSearch = Just v }
