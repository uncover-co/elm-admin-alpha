module Admin.Internal.Form exposing
    ( Field(..)
    , FieldValue(..)
    , Form
    , FormBuilder
    , FormModel
    , empty
    , fromValues
    )

import Dict exposing (Dict)
import Http
import Platform exposing (Task)


type alias FormModel =
    { values : Dict String FieldValue
    , remoteAutocomplete : Dict ( String, String ) (Result Http.Error (List { id : String, label : String }))
    }


empty : FormModel
empty =
    { remoteAutocomplete = Dict.empty
    , values = Dict.empty
    }


fromValues : Dict String FieldValue -> FormModel
fromValues values =
    { remoteAutocomplete = Dict.empty
    , values = values
    }


type FieldValue
    = FieldValueString String
    | FieldValueFloat Float
    | FieldValueBool Bool
    | FieldValueAutocomplete ( String, Maybe String )
    | FieldValueRemoteAutocomplete ( String, Maybe { id : String, label : String } )


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
    | RemoteAutocomplete
        { value : resource -> Maybe String
        , initRequest : model -> params -> String -> Task Http.Error { id : String, label : String }
        , searchRequest : model -> params -> String -> Task Http.Error (List { id : String, label : String })
        , attrs :
            { required : Bool
            , hidden : model -> params -> resource -> Bool
            , readOnly : model -> params -> resource -> Bool
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
