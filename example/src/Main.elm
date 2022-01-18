module Main exposing (main)

import Dict
import ElmAdmin as A exposing (ElmAdmin, admin)
import ElmAdmin.Form as AF
import Html exposing (..)
import Html.Attributes exposing (..)
import SubCmd exposing (SubCmd)
import ThemeSpec


type Msg
    = SetUser User


type alias Model =
    { user : Maybe User }


type alias User =
    { name : String
    , isAdmin : Bool
    }


emptyUser : User
emptyUser =
    { name = ""
    , isAdmin = False
    }


{-|

    page :

-}
createUser : A.Page Model Msg
createUser =
    A.formPage "/users/create"
        "Create User"
        { init = \_ _ -> Just emptyUser
        , fields =
            AF.fields User
                |> AF.textField "Name" .name []
                |> AF.checkboxField "Admin" .isAdmin []
        , onSubmit = \_ model user -> ( { model | user = Just user }, SubCmd.none )
        }


main : ElmAdmin () Model Msg
main =
    admin "Admin"
        { init = \_ _ -> ( { user = Nothing }, Cmd.none )
        , update = \_ _ model -> ( model, Cmd.none )
        , subscriptions = \_ _ -> Sub.none
        }
        [ A.theme [ A.preferDarkMode ]
        , A.pages
            [ A.single "User" createUser
            ]
        ]
