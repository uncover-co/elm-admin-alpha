module Main exposing (main)

import Dict
import ElmAdmin as A exposing (ElmAdmin, admin)
import ElmAdmin.Form as AF
import ElmAdmin.Page as AP
import Html exposing (..)
import Html.Attributes exposing (..)
import SubCmd


type alias Msg =
    ()


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


workspacesIndex : AP.Page Model Msg ()
workspacesIndex =
    AP.page "Workspaces"
        |> AP.view (\_ _ -> div [] [ text "Workspaces" ])


createUser : AP.Page Model Msg ()
createUser =
    AP.page "Create User"
        |> AP.form
            { init = \_ _ -> Just emptyUser
            , fields =
                AF.fields User
                    |> AF.textField "Name" .name []
                    |> AF.checkboxField "Admin" .isAdmin []
            , onSubmit = \_ model user -> ( { model | user = Just user }, SubCmd.none )
            }


main : ElmAdmin () Model Msg
main =
    admin
        { title = "Admin"
        , init = \_ _ -> ( { user = Nothing }, Cmd.none )
        }
        [ A.theme [ A.preferDarkMode ]
        , A.pages
            [ A.single "/workspaces" "Workspaces" workspacesIndex
            ]
        , A.protectedPages
            { fromModel = \model -> Just model
            , toModel = \_ model -> model
            }
            [ A.single "/workspaces/new" "Create" workspacesIndex
            , A.single "/users/new" "User" createUser
            ]
        ]
