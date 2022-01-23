module Main exposing (main)

import Dict
import ElmAdmin as A exposing (ElmAdmin, admin)
import ElmAdmin.Actions as AA
import ElmAdmin.Form as AF
import ElmAdmin.Page as AP
import Html exposing (..)
import Html.Attributes exposing (..)
import SubCmd
import Task
import ThemeSpec


type alias Msg =
    ()


type alias Model =
    { user : Maybe User
    , users : List User
    }


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


createUserForm : AF.Form User
createUserForm =
    AF.form User
        |> AF.textField "Name" .name []
        |> AF.checkboxField "Admin" .isAdmin []


createUser : AP.Page Model Msg ()
createUser =
    AP.page "Create User"
        |> AP.init (\_ model -> ( model, AA.none ))
        |> AP.form
            { init = \_ _ -> Just emptyUser
            , form = createUserForm
            , onSubmit =
                \_ model user ->
                    ( { model | users = user :: model.users }
                    , AA.initForm createUserForm emptyUser
                    )
            }
        |> AP.view
            (\_ model ->
                case model.users of
                    [] ->
                        div [ style "color" ThemeSpec.color ] [ text "No users created." ]

                    _ ->
                        div []
                            (model.users
                                |> List.map (\user -> div [ style "color" ThemeSpec.color ] [ text user.name ])
                            )
            )


main : ElmAdmin () Model Msg
main =
    admin
        { title = "Admin"
        , init =
            \_ _ ->
                ( { user = Nothing
                  , users = []
                  }
                , AA.none
                )
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
