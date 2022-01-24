module Main exposing (main)

import ElmAdmin as A exposing (ElmAdmin, admin)
import ElmAdmin.Actions as AA
import ElmAdmin.Form as AF
import ElmAdmin.Page as AP
import ElmWidgets.Attributes as WA
import Html exposing (..)
import Html.Attributes exposing (..)
import ThemeSpec


type alias Msg =
    ()


type alias Model =
    { user : Maybe User
    , users : Maybe (List User)
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
    AF.form "Create User" User
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
                    ( case model.users of
                        Just users ->
                            { model | users = Just (user :: users) }

                        Nothing ->
                            { model | users = Just [ user ] }
                    , AA.initForm createUserForm emptyUser
                    )
            }
        |> AP.list
            { title = text "All Users"
            , init = \_ model -> model.users
            , toItem =
                \_ user ->
                    { label = text user.name
                    , actions = []
                    , options = [ WA.href "/logAction/#" ]
                    }
            }


main : ElmAdmin () Model Msg
main =
    admin
        { title = "Admin"
        , init =
            \_ _ ->
                ( { user = Nothing
                  , users = Nothing
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
