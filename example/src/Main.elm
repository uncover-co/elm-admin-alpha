module Main exposing (main)

import Dict
import ElmAdmin as A exposing (ElmAdmin, RouteParams, admin)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE


type Msg
    = Msg


type alias Model =
    ()


page : String -> String -> A.Page Model Msg
page title path =
    A.page
        { path = path
        , title = \_ _ -> title
        , init = \_ model -> ( model, Cmd.none )
        , update = \_ _ model -> ( model, Cmd.none )
        , subscriptions = \_ _ -> Sub.none
        , view =
            \{ pathParams } _ ->
                div []
                    [ h1 [] [ text title ]
                    , div []
                        (pathParams
                            |> Dict.keys
                            |> List.map (\k -> p [] [ text k ])
                        )
                    ]
        }


main : ElmAdmin () Model Msg
main =
    admin
        [ A.external "Docs" "https://uncover.co"
        , A.url (page "Home" "/")
        , A.single "Users" (page "Users" "/users")
            |> A.dynamic (\_ p _ -> Dict.get ":id" p.pathParams |> Maybe.withDefault "…")
        , A.url (page "User" "/users/joaao")
        , A.single "Grgs" (page "User" "/users/:userId")
            |> A.params [ ( ":userId", "grgs" ) ]
        , A.group "Workspaces"
            { main = page "Workspaces" "/workspaces"
            , items =
                [ A.single "Create" (page "New Workspace" "/workspaces/new")
                , A.group "Group"
                    { main = page "Group" "/group"
                    , items =
                        [ A.single "Group Item" (page "Group Item" "/group/item")
                        , A.visualGroup "Visual Group"
                            [ A.single "Visual Group Item" (page "Visual Group Item" "/visual/group/item")
                            ]
                        ]
                    }
                ]
            }
        , A.resources "Workspace Users"
            { index = page "Index" "/workspace-users"
            , create = page "Create" "/workspace-users/new"
            , update = page "Update" "/workspace-users/:workspaceId/update"
            , show = page "Show" "/workspace-users/:workspaceId"
            }
        ]
        [ A.preventDarkMode ]
        { title = "Admin"
        , init = \_ _ -> ( (), Cmd.none )
        }
