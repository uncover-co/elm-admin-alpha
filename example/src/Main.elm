module Main exposing (main)

import Dict
import ElmAdmin as A exposing (ElmAdmin, admin)
import Html exposing (..)
import Html.Attributes exposing (..)
import ThemeSpec


type Msg
    = Msg


type alias Model =
    ()


page : String -> String -> A.Page Model Msg
page title path =
    A.fullPage path
        { title = \_ _ -> title
        , init = \_ model -> ( model, Cmd.none )
        , update = \_ _ model -> ( model, Cmd.none )
        , subscriptions = \_ _ -> Sub.none
        , view =
            \{ pathParams } _ ->
                div [ style "color" ThemeSpec.color ]
                    [ h1 [] [ text title ]
                    , div []
                        (pathParams
                            |> Dict.keys
                            |> List.map (\k -> p [] [ text k ])
                        )
                    ]
        }


usersIndex : A.Page Model Msg
usersIndex =
    page "Users" "/users"


usersShow : A.Page Model Msg
usersShow =
    page "Users" "/users/:userId"


usersArchive : A.Page Model Msg
usersArchive =
    page "Users" "/users/archive"


usersCreate : A.Page Model Msg
usersCreate =
    page "Users" "/users/new"


workspacesIndex : A.Page Model Msg
workspacesIndex =
    page "Workspaces" "/workspaces"


workspacesCreate : A.Page Model Msg
workspacesCreate =
    page "Workspaces" "/workspaces/new"


main : ElmAdmin () Model Msg
main =
    admin
        { title = "Admin"
        , init = \_ _ -> ( (), Cmd.none )
        , update = \_ _ model -> ( model, Cmd.none )
        , subscriptions = \_ _ -> Sub.none
        , options = []
        , pages =
            [ A.external "Docs" "https://package.elm-lang.org/"
            , A.visualGroup "Intro"
                [ A.single "First steps" (page "First Steps" "/first-steps")
                , A.single "Knowing more" (page "Knowing More" "/know-more")
                ]
            , A.folderGroup "Users"
                usersIndex
                [ A.single "Create" usersCreate
                , A.single "Archive" usersArchive
                , A.url usersShow
                ]
            , A.group "Workspaces"
                workspacesIndex
                [ A.single "Create" workspacesCreate
                ]
            ]
        }
