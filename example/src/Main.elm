module Main exposing (main)

import Dict
import ElmAdmin as A exposing (ElmAdmin, RouteParams, admin)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE


type Msg
    = OnInput String


type alias Model =
    { value : String }


init : RouteParams -> Model -> String -> ( Model, Cmd Msg )
init _ _ _ =
    ( { value = "" }, Cmd.none )


update : Msg -> A.RouteParams -> Model -> String -> ( Model, Cmd Msg )
update msg _ _ _ =
    case msg of
        OnInput v ->
            ( { value = v }, Cmd.none )


page : A.Page Model Msg
page =
    A.resourcePage
        { path = "/page/:id"
        , title = \_ _ v -> "User: " ++ v
        , resource = \_ model -> model.value
        , init = init
        , update = update
        , subscriptions = \_ _ _ -> Sub.none
        , view =
            \_ _ value_ ->
                div []
                    [ input [ HE.onInput OnInput, value value_ ] []
                    , p [] [ text value_ ]
                    ]
        }


home : A.Page Model Msg
home =
    A.page
        { path = "/"
        , title = \_ _ -> "Home"
        , init = \_ model -> ( model, Cmd.none )
        , update = \_ _ model -> ( model, Cmd.none )
        , subscriptions = \_ _ -> Sub.none
        , view =
            \_ _ ->
                div [] [ text "Home" ]
        }


main : ElmAdmin () Model Msg
main =
    admin
        [ A.external "Docs" "packages"
        , A.url home
        , A.single "Users" page
            |> A.dynamic (\p _ -> Dict.get ":id" p.pathParams |> Maybe.withDefault "…")
        , A.url page
        , A.group "Workspaces"
            { main = page
            , items = [ A.single "Other" page ]
            }
        , A.resources "Workspaces"
            { index = page
            , show = page
            , create = page
            , update = page
            }
        ]
        []
        { title = "Admin"
        , init = \_ _ -> ( { value = "Hello" }, Cmd.none )
        }
