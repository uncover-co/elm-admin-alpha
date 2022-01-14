module Main exposing (main)

import Dict
import ElmAdmin as A exposing (ElmAdmin, RouteParams, admin)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE
import Url exposing (Url)


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
    A.page
        { path = "/page/:id"
        , toResource = \_ model -> model.value
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


main : ElmAdmin () Model Msg
main =
    admin
        { title = "Admin"
        , initModel = \_ _ -> ( { value = "Hello" }, Cmd.none )
        }
        [ A.external "Docs" "packages"
        , A.single "Users" page
            |> A.format (\p _ -> Dict.get ":id" p.pathParams |> Maybe.withDefault "…")
        , A.hidden page
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
