module Main exposing (main)

import ElmAdmin as A exposing (ElmAdmin, admin)
import ElmAdmin.Actions as AA
import ElmAdmin.Form as AF
import ElmAdmin.Page as AP
import ElmWidgets.Attributes as WA
import Html exposing (..)
import Html.Attributes exposing (..)



-- Model


type Model
    = SignedOut
    | SignedIn SignedInModel


type alias SignedInModel =
    { user : User
    , users : List User
    , posts : List Post
    }


signedIn : Model -> Maybe SignedInModel
signedIn model =
    case model of
        SignedIn m ->
            Just m

        _ ->
            Nothing


type alias Post =
    { title : String
    , author : User
    }


type alias User =
    { name : String
    }


emptyUser : User
emptyUser =
    { name = ""
    }



-- Update


type Msg
    = SignIn User
    | CreatePost PostForm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        SignedIn m ->
            signedInUpdate msg m
                |> Tuple.mapFirst SignedIn

        SignedOut ->
            case msg of
                SignIn user ->
                    ( SignedIn
                        { user = user
                        , users = [ user ]
                        , posts = []
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


signedInUpdate : Msg -> SignedInModel -> ( SignedInModel, Cmd Msg )
signedInUpdate msg model =
    case msg of
        CreatePost postForm ->
            case postForm.author of
                Just author ->
                    ( { model | posts = { title = postForm.title, author = author } :: model.posts }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


type alias PostForm =
    { title : String
    , author : Maybe User
    }


emptyPostForm : PostForm
emptyPostForm =
    { title = ""
    , author = Nothing
    }



-- Pages


pageSignIn : AP.Page Model Msg ()
pageSignIn =
    AP.page "Sign In"
        |> AP.form
            { init = \_ _ -> Just emptyUser
            , onSubmit = SignIn
            , form =
                AF.form "Create User" User
                    |> AF.textField "Name" .name []
            }


pagePosts : AP.Page SignedInModel Msg ()
pagePosts =
    AP.page "Posts"
        |> AP.form
            { init = \_ _ -> Just emptyPostForm
            , onSubmit = CreatePost
            , form =
                AF.form "Create Post" PostForm
                    |> AF.textField "Title" .title []
                    |> AF.autocompleteField
                        { label = "Author"
                        , value = .author
                        , options = \model -> Just model.users
                        , optionToLabel = .name
                        , attrs = []
                        }
            }
        |> AP.list
            { title = text "All Posts"
            , init = \_ model -> Just model.posts
            , toItem =
                \model post ->
                    { label = text post.title
                    , actions = []
                    , options = []
                    }
            }


main : ElmAdmin () Model Msg
main =
    admin
        { title = "Admin"
        , init = \_ _ -> ( SignedOut, Cmd.none )
        , update = \_ -> update
        , subscriptions = \_ _ -> Sub.none
        }
        [ A.theme [ A.preferDarkMode ]
        , A.pages
            [ A.single "/sign-in" "Sign In" pageSignIn
            ]
        , A.protectedPages
            { fromModel = signedIn
            , toModel = \_ -> SignedIn
            }
            [ A.single "/posts" "Posts" pagePosts
            ]
        ]
