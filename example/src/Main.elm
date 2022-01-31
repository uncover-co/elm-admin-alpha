module Main exposing (main)

import ElmAdmin as A exposing (ElmAdmin, admin)
import ElmAdmin.Form as AF
import ElmAdmin.Page as AP
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


type Published
    = Published
    | NotPublished


publishedToString : Published -> String
publishedToString p =
    case p of
        Published ->
            "Published"

        NotPublished ->
            "Draft"


type alias PostForm =
    { title : String
    , author : Maybe User
    , authorNickname : String
    , published : Published
    , age : Float
    }


emptyPostForm : PostForm
emptyPostForm =
    { title = ""
    , author = Nothing
    , authorNickname = ""
    , published = NotPublished
    , age = 20.0
    }



-- Pages


pageHome : AP.Page model msg ()
pageHome =
    AP.page "Welcome"
        |> AP.card
            (\_ _ ->
                div [ style "padding" "20px" ]
                    [ text "Welcome to our homepage!" ]
            )


pageSignIn : AP.Page Model Msg ()
pageSignIn =
    AP.page "Sign In"
        |> AP.form
            { init = \_ _ -> Just emptyUser
            , onSubmit = \_ _ -> SignIn
            , form =
                AF.form "Create User"
                    User
                    |> AF.text "Name" .name []
            }


pagePosts : AP.Page SignedInModel Msg ()
pagePosts =
    AP.page "Posts"
        |> AP.form
            { init = \_ _ -> Just emptyPostForm
            , onSubmit = \_ _ -> CreatePost
            , form =
                AF.form "Create Post" PostForm
                    |> AF.text "Title" .title []
                    |> AF.autocomplete
                        { label = "Author"
                        , value = .author
                        , options = \model -> Just model.users
                        , optionToLabel = .name
                        , attrs = []
                        }
                    |> AF.text "Author Nickname"
                        .authorNickname
                        [ AF.hiddenIf (\_ _ f -> f.author == Nothing)
                        ]
                    |> AF.select
                        { label = "Is Published"
                        , value = .published
                        , optionToLabel = publishedToString
                        , options = \_ -> [ Published, NotPublished ]
                        , attrs = [ AF.readOnly ]
                        }
                    |> AF.range
                        { label = "Age"
                        , value = .age
                        , min = 18
                        , max = 90
                        , step = 1
                        , attrs = []
                        }
            }
        |> AP.list
            { title = text "All Posts"
            , init = \model _ -> Just model.posts
            , toItem =
                \_ post ->
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
            [ A.url "/" pageHome
            , A.single "/sign-in" "Sign In" pageSignIn
            ]
        , A.protectedPages
            { fromModel = signedIn
            , toModel = \_ -> SignedIn
            }
            [ A.single "/posts" "Posts" pagePosts
            ]
        ]
