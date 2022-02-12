module Main exposing (main)

import ElmAdmin as A exposing (ElmAdmin, admin)
import ElmAdmin.Actions as AA
import ElmAdmin.Form as AF
import ElmAdmin.Page as AP
import Html exposing (..)
import Html.Attributes exposing (..)
import Set exposing (Set)



-- Model


type Model
    = SignedOut
    | SignedIn SignedInModel


type alias SignedInModel =
    { user : User
    , users : List User
    , posts : List Post
    , nicknames : Set String
    , validNicknames : Maybe (List String)
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
    | SearchNicknames String
    | SearchNicknamesResponse String
    | AddNicknameOption String


update : Msg -> Model -> ( Model, AA.Action Msg )
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
                        , nicknames = Set.fromList [ "Georges", "Boris", "Elmhead" ]
                        , validNicknames = Just []
                        }
                    , AA.none
                    )

                _ ->
                    ( model, AA.none )


signedInUpdate : Msg -> SignedInModel -> ( SignedInModel, AA.Action Msg )
signedInUpdate msg model =
    case msg of
        CreatePost postForm ->
            case postForm.author of
                Just author ->
                    ( { model | posts = { title = postForm.title, author = author } :: model.posts }, AA.none )

                Nothing ->
                    ( model, AA.none )

        SearchNicknames v ->
            ( { model | validNicknames = model.validNicknames }
            , AA.debounce "search-nicknames" 200 (SearchNicknamesResponse v)
            )

        SearchNicknamesResponse v ->
            ( { model
                | validNicknames =
                    model.nicknames
                        |> Set.filter (\s -> String.startsWith v s)
                        |> Set.toList
                        |> Just
              }
            , AA.none
            )

        AddNicknameOption v ->
            let
                nicknames =
                    Set.insert v model.nicknames
            in
            ( { model
                | nicknames = nicknames
                , validNicknames =
                    nicknames
                        |> Set.filter (\s -> String.startsWith v s)
                        |> Set.toList
                        |> Just
              }
            , AA.none
            )

        _ ->
            ( model, AA.none )


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
    , authorNickname : Maybe String
    , published : Published
    , age : Float
    }


emptyPostForm : PostForm
emptyPostForm =
    { title = ""
    , author = Nothing
    , authorNickname = Nothing
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
            , attrs = []
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
            , attrs = []
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
                    |> AF.autocomplete
                        { label = "Author Nickname"
                        , value = .authorNickname
                        , options = \model -> model.validNicknames
                        , optionToLabel = identity
                        , attrs =
                            [ AF.onSearch SearchNicknames
                            , AF.onEnter AddNicknameOption
                            , AF.required
                            ]
                        }
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
                        , attrs = [ AF.readOnly ]
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
    A.adminWithActions
        { title = "Admin"
        , init = \_ _ -> ( SignedOut, AA.none )
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
