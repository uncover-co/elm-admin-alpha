module Main exposing (main)

import Admin as A exposing (Admin)
import Admin.Actions as AA
import Admin.Form as AF
import Admin.Http
import Admin.Page as AP
import Admin.Router as AR
import Browser.Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error(..), Response(..))
import Json.Decode as D
import Platform exposing (Task)
import Set exposing (Set)



-- Model


type Model
    = SignedOut Browser.Navigation.Key
    | SignedIn SignedInModel


type alias SignedInModel =
    { navKey : Browser.Navigation.Key
    , user : User
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

        SignedOut navKey ->
            case msg of
                SignIn user ->
                    ( SignedIn
                        { navKey = navKey
                        , user = user
                        , users = [ user ]
                        , posts = []
                        , nicknames = Set.fromList [ "Georges", "Boris", "Elmhead" ]
                        , validNicknames = Just []
                        }
                    , AA.none
                      -- , AA.cmd <|
                      --     Browser.Navigation.pushUrl navKey "/posts"
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



-- Pokemon API
-- Admin.Http.get
--     { url : String
--     , headers : []
--     , decoder : D.Decoder a
--     }
--     -> Task Http.Error a
-- Admin.Http.get


fetchPokemon : String -> Task Http.Error { id : String, label : String }
fetchPokemon id =
    Admin.Http.get
        { url = "https://pokeapi.co/api/v2/pokemon/" ++ id
        , headers = []
        , decoder =
            D.map2 (\id_ name -> { id = String.fromInt id_, label = name })
                (D.field "id" D.int)
                (D.field "name" D.string)
        }


searchPokemon : Task Http.Error (List { id : String, label : String })
searchPokemon =
    Admin.Http.get
        { url = "https://pokeapi.co/api/v2/pokemon"
        , headers = []
        , decoder =
            D.field "results"
                (D.list
                    (D.map2 (\id name -> { id = id, label = name })
                        (D.field "name" D.string)
                        (D.field "name" D.string)
                    )
                )
        }



-- Pages


pageHome : AP.Page model msg ()
pageHome =
    AP.page "Welcome"
        |> AP.card
            (\_ _ ->
                div [ style "padding" "20px 20px 1800px" ]
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
                    (\name _ -> User name)
                    |> AF.text "Name" .name []
                    |> AF.remoteAutocomplete
                        { label = "Favorite Pokemon"
                        , value = \_ -> Just "1"
                        , initRequest = \_ _ id -> fetchPokemon id
                        , searchRequest = \_ _ _ -> searchPokemon
                        , attrs = []
                        }
            }


pageAlreadySignedIn : AP.Page SignedInModel Msg ()
pageAlreadySignedIn =
    AP.page "Sign In"
        |> AP.card
            (\_ _ ->
                text "You're already signed in, silly!"
            )


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
                        , options = \model _ -> Just model.users
                        , optionToLabel = .name
                        , attrs = []
                        }
                    |> AF.autocomplete
                        { label = "Author Nickname"
                        , value = .authorNickname
                        , options = \model _ -> model.validNicknames
                        , optionToLabel = identity
                        , attrs =
                            [ AF.onSearch (\_ _ _ -> SearchNicknames)
                            , AF.onEnter (\_ _ _ -> AddNicknameOption)
                            , AF.required
                            ]
                        }
                    |> AF.select
                        { label = "Is Published"
                        , value = .published
                        , optionToLabel = publishedToString
                        , options = \_ _ -> [ Published, NotPublished ]
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
                \_ _ post ->
                    { label = text post.title
                    , actions = []
                    , options = []
                    }
            }


main : Admin () Model Msg
main =
    A.adminWithActions
        { title = "Admin"
        , init = \_ navKey -> ( SignedOut navKey, AA.none )
        , update = update
        , subscriptions = \_ -> Sub.none
        }
        [ A.protectedRouter signedIn
            [ AR.route "/sign-in"
                { page = pageAlreadySignedIn
                , options = []
                }
                []
            , AR.route "/posts"
                { page = pagePosts
                , options = []
                }
                []
            ]
        , A.router
            [ AR.route "/"
                { page = pageHome
                , options = []
                }
                []
            , AR.route "/sign-in"
                { page = pageSignIn
                , options = [ AR.full ]
                }
                [ AR.external
                    { url = "/sign-in/go"
                    , label = "Go"
                    }
                ]
            ]
        ]
