module App exposing (..)

import Admin.Actions as AA
import Admin.Http
import Browser.Navigation
import Dict exposing (Dict)
import Html
import Http
import Json.Decode as D
import Task exposing (Task)



-- Model


type alias Model =
    { navKey : Browser.Navigation.Key
    , store : Store
    , auth : Auth
    }


type alias Store =
    { users : Dict Int User
    , posts : Dict Int Post
    , pokemon : Dict Int Pokemon
    , pokemonsPerPage : Dict Int Int
    , comments : Dict Int (List Comment)
    }


type alias Pokemon =
    { id : Int
    , name : String
    , type_ : String
    }


type Auth
    = SignedOut
    | SignedIn User


type alias SignedInModel =
    { navKey : Browser.Navigation.Key
    , user : User
    , store : Store
    }


type alias User =
    { id : Int
    , username : String
    , password : String
    , role : UserRole
    }


type UserRole
    = Admin
    | Viewer


type alias Post =
    { id : Int
    , title : String
    , body : String
    , author : Int
    }


type alias Comment =
    { id : Int
    , author : Int
    , body : String
    }



-- Update


type Msg
    = FetchPokemonList Int
    | FetchPokemonListResponse (Result Http.Error (List { id : Int, label : String }))
    | SignIn String String
    | CreatePost String String


update : Msg -> Model -> ( Model, AA.Action Msg )
update msg model =
    case msg of
        FetchPokemonList _ ->
            ( { model | store = model.store }, AA.none )

        FetchPokemonListResponse _ ->
            ( model, AA.none )

        _ ->
            case model.auth of
                SignedIn user ->
                    signedInUpdate msg user model

                SignedOut ->
                    case msg of
                        SignIn username password ->
                            case signIn username password model of
                                ( Just user, model_ ) ->
                                    ( model_
                                    , AA.showSuccessNotification (Html.text <| "Welcome back " ++ user.username ++ "!")
                                    )

                                ( Nothing, model_ ) ->
                                    ( model_
                                    , AA.showWarningNotification (Html.text "Couldn't sign in with that username/password")
                                    )

                        _ ->
                            ( model, AA.none )


signedInUpdate : Msg -> User -> Model -> ( Model, AA.Action Msg )
signedInUpdate msg user model =
    case msg of
        CreatePost title body ->
            case createPost title body user model of
                ( _, model_ ) ->
                    ( model_
                    , AA.showSuccessNotification (Html.text "Congratulations on putting it out there!")
                    )

        _ ->
            ( model, AA.none )



-- Init


init : Browser.Navigation.Key -> Model
init navKey =
    { navKey = navKey
    , store =
        { pokemon = Dict.empty
        , pokemonsPerPage = Dict.empty
        , users =
            idDict
                [ { id = 0, username = "admin", password = "password", role = Admin }
                , { id = 1, username = "commoner", password = "12345", role = Viewer }
                ]
        , posts =
            idDict
                [ { id = 0, title = "I'm the ruler of these lands", body = "Obey me, commoners", author = 0 }
                , { id = 1, title = "Out with the old", body = "I say it's time we take over what is ours", author = 1 }
                ]
        , comments =
            Dict.fromList
                [ ( 0, [ { id = 0, author = 1, body = "Never!" } ] )
                , ( 1, [ { id = 1, author = 0, body = "I can delete this post whenever I want you dirty little commoner" } ] )
                ]
        }
    , auth = SignedOut
    }


idDict : List { x | id : Int } -> Dict Int { x | id : Int }
idDict idList =
    idList
        |> List.map (\x -> ( x.id, x ))
        |> Dict.fromList



-- Auth


signedIn : Model -> Maybe SignedInModel
signedIn model =
    case model.auth of
        SignedIn user ->
            Just
                { navKey = model.navKey
                , store = model.store
                , user = user
                }

        _ ->
            Nothing


signIn : String -> String -> Model -> ( Maybe User, Model )
signIn username password model =
    case userForUsernamePassword username password model.store.users of
        Just user ->
            ( Just user
            , { model | auth = SignedIn user }
            )

        Nothing ->
            ( Nothing, model )


userForUsernamePassword : String -> String -> Dict Int User -> Maybe User
userForUsernamePassword username password users =
    case
        users
            |> Dict.values
            |> List.filter (\u -> u.username == username && u.password == password)
    of
        user :: [] ->
            Just user

        _ ->
            Nothing



-- Create User


createUser : String -> String -> Model -> ( User, Model )
createUser username password model =
    let
        newId : Int
        newId =
            Dict.size model.store.users

        newUser : User
        newUser =
            { id = newId, username = username, password = password, role = Viewer }

        prevStore : Store
        prevStore =
            model.store
    in
    ( newUser
    , { model
        | store =
            { prevStore
                | users =
                    Dict.insert newId newUser model.store.users
            }
      }
    )


updateUser : User -> Model -> Model
updateUser user model =
    let
        prevStore : Store
        prevStore =
            model.store
    in
    { model
        | store =
            { prevStore
                | users =
                    Dict.insert user.id user model.store.users
            }
    }


deleteUser : User -> Model -> Model
deleteUser user model =
    let
        prevStore : Store
        prevStore =
            model.store
    in
    { model
        | store =
            { prevStore
                | users =
                    Dict.remove user.id model.store.users
            }
    }



-- Posts


createPost : String -> String -> User -> Model -> ( Post, Model )
createPost title body author model =
    let
        newId : Int
        newId =
            Dict.size model.store.users

        newPost : Post
        newPost =
            { id = newId, author = author.id, title = title, body = body }

        prevStore : Store
        prevStore =
            model.store
    in
    ( newPost
    , { model
        | store =
            { prevStore
                | posts =
                    Dict.insert newId newPost model.store.posts
            }
      }
    )


updatePost : Post -> Model -> Model
updatePost post model =
    let
        prevStore : Store
        prevStore =
            model.store
    in
    { model
        | store =
            { prevStore
                | posts =
                    Dict.insert post.id post model.store.posts
            }
    }


deletePost : Post -> Model -> Model
deletePost post model =
    let
        prevStore : Store
        prevStore =
            model.store
    in
    { model
        | store =
            { prevStore
                | posts =
                    Dict.remove post.id model.store.posts
            }
    }



-- Comments


createComment : Post -> String -> SignedInModel -> Model -> ( Comment, Model )
createComment post body signedInModel model =
    let
        author : User
        author =
            signedInModel.user

        newId : Int
        newId =
            Dict.size model.store.users

        newComment : Comment
        newComment =
            { id = newId, author = author.id, body = body }

        prevStore : Store
        prevStore =
            model.store

        newComments : List Comment
        newComments =
            model.store.comments
                |> Dict.get post.id
                |> Maybe.withDefault []
                |> (::) newComment
    in
    ( newComment
    , { model
        | store =
            { prevStore
                | comments =
                    Dict.insert post.id newComments model.store.comments
            }
      }
    )


updateComment : Post -> Comment -> Model -> Model
updateComment post comment model =
    let
        prevStore : Store
        prevStore =
            model.store

        newComments : List Comment
        newComments =
            model.store.comments
                |> Dict.get post.id
                |> Maybe.withDefault []
                |> List.map
                    (\c ->
                        if c.id == comment.id then
                            comment

                        else
                            c
                    )
    in
    { model
        | store =
            { prevStore
                | comments =
                    Dict.insert
                        post.id
                        newComments
                        model.store.comments
            }
    }


deleteComment : Post -> Comment -> Model -> Model
deleteComment post comment model =
    let
        prevStore : Store
        prevStore =
            model.store

        newComments : List Comment
        newComments =
            model.store.comments
                |> Dict.get post.id
                |> Maybe.withDefault []
                |> List.filter (\c -> c.id == comment.id)
    in
    { model
        | store =
            { prevStore
                | comments =
                    Dict.insert
                        post.id
                        newComments
                        model.store.comments
            }
    }



-- Pokemon API


fetchPokemonList : Task Http.Error (List { id : Int, label : String })
fetchPokemonList =
    Admin.Http.get
        { url = "https://pokeapi.co/api/v2/pokemon/"
        , headers = []
        , decoder =
            D.list
                (D.map2 (\id_ name -> { id = id_, label = name })
                    (D.field "id" D.int)
                    (D.field "name" D.string)
                )
        }


fetchPokemon : String -> Task Http.Error { id : Int, label : String }
fetchPokemon id =
    Admin.Http.get
        { url = "https://pokeapi.co/api/v2/pokemon/" ++ id
        , headers = []
        , decoder =
            D.map2 (\id_ name -> { id = id_, label = name })
                (D.field "id" D.int)
                (D.field "name" D.string)
        }



-- searchPokemon : Task Http.Error (List { id : Int, label : String })
-- searchPokemon =
--     Admin.Http.get
--         { url = "https://pokeapi.co/api/v2/pokemon"
--         , headers = []
--         , decoder =
--             D.field "results"
--                 (D.list
--                     (D.map2 (\id name -> { id = id, label = name })
--                         (D.field "name" D.string)
--                         (D.field "name" D.string)
--                     )
--                 )
--         }
