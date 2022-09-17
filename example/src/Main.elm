module Main exposing (main)

{-| This admin is an admin for a blog. But it also lists all pokemon (why not?).

  - User can list all pokemons (paginated)
  - User can search pokemons
  - User can list posts
  - User can search posts
  - User can sign in
  - When signed in, sign in page should display a different message (sign out)
  - When signed in, user can create posts
  - When signed in, user can create comment on posts
  - If signed user is admin, user can create and update other users

User profile should display different field types:

  - Name (string)
  - Role (custom type)
  - Age (int)
  - Birthday (datepicker)
  - Favorite Pokemon (remote option)
  - Current mood (slider between happy and sad)

-}

import Admin as A exposing (Admin)
import Admin.Actions as AA
import Admin.Form as AF
import Admin.Page as AP
import Admin.Router as AR
import App
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error(..), Response(..))


pageUserPosts : AP.Page App.SignedInModel App.Msg ()
pageUserPosts =
    AP.page "User Posts"


pagePosts : AP.Page App.Model App.Msg ()
pagePosts =
    AP.page "Posts"


pagePokemons : AP.Page App.Model App.Msg ()
pagePokemons =
    AP.page "Pokemons"


pageSignIn : AP.Page App.Model App.Msg ()
pageSignIn =
    AP.page "Sign In"
        |> AP.form
            { init = \_ _ -> Just ( "", "" )
            , attrs = []
            , onSubmit = \_ _ ( username, password ) -> App.SignIn username password
            , form =
                AF.form "Sign in" Tuple.pair
                    |> AF.text "Username" Tuple.first []
                    |> AF.text "Password" Tuple.first []
            }


main : Admin () App.Model App.Msg
main =
    A.adminWithActions
        { title = "elm-admin"
        , init = \_ navKey -> ( App.init navKey, AA.none )
        , update = App.update
        , subscriptions = \_ -> Sub.none
        }
        [ A.protectedRouter App.signedIn
            [ AR.route "/posts"
                { page = pageUserPosts
                , options = []
                }
                []
            ]
        , A.router
            [ AR.route "/"
                { page = pagePosts
                , options = []
                }
                []
            , AR.route "/pokemons"
                { page = pagePokemons
                , options = []
                }
                []
            , AR.route "/sign-in"
                { page = pageSignIn
                , options = []
                }
                []
            ]
        ]
