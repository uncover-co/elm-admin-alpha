module Admin.Internal.RouterTest exposing (..)

import Admin.Internal.Router
import Dict
import Expect exposing (equal)
import Test exposing (Test, describe, test)
import Url exposing (Url)


routes : List String
routes =
    [ "/"
    , "/users"
    , "/users/:userId"
    , "/posts/:userId/new"
    , "/:404"
    ]


exampleUrl : Url
exampleUrl =
    { protocol = Url.Http
    , host = "example.com"
    , port_ = Nothing
    , path = "/users/123"
    , query = Nothing
    , fragment = Nothing
    }


router : Admin.Internal.Router.Router
router =
    Admin.Internal.Router.fromList routes


suite : Test
suite =
    describe "Creating Routers"
        [ test "`fromList` creates a new router based on a list of paths and `toList` returns the stored sorted paths." <|
            \_ ->
                equal
                    (Admin.Internal.Router.toList router)
                    (List.sort routes)
        , test "`findByString` searches for a matching route based on a path string" <|
            \_ ->
                equal
                    (Admin.Internal.Router.findByString "/users/123" Nothing router
                        |> Maybe.map Admin.Internal.Router.toPathId
                    )
                    (Just "/users/:userId")
        , test "`findByUrl` searches for a matching route based on a Url" <|
            \_ ->
                equal
                    (Admin.Internal.Router.findByUrl exampleUrl router
                        |> Maybe.map Admin.Internal.Router.toPathId
                    )
                    (Just "/users/:userId")
        , test "`validRoutes` returns all paths resolved by the current path params" <|
            \_ ->
                equal
                    (Admin.Internal.Router.findByUrl exampleUrl router
                        |> Maybe.map (\r -> Admin.Internal.Router.validRoutes r router)
                    )
                    (Just
                        (Dict.fromList
                            [ ( "/", "/" )
                            , ( "/users", "/users" )
                            , ( "/users/:userId", "/users/123" )
                            , ( "/posts/:userId/new", "/posts/123/new" )
                            ]
                        )
                    )
        ]
