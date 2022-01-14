# ElmAdmin

A framework for building admin applications themed trough [ThemeSpec](https://package.elm-lang.org/packages/uncover-co/elm-theme-spec/latest/).

**Alpha** - This is the alpha version of ElmAdmin. This means this package will be updated faster but breakage is expected. A stable version is yet to be released as `uncover-co/elm-admin`.

## Setup

ElmAdmin follows a somewhat similar strategy to [ElmBook](package.elm-lang.org/packages/dtwrks/elm-book/latest/). A minimalistic API focusing on pragmatism and user customizations.

You can start by creating pages for your admin:

```elm
module Users exposing (update)


import ElmAdmin as A


update : A.Page Msg Model
update =
    A.page
        { path = "/users/:userId"
        , title = \_ _ user -> user.name
        , resource = userFromParams
        , init = fetchUser
        , update = ...
        , subscriptions = ...
        , view \_ _ user = viewUserForm user
        }
```

Then you put them all together:

```elm
module Main exposing (main)


import ElmAdmin as A
import ThemeSpec
import Users


main : ElmAdmin () Msg Model
main =
    admin
        [ A.page "Home" Home.page
        , A.external "Docs" "https://package.elm-lang.org/"
        , A.resources "Users"
            { index : Users.index
            , show : Users.show
            , create : Users.create
            , update : Users.update
            }
        , A.group "Workspaces"
            { main = Workspaces.index
            , items =
                [ A.single "Archive" Workspaces.archive
                ]
            }
        ]
        [ preferDarkMode
        , lightTheme ThemeSpec.lightTheme
        ]
        { title = "My Admin"
        , init = init
        }
```

ElmAdmin works as you'd expect an Elm application to work. You can provide your own Msg/Model/etc functions and they will all work. We mostly handle routing, themes and a few other niceties that make building admins easier.
