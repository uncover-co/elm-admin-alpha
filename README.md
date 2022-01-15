# ElmAdmin

A framework for building admin applications themed trough [ThemeSpec](https://package.elm-lang.org/packages/uncover-co/elm-theme-spec/latest/).

**Alpha** - This is the alpha version of ElmAdmin. This means this package will be updated faster but breakage is expected. A stable version is yet to be released as `uncover-co/elm-admin`.

## Setup

ElmAdmin follows a somewhat similar strategy to [ElmBook](package.elm-lang.org/packages/dtwrks/elm-book/latest/). A minimalistic API focusing on pragmatism and user customizations.

You can start by creating pages for your admin:

```elm
module Users exposing (update)


import ElmAdmin as A
import UI exposing (viewUsers)


index : A.Page Msg Model
index =
    A.page "/users" "Users"
        (\routeParams model ->
            viewUsers model
        )
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
        { title = "My Admin"
        , init = init
        , update = update
        , subscriptions = subscriptions
        , options = [ preferDarkMode ]
        , pages =
            [ A.visualGroup "Guides"
                [ A.external "Documentation" "https://package.elm-lang.org/"
                , A.external "Design System" "https://elm-book-in-elm-book.netlify.app/"
                ]
            , A.folderGroup "Users"
                Users.index
                [ A.single "Create" Users.create
                , A.single "Archive" Users.archive
                , A.url Users.show
                ]
            , A.group "Workspaces"
                Workspaces.index
                [ A.single "Create" Workspaces.create
                , A.single "Create Project" Workspaces.createProject
                ]
            ]
        }
```

ElmAdmin works as you'd expect an Elm application to work. You can provide your own Msg/Model/etc functions and they will all work. We mostly handle routing, themes and a few other niceties that make building admins easier.
