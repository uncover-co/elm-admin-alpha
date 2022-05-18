module Admin.Internal.Application exposing (..)

import Admin.Internal.Router
import Dict exposing (Dict)


type alias Model =
    { activeRoute : Maybe Admin.Internal.Router.Route
    , validRoutes : Dict String String
    }


main =
    -- validate routes
    -- validate pages (?)
    -- cache routes
    -- cache pages
    -- onRouteChange
    -- find page
    -- update validPages
    -- initOneOf
    -- onUpdate
    -- nothing
    -- onView
    --
    -- viewOneOf
    Debug.todo ""



-- view :
--     List (Router model msg) ->
--     oneOf routers
--     |> Maybe.andThen (\model ->
--     )
