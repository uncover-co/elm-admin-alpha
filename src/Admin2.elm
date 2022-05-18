module Admin2 exposing
    ( Admin
    , adminWithActions
    )

import Admin.Actions exposing (Action)
import Admin.Internal.Router exposing (RouteParams)
import Admin.Shared exposing (Admin)
import Browser.Navigation


type alias Admin flags model msg =
    Admin.Shared.Admin flags model msg


type Option
    = Option


adminWithActions :
    { title : String
    , init : flags -> Browser.Navigation.Key -> ( model, Action msg )
    , update : RouteParams -> msg -> model -> ( model, Action msg )
    , subscriptions : RouteParams -> model -> Sub msg
    }
    -> List Option
    -> Admin flags model msg
adminWithActions props options =
    -- Define theme
    -- Define attrs
    -- Validate routes
    -- !! view:
    --   oneOf [ routers ]
    --      Maybe.andThen (\router -> activeRoute)
    --      Maybe.map (\_ -> view)
    Debug.todo ""
