module Admin exposing
    ( admin, adminWithActions, Admin
    , router, protectedRouter
    )

{-|

@docs admin, adminWithActions, Admin
@docs router, protectedRouter

-}

import Admin.Actions exposing (Action)
import Admin.Internal.Application
import Admin.Internal.Router
import Admin.Router
import Admin.Shared exposing (Msg(..))
import Browser
import Browser.Navigation
import SubCmd
import ThemeProvider
import ThemeSpec


{-| -}
type alias Admin flags model msg =
    Admin.Internal.Application.Admin flags model msg



-- Attributes


{-| -}
type Attribute model msg
    = Attribute (Attributes model msg -> Attributes model msg)


type alias Attributes model msg =
    { routers : List (Admin.Internal.Router.Router model msg)
    }


defaultAttrs : Attributes model msg
defaultAttrs =
    { routers = []
    }


applyAttrs : List (Attribute model msg) -> Attributes model msg
applyAttrs attrs =
    List.foldl (\(Attribute fn) a -> fn a) defaultAttrs attrs


{-| -}
router : List (Admin.Router.Route model msg) -> Attribute model msg
router v =
    Attribute <| \attrs -> { attrs | routers = attrs.routers ++ [ Admin.Internal.Router.router v ] }


{-| -}
protectedRouter : (model -> Maybe subModel) -> List (Admin.Router.Route subModel msg) -> Attribute model msg
protectedRouter a b =
    Attribute <| \attrs -> { attrs | routers = Admin.Internal.Router.protectedRouter a b :: attrs.routers }


{-| -}
admin :
    { title : String
    , init : flags -> Browser.Navigation.Key -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }
    -> List (Attribute model msg)
    -> Admin flags model msg
admin props =
    adminWithActions
        { title = props.title
        , init =
            \flags key ->
                props.init flags key
                    |> Tuple.mapSecond SubCmd.cmd
        , update =
            \msg model ->
                props.update msg model
                    |> Tuple.mapSecond SubCmd.cmd
        , subscriptions = props.subscriptions
        }


{-| -}
adminWithActions :
    { title : String
    , init : flags -> Browser.Navigation.Key -> ( model, Action msg )
    , update : msg -> model -> ( model, Action msg )
    , subscriptions : model -> Sub msg
    }
    -> List (Attribute model msg)
    -> Admin flags model msg
adminWithActions props attrs_ =
    let
        attrs : Attributes model msg
        attrs =
            applyAttrs attrs_
    in
    Browser.application
        { onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        , init =
            Admin.Internal.Application.init
                { init = props.init
                , update = props.update
                , routers = attrs.routers
                }
        , update =
            Admin.Internal.Application.update
                { update = props.update
                , routers = attrs.routers
                }
        , subscriptions =
            Admin.Internal.Application.subscriptions
                { subscriptions = props.subscriptions
                }
        , view =
            Admin.Internal.Application.view
                { title = props.title
                , theme =
                    { lightTheme = ThemeSpec.theme ThemeSpec.lightTheme
                    , darkTheme = ThemeSpec.theme ThemeSpec.darkTheme
                    , darkModeStrategy = ThemeProvider.ClassStrategy "eadm-dark"
                    , preferDarkMode = True
                    , disableModeSwitch = False
                    }
                }
        }



-- Define theme
-- Define attrs
-- Validate routes
-- !! view:
--   oneOf [ routers ]
--      Maybe.andThen (\router -> activeRoute)
--      Maybe.map (\_ -> view)
-- create router ids
-- router id + pathId -> Maybe page
