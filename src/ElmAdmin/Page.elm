module ElmAdmin.Page exposing
    ( page, params, title, nav, Page
    , init, update, view, subscriptions
    , form, list
    )

{-|


## Setup

@docs page, params, title, nav, Page


## Elm Architecture

@docs init, update, view, subscriptions


## Especial Views

@docs form, list

-}

import Dict exposing (Dict)
import ElmAdmin.Form
import ElmAdmin.Internal.Page exposing (PageRouteParams)
import ElmAdmin.Shared exposing (Action, Effect(..), Msg(..))
import Html exposing (Html)


{-| -}
type alias Page model msg params =
    ElmAdmin.Internal.Page.Page model msg params


{-| -}
page : String -> Page model msg ()
page =
    ElmAdmin.Internal.Page.page


{-| -}
params :
    (Dict String String -> Maybe params)
    -> Page model msg x
    -> Page model msg params
params =
    ElmAdmin.Internal.Page.params


{-| -}
nav :
    (PageRouteParams params -> model -> String)
    -> Page model msg params
    -> Page model msg params
nav =
    ElmAdmin.Internal.Page.nav


{-| -}
title :
    (PageRouteParams params -> model -> String)
    -> Page model msg params
    -> Page model msg params
title =
    ElmAdmin.Internal.Page.title


{-| -}
init :
    (PageRouteParams params -> model -> ( model, Action msg ))
    -> Page model msg params
    -> Page model msg params
init =
    ElmAdmin.Internal.Page.init


{-| -}
update : (PageRouteParams params -> Msg msg -> model -> ( model, Action msg )) -> Page model msg params -> Page model msg params
update =
    ElmAdmin.Internal.Page.update


{-| -}
subscriptions : (PageRouteParams params -> model -> Sub msg) -> Page model msg params -> Page model msg params
subscriptions =
    ElmAdmin.Internal.Page.subscriptions


{-| -}
view : (PageRouteParams params -> model -> Html msg) -> Page model msg params -> Page model msg params
view =
    ElmAdmin.Internal.Page.view


{-| -}
form :
    { init : PageRouteParams params -> model -> Maybe resource
    , form : ElmAdmin.Form.Form resource
    , onSubmit : PageRouteParams params -> model -> resource -> ( model, Action msg )
    }
    -> Page model msg params
    -> Page model msg params
form =
    ElmAdmin.Internal.Page.form


{-| -}
list :
    { title : Html msg
    , init : PageRouteParams params -> model -> Maybe (List resource)
    , toItem : model -> resource -> { label : Html msg, actions : Html msg }
    }
    -> Page model msg params
    -> Page model msg params
list =
    ElmAdmin.Internal.Page.list
