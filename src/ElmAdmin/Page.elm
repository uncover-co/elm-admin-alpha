module ElmAdmin.Page exposing
    ( page, params, title, nav, Page
    , init, update, view, subscriptions
    , form
    , initWithEffect, updateWithEffect
    )

{-|


## Setup

@docs page, params, title, nav, Page


## Elm Architecture

@docs init, update, view, subscriptions


## Especial Views

@docs form


## Effects

@docs initWithEffect, updateWithEffect

-}

import Dict exposing (Dict)
import ElmAdmin.Form
import ElmAdmin.Internal.Page exposing (PageRouteParams)
import ElmAdmin.Shared exposing (Effect(..), Msg(..), SubCmd)
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
    (PageRouteParams params -> model -> ( model, Cmd msg ))
    -> Page model msg params
    -> Page model msg params
init =
    ElmAdmin.Internal.Page.init


{-| -}
initWithEffect : (PageRouteParams params -> model -> ( model, SubCmd msg )) -> Page model msg params -> Page model msg params
initWithEffect =
    ElmAdmin.Internal.Page.initWithEffect


{-| -}
update : (PageRouteParams params -> Msg msg -> model -> ( model, Cmd msg )) -> Page model msg params -> Page model msg params
update =
    ElmAdmin.Internal.Page.update


{-| -}
updateWithEffect : (PageRouteParams params -> Msg msg -> model -> ( model, SubCmd msg )) -> Page model msg params -> Page model msg params
updateWithEffect =
    ElmAdmin.Internal.Page.updateWithEffect


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
    , fields : ElmAdmin.Form.Fields resource
    , onSubmit : PageRouteParams params -> model -> resource -> ( model, SubCmd msg )
    }
    -> Page model msg params
    -> Page model msg params
form =
    ElmAdmin.Internal.Page.form
