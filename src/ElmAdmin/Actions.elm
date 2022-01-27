module ElmAdmin.Actions exposing
    ( cmd, none, batch, Action
    , showNotification, showHighlightNotification, showSuccessNotification, showWarningNotification, showDangerNotification
    , initForm
    )

{-| Actions are a especial kind of `Cmd` that can be used to trigger both your usual commands as well as especial ones used by ElmAdmin.

You can use commands that will be handled by your own update functions.

    import ElmAdmin.Actions as Actions

    update : RouteParams -> Msg -> Model -> ( Model, Action Msg )
    update _ msg model =
        case msg of
            RequestedUsers ->
                ( model
                , Actions.cmd <| fetchUsers GotUsers
                )

            GotUsers users ->
                ( { model | users = users }
                , Actions.none
                )

But you can also trigger actions that will be handled by ElmAdmin itself.

    import ElmAdmin.Actions as Actions

    update : RouteParams -> Msg -> Model -> ( Model, Action Msg )
    update _ msg model =
        case msg of
            RequestedUsers ->
                ( model
                , Actions.cmd <| fetchUsers GotUsers
                )

            GotUsers users ->
                ( { model | users = users }
                , Actions.showNotification <|
                    text "Got some users!"
                )


# Commands

@docs cmd, none, batch, Action


# Notifications

You can trigger a notification popup from anywhere in your application through one of these commands.

@docs showNotification, showHighlightNotification, showSuccessNotification, showWarningNotification, showDangerNotification


# Form

@docs initForm

-}

import ElmAdmin.Form exposing (Form)
import ElmAdmin.Internal.Form
import ElmAdmin.Shared exposing (Action, Effect(..), Msg(..))
import ElmAdmin.UI.Notification
import Html as H
import SubCmd


{-| -}
type alias Action msg =
    ElmAdmin.Shared.Action msg


{-| Sends a command that will be handled by your update functions.
-}
cmd : Cmd msg -> Action msg
cmd =
    SubCmd.cmd


{-| Does nothing. This is similar to `Cmd.none`.
-}
none : Action msg
none =
    SubCmd.none


{-| Batchs a list of actions. You can mix and match your cmds and ElmAdmin actions by using this function.
-}
batch : List (Action msg) -> Action msg
batch =
    SubCmd.batch


{-| Triggers a notification that will fade out after a while.
-}
showNotification : H.Html msg -> Action msg
showNotification content =
    SubCmd.effect <| ShowNotification ElmAdmin.UI.Notification.None content


{-| Triggers a notification with a "highlight" theme.
-}
showHighlightNotification : H.Html msg -> Action msg
showHighlightNotification content =
    SubCmd.effect <| ShowNotification ElmAdmin.UI.Notification.Highlight content


{-| Triggers a notification with a "success" theme.
-}
showSuccessNotification : H.Html msg -> Action msg
showSuccessNotification content =
    SubCmd.effect <| ShowNotification ElmAdmin.UI.Notification.Success content


{-| Triggers a notification with a "warning" theme.
-}
showWarningNotification : H.Html msg -> Action msg
showWarningNotification content =
    SubCmd.effect <| ShowNotification ElmAdmin.UI.Notification.Warning content


{-| Triggers a notification with a "danger" theme.
-}
showDangerNotification : H.Html msg -> Action msg
showDangerNotification content =
    SubCmd.effect <| ShowNotification ElmAdmin.UI.Notification.Danger content


{-| Set a form state based on a resource.
-}
initForm : Form model msg params resource -> resource -> Action msg
initForm form resource =
    ElmAdmin.Internal.Form.initFields resource form
        |> UpdateFormModel
        |> SubCmd.effect
