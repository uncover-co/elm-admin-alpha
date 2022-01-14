module ElmAdmin.Model exposing
    ( ElmAdmin
    , Model
    , Msg(..)
    , Page
    , init
    , subscriptions
    , update
    , view
    )

import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Dict exposing (Dict)
import ElmAdmin.RouteParams exposing (RouteParams)
import ElmAdmin.Router
import ElmAdmin.Styles
import ElmWidgets
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE
import ThemeSpec
import UI.Nav exposing (UINavItem)
import Url exposing (Url)


type alias ElmAdmin flags model msg =
    Program flags (Model model) (Msg msg)


type alias Page model msg =
    { path : List String
    , title : RouteParams -> model -> String
    , disabled : RouteParams -> model -> Bool
    , init : RouteParams -> model -> ( model, Cmd msg )
    , update : msg -> RouteParams -> model -> ( model, Cmd msg )
    , subscriptions : RouteParams -> model -> Sub msg
    , view : RouteParams -> model -> Html msg
    }


pagePathAsString : Page model msg -> String
pagePathAsString page =
    "/" ++ (page.path |> String.join "/")


type alias Model model =
    { navKey : Browser.Navigation.Key
    , model : model
    , routeParams : RouteParams
    , activePath : Maybe String
    , darkMode : Bool
    }


type Msg msg
    = OnUrlRequest UrlRequest
    | OnUrlChange Url
    | ToggleDarkMode
    | Msg msg


oneOfPage :
    Dict String (List (Page model msg))
    -> Url
    -> model
    -> Maybe ( Page model msg, RouteParams )
oneOfPage pageRouteCache url model =
    ElmAdmin.Router.oneOf .path pageRouteCache url
        |> Maybe.andThen
            (\( p, routeParams_ ) ->
                if p.disabled routeParams_ model then
                    Nothing

                else
                    Just ( p, routeParams_ )
            )


init :
    { pageRoutes : Dict String (List (Page model msg))
    , initModel : flags -> Browser.Navigation.Key -> ( model, Cmd msg )
    , preferDarkMode : Bool
    }
    -> flags
    -> Url
    -> Browser.Navigation.Key
    -> ( Model model, Cmd (Msg msg) )
init props flags url navKey =
    let
        ( initialModel, initialCmd ) =
            props.initModel flags navKey

        activePage : Maybe ( Page model msg, RouteParams )
        activePage =
            oneOfPage props.pageRoutes url initialModel

        ( initialPageModel, initialPageCmd ) =
            activePage
                |> Maybe.map (\( page, routeParams_ ) -> page.init routeParams_ initialModel)
                |> Maybe.withDefault ( initialModel, initialCmd )

        ( activePath, routeParams ) =
            activePage
                |> Maybe.map (Tuple.mapFirst (Just << pagePathAsString))
                |> Maybe.withDefault ( Nothing, ElmAdmin.RouteParams.empty )

        adminCmd =
            if activePage == Nothing && url.path /= "/" then
                Browser.Navigation.replaceUrl navKey "/"

            else
                Cmd.none
    in
    ( { navKey = navKey
      , model = initialPageModel
      , routeParams = routeParams
      , activePath = activePath
      , darkMode = props.preferDarkMode
      }
    , Cmd.batch
        [ Cmd.map Msg initialCmd
        , Cmd.map Msg initialPageCmd
        , adminCmd
        ]
    )


update :
    Dict String (Page model msg)
    -> Dict String (List (Page model msg))
    -> Msg msg
    -> Model model
    -> ( Model model, Cmd (Msg msg) )
update pages pageRouteCache msg model =
    case msg of
        OnUrlRequest request ->
            case request of
                External url ->
                    ( model, Browser.Navigation.load url )

                Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl
                        model.navKey
                        (Url.toString url)
                    )

        OnUrlChange url ->
            case oneOfPage pageRouteCache url model.model of
                Just ( page, routeParams ) ->
                    ( { model
                        | activePath = Just (pagePathAsString page)
                        , routeParams = routeParams
                      }
                    , Cmd.none
                    )

                Nothing ->
                    if url.path == "/" then
                        ( { model
                            | activePath = Nothing
                            , routeParams = ElmAdmin.RouteParams.empty
                          }
                        , Cmd.none
                        )

                    else
                        ( model
                        , Browser.Navigation.pushUrl model.navKey "/"
                        )

        ToggleDarkMode ->
            ( { model | darkMode = not model.darkMode }, Cmd.none )

        Msg msg_ ->
            model.activePath
                |> Maybe.andThen
                    (\path ->
                        Dict.get path pages
                            |> Maybe.map
                                (\page ->
                                    page.update msg_ model.routeParams model.model
                                        |> (\( model_, cmd ) ->
                                                ( { model | model = model_ }
                                                , Cmd.map Msg cmd
                                                )
                                           )
                                )
                    )
                |> Maybe.withDefault ( model, Cmd.none )


subscriptions : Model model -> Sub (Msg msg)
subscriptions _ =
    Sub.none


view :
    { title : String
    , lightTheme : ThemeSpec.Theme
    , darkTheme : ThemeSpec.Theme
    , darkModeClass : String
    }
    -> Dict String (Page model msg)
    -> List (UINavItem model)
    -> Model model
    -> Browser.Document (Msg msg)
view props pages navItems model =
    let
        activePage =
            model.activePath
                |> Maybe.andThen (\path -> Dict.get path pages)
                |> Maybe.andThen
                    (\p ->
                        if p.disabled model.routeParams model.model then
                            Nothing

                        else
                            Just p
                    )

        activePageTitle =
            activePage
                |> Maybe.map
                    (\page ->
                        h2
                            [ class "eadm eadm-page-title" ]
                            [ text (page.title model.routeParams model.model)
                            ]
                    )
                |> Maybe.withDefault (text "")

        activePageView =
            activePage
                |> Maybe.map
                    (\page ->
                        page.view model.routeParams model.model
                            |> Html.map Msg
                    )
                |> Maybe.withDefault (text "")
    in
    { title = props.title
    , body =
        [ ThemeSpec.globalProviderWithDarkMode
            { light = props.lightTheme
            , dark = props.darkTheme
            , strategy = ThemeSpec.ClassStrategy props.darkModeClass
            }
        , ElmWidgets.globalStyles
        , ElmAdmin.Styles.globalStyles
        , div [ classList [ ( props.darkModeClass, model.darkMode ) ] ]
            [ div [ class "eadm eadm-wrapper" ]
                [ aside [ class "eadm eadm-sidebar" ]
                    [ header [ class "eadm eadm-sidebar-header" ]
                        [ h1 [ class "eadm eadm-title" ]
                            [ a
                                [ class "eadm eadm-link"
                                , href "/"
                                ]
                                [ text props.title ]
                            ]
                        , button
                            [ class "eadm eadm-sidebar-dark-btn"
                            , HE.onClick ToggleDarkMode
                            ]
                            [ text "D" ]
                        ]
                    , UI.Nav.view model.activePath model.routeParams model.model navItems
                    ]
                , main_ [ class "eadm eadm-main" ]
                    [ activePageTitle
                    , activePageView
                    ]
                ]
            ]
        ]
    }
