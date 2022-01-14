module ElmAdmin.Model exposing
    ( Model
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
import ElmAdmin.Router exposing (RouteParams, emptyRouteParams)
import ElmAdmin.Styles
import ElmWidgets
import Html exposing (..)
import Html.Attributes exposing (..)
import ThemeSpec
import UI.Nav exposing (UINavItem)
import Url exposing (Url)


type alias RouteParams =
    { pathParams : Dict String String
    , queryParams : Dict String (List String)
    }


type alias Page model msg =
    { path : List String
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
    }


type Msg msg
    = DoNothing
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | Msg msg


oneOfPage :
    Dict String (List (Page model msg))
    -> Url
    -> Maybe ( Page model msg, RouteParams )
oneOfPage pageRouteCache url =
    ElmAdmin.Router.oneOf .path pageRouteCache url


init :
    Dict String (List (Page model msg))
    -> (flags -> Browser.Navigation.Key -> ( model, Cmd msg ))
    -> flags
    -> Url
    -> Browser.Navigation.Key
    -> ( Model model, Cmd (Msg msg) )
init pageRouteCache initModel flags url navKey =
    let
        activePage : Maybe ( Page model msg, RouteParams )
        activePage =
            oneOfPage pageRouteCache url

        ( initialModel, initialCmd ) =
            initModel flags navKey

        ( initialPageModel, initialPageCmd ) =
            activePage
                |> Maybe.map (\( page, routeParams_ ) -> page.init routeParams_ initialModel)
                |> Maybe.withDefault ( initialModel, initialCmd )

        ( activePath, routeParams ) =
            activePage
                |> Maybe.map (Tuple.mapFirst (Just << pagePathAsString))
                |> Maybe.withDefault ( Nothing, emptyRouteParams )

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
        DoNothing ->
            ( model, Cmd.none )

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
            if url.path == "/" then
                ( { model
                    | activePath = Nothing
                    , routeParams = emptyRouteParams
                  }
                , Cmd.none
                )

            else
                case oneOfPage pageRouteCache url of
                    Just ( page, routeParams ) ->
                        ( { model
                            | activePath = Just (pagePathAsString page)
                            , routeParams = routeParams
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        ( model
                        , Browser.Navigation.pushUrl model.navKey "/"
                        )

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
    String
    -> Dict String (Page model msg)
    -> List (UINavItem model)
    -> Model model
    -> Browser.Document (Msg msg)
view title pages navItems model =
    let
        activePage =
            case model.activePath of
                Just path ->
                    Dict.get path pages
                        |> Maybe.map
                            (\page ->
                                page.view model.routeParams model.model
                                    |> Html.map Msg
                            )
                        |> Maybe.withDefault (text "")

                Nothing ->
                    text ""
    in
    { title = title
    , body =
        [ ThemeSpec.globalProviderWithDarkMode
            { light = ThemeSpec.lightTheme
            , dark = ThemeSpec.darkTheme
            , strategy = ThemeSpec.SystemStrategy
            }
        , ElmWidgets.globalStyles
        , ElmAdmin.Styles.globalStyles
        , div [ class "eadm eadm-wrapper" ]
            [ aside [ class "eadm eadm-sidebar" ]
                [ h1 [ class "eadm eadm-title" ] [ text title ]
                , UI.Nav.view model.activePath model.routeParams model.model navItems
                ]
            , main_ [ class "eadm eadm-main" ]
                [ activePage ]
            ]
        ]
    }
