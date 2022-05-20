module Admin.Http exposing (get)

{-|

@docs get

-}

import Http exposing (Error(..), Response(..))
import Json.Decode as D
import Platform exposing (Task)


{-| -}
get :
    { url : String
    , headers : List Http.Header
    , decoder : D.Decoder a
    }
    -> Task Http.Error a
get props =
    Http.task
        { method = "GET"
        , headers = []
        , url = props.url
        , body = Http.emptyBody
        , timeout = Nothing
        , resolver =
            Http.stringResolver
                (\response ->
                    case response of
                        GoodStatus_ _ body ->
                            case D.decodeString props.decoder body of
                                Ok v ->
                                    Ok v

                                Err e ->
                                    Err (BadBody (D.errorToString e))

                        BadUrl_ v ->
                            Err (BadUrl v)

                        Timeout_ ->
                            Err Timeout

                        NetworkError_ ->
                            Err NetworkError

                        BadStatus_ m _ ->
                            Err (BadStatus m.statusCode)
                )
        }
