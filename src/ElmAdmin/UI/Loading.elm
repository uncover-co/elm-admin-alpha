module ElmAdmin.UI.Loading exposing (view)

import Svg as S exposing (Svg, path, svg)
import Svg.Attributes as SA


view : Svg msg
view =
    svg
        [ SA.class "eadm eadm-loading"
        , SA.viewBox "0 0 40 40"
        , SA.xmlSpace "preserve"
        ]
        [ path
            [ SA.fill "var(--tmspc-background-tint)"
            , SA.d "M20.201,5.169c-8.254,0-14.946,6.692-14.946,14.946c0,8.255,6.692,14.946,14.946,14.946 s14.946-6.691,14.946-14.946C35.146,11.861,28.455,5.169,20.201,5.169z M20.201,31.749c-6.425,0-11.634-5.208-11.634-11.634 c0-6.425,5.209-11.634,11.634-11.634c6.425,0,11.633,5.209,11.633,11.634C31.834,26.541,26.626,31.749,20.201,31.749z"
            ]
            []
        , path
            [ SA.fill "var(--tmspc-background-dark)"
            , SA.d "M26.013,10.047l1.654-2.866c-2.198-1.272-4.743-2.012-7.466-2.012h0v3.312h0 C22.32,8.481,24.301,9.057,26.013,10.047z"
            ]
            [ S.animateTransform
                [ SA.attributeType "xml"
                , SA.attributeName "transform"
                , SA.type_ "rotate"
                , SA.from "0 20 20"
                , SA.to "360 20 20"
                , SA.dur "0.8s"
                , SA.repeatCount "indefinite"
                ]
                []
            ]
        ]
