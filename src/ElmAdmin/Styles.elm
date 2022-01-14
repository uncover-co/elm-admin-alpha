module ElmAdmin.Styles exposing (..)

import Html as H exposing (Html)

globalStyles : Html msg
globalStyles =
    H.node "style"
        []
        [ H.text """.eadm{box-sizing:border-box;margin:0;padding:0}.eadm.eadm-wrapper{background:var(--tmspc-background-base);display:flex;height:100vh;overflow:auto}.eadm.eadm-sidebar{background:var(--tmspc-background-tint);box-shadow:0 0 8px var(--tmspc-background-shadow);position:relative;width:340px;z-index:1}.eadm.eadm-title{border-bottom:1px solid var(--tmspc-background-light);color:var(--tmspc-color-dark);font-family:var(--tmspc-font-title);font-size:20px;font-weight:500;padding:20px}.eadm.eadm-nav{font-family:var(--tmspc-font-text)}.eadm.eadm-nav-group{list-style-type:none}.eadm.eadm-nav-group-title{color:var(--tmspc-color-base);font-size:14px;letter-spacing:.05em;padding:8px 20px}.eadm.eadm-nav-item{color:var(--tmspc-color-light);font-size:16px;padding:8px 20px}.eadm.eadm-nav-item:hover{background:var(--tmspc-background-light)}.eadm.eadm-nav-item.m-group{padding:0}.eadm.eadm-nav-item.m-group:hover{background:transparent}.eadm.eadm-main{flex-grow:1}""" ]