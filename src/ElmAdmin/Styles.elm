module ElmAdmin.Styles exposing (..)

import Html as H exposing (Html)

globalStyles : Html msg
globalStyles =
    H.node "style"
        []
        [ H.text """.eadm{box-sizing:border-box;margin:0;padding:0}@-webkit-keyframes eadm-fade-slide{0%{opacity:0;transform:translateY(10px)}to{opacity:1;transform:translateY(0)}}@keyframes eadm-fade-slide{0%{opacity:0;transform:translateY(10px)}to{opacity:1;transform:translateY(0)}}.eadm.eadm-fade-slide{-webkit-animation:eadm-fade-slide .4s;animation:eadm-fade-slide .4s}.eadm.eadm-link{color:inherit;text-decoration:none}.eadm.eadm-wrapper{background:var(--tmspc-background-contrast);display:flex;height:100vh;overflow:auto}.eadm.eadm-sidebar{background:var(--tmspc-background-base);box-shadow:0 0 8px var(--tmspc-background-shadow);position:relative;width:340px;z-index:1}.eadm.eadm-sidebar-header{align-items:center;border-bottom:1px solid var(--tmspc-background-tint);color:var(--tmspc-color-dark);display:flex;justify-content:space-between;padding:20px}.eadm.eadm-title{color:var(--tmspc-color-dark);font-family:var(--tmspc-font-title);font-size:20px;font-weight:500}.eadm.eadm-sidebar-dark-btn{background:transparent;border:none;color:var(--tmspc-color-light);cursor:pointer;font-size:18px;height:24px;line-height:18px;opacity:.75;transition:opacity .12s;width:24px}.eadm.eadm-sidebar-dark-btn:hover{opacity:1}.eadm.eadm-sidebar-dark-btn:active{opacity:.5}.eadm.eadm-nav{font-family:var(--tmspc-font-text)}.eadm.eadm-nav-list{list-style-type:none}.eadm.eadm-nav-list .eadm.eadm-nav-list .eadm.eadm-nav-item{padding-left:32px}.eadm.eadm-nav-list .eadm.eadm-nav-list .eadm.eadm-nav-list .eadm.eadm-nav-item{padding-left:44px}.eadm.eadm-nav-list .eadm.eadm-nav-list .eadm.eadm-nav-list .eadm.eadm-nav-list .eadm.eadm-nav-item{padding-left:58px;padding-left:70px}.eadm.eadm-nav-item{border:0 solid var(--tmspc-highlight-base);color:var(--tmspc-color-light);display:block;font-family:var(--tmspc-font-text);font-size:16px;padding:8px 20px;text-decoration:none;transition:border .12s}.eadm.eadm-nav-item:hover{background:var(--tmspc-background-tint)}.eadm.eadm-nav-item.eadm-m-group{color:var(--tmspc-color-dark)}.eadm.eadm-nav-item.eadm-m-visual-group:hover{background:transparent;cursor:default}.eadm.eadm-nav-item.eadm-m-active{background:var(--tmspc-highlight-tint);border-left-width:4px;color:var(--tmspc-highlight-dark)}.eadm.eadm-main{flex-grow:1;padding:20px}.eadm.eadm-page-title{color:var(--tmspc-color-base);font-family:var(--tmspc-font-title);font-size:20px;font-weight:500;padding-bottom:20px}.eadm.eadm-notification{background:var(--tmspc-background-base);border-radius:var(--tmspc-border-radius);border-top:var(--tmspc-border-radius) solid var(--tmspc-color-dark);box-shadow:0 1px 8px var(--tmspc-background-shadow);max-width:280px;padding:12px;position:fixed;right:20px;top:20px;width:100%}.eadm.eadm-notification.eadm-m-highlight{border-top-color:var(--tmspc-highlight-base)}.eadm.eadm-notification.eadm-m-success{border-top-color:var(--tmspc-success-base)}.eadm.eadm-notification.eadm-m-warning{border-top-color:var(--tmspc-warning-base)}.eadm.eadm-notification.eadm-m-danger{border-top-color:var(--tmspc-danger-base)}.eadm.eadm-notification-content{color:var(--tmspc-color-base);font-family:var(--tmspc-font-text);font-size:14px}.eadm.eadm-view{padding-bottom:20px}.eadm.eadm-card{background:var(--tmspc-background-base);border-radius:8px;box-shadow:0 0 8px var(--tmspc-background-shadow);display:block;overflow:hidden}.eadm.eadm-loading{max-height:100%;max-width:100%}.eadm.eadm-list-title{border-bottom:2px solid var(--tmspc-background-tint);color:var(--tmspc-color-light);font-family:var(--tmspc-font-title);font-size:12px;font-weight:500;letter-spacing:.5px;line-height:1em;padding:12px 16px;text-transform:uppercase}.eadm.eadm-list-item{align-items:center;color:var(--tmspc-color-base);display:flex;font-family:var(--tmspc-font-text);font-size:16px;justify-content:space-between;min-height:48px;padding:12px 16px}.eadm.eadm-list-empty{align-items:center;color:var(--tmspc-background-dark);display:flex;font-size:24px;font-weight:700;height:48px;justify-content:center;line-height:2px;padding:4px;text-align:center}.eadm.eadm-form-title{border-bottom:2px solid var(--tmspc-background-tint);color:var(--tmspc-color-light);font-family:var(--tmspc-font-title);font-size:12px;font-weight:500;letter-spacing:.5px;line-height:1em;padding:12px 16px;text-transform:uppercase}.eadm.eadm-form-fields{list-style-type:none}.eadm.eadm-form-footer{padding-top:8px}.eadm.eadm-form-footer-inner{border-top:2px solid var(--tmspc-background-tint);padding:12px 16px}.eadm.eadm-form-loading{display:flex;height:48px;justify-content:center;padding:4px}""" ]