module Admin.Internal.NavItem exposing (NavItem(..))


type NavItem
    = NavItemExternal { url : String, label : String }
    | NavItemInternal
        { path : String
        , title : String
        , full : Bool
        , children : List NavItem
        }
