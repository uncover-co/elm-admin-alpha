module ElmAdmin.Libs.List exposing (find)


find: (a -> Bool) -> List a -> Maybe a
find fn xs =
    case xs of
        [] ->
            Nothing

        h :: xs_ ->
            if fn h then
                Just h

            else
                find fn xs_
