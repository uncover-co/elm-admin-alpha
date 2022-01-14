module ElmAdmin.Experiments exposing (..)


type Form a
    = Form a


form_ : a -> Form a
form_ a =
    Form a


map2 : (a -> b -> c) -> Form a -> Form b -> Form c
map2 ac (Form a) (Form b) =
    Form (ac a b)


andMap : Form a -> Form (a -> b) -> Form b
andMap =
    map2 (|>)


x : Form String
x =
    form_ (\b _ _ -> b)
        |> andMap (Form "b")
        |> andMap (Form "c")
        |> andMap (Form "d")
