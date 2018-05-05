#use "foo.ml"

open Bar

let foo x =
    if x = false then true else false

let quux asdf = asdf = true

let bar x y =
    if x then
        x == y
    else
        let foo = (x != y) in foo

let baz = function
| [] -> []
| xs -> (List.hd xs) :: (List.tl xs)

let cons h t = [h] @ t

let quux =
    if 1 = 2 then
        1
