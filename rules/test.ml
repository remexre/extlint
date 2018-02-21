let foo x =
    if x = true then true else false

let bar x y =
    if x then
        x == y
    else
        x != y

let baz y =
    let x = y in x
