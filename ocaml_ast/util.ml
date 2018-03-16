let (%) : 'a 'b 'c. ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c =
    (fun f g x -> f (g x))

let (%%) : 'a 'b 'c. ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c =
    (fun f g x -> g (f x))

let id x = x

let rec str_all_from f s n =
    if n >= String.length s then
        true
    else
        f (String.get s n) && str_all_from f s (n + 1)

let str_all f s = str_all_from f s 0

let sort_by_key (f: 'a -> 'b) : 'a list -> 'a list =
    List.fast_sort (fun a b -> compare (f a) (f b))

let comp_eff (f: ('a -> 'a) list) : 'a -> 'a =
    List.fold_left (%%) id f
