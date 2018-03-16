type t =
| Array of t list
| Bool of bool
| Int of int
| Null
| Object of (string * t) list
| String of string

(* Escapes a string according to JSON rules. *)
let escape_string (s: string) : string =
    let escape_char (ch: char) : string =
    match Char.code ch with
    |  0 -> "\\u0000"
    |  1 -> "\\u0001"
    |  2 -> "\\u0002"
    |  3 -> "\\u0003"
    |  4 -> "\\u0004"
    |  5 -> "\\u0005"
    |  6 -> "\\u0006"
    |  7 -> "\\u0007"
    |  8 -> "\\b"
    |  9 -> "\\t"
    | 10 -> "\\n"
    | 11 -> "\\u000b"
    | 12 -> "\\f"
    | 13 -> "\\r"
    | 14 -> "\\u000e"
    | 15 -> "\\u000f"
    | 16 -> "\\u0010"
    | 17 -> "\\u0011"
    | 18 -> "\\u0012"
    | 19 -> "\\u0013"
    | 20 -> "\\u0014"
    | 21 -> "\\u0015"
    | 22 -> "\\u0016"
    | 23 -> "\\u0017"
    | 24 -> "\\u0018"
    | 25 -> "\\u0019"
    | 26 -> "\\u001a"
    | 27 -> "\\u001b"
    | 28 -> "\\u001c"
    | 29 -> "\\u001d"
    | 30 -> "\\u001e"
    | 31 -> "\\u001f"
    | 34 -> "\\\""
    | 92 -> "\\\\"
    | ch -> String.make 1 (Char.chr ch)
    in
    let explode (s: string) : char list =
      let rec expl i l =
        if i < 0 then l else
        expl (i - 1) (s.[i] :: l) in
      expl (String.length s - 1) []
    in
    let quote (s: string) : string =
        "\"" ^ s ^ "\""
    in
    explode s
    |> List.map escape_char
    |> String.concat ""
    |> quote

let rec string_of_t = function
| Array(a) -> "[" ^ String.concat "," (List.map string_of_t a) ^ "]"
| Bool(b) -> string_of_bool b
| Int(i) -> string_of_int(i)
| Null -> "null"
| Object(o) -> let helper (n, v) = escape_string n ^ ":" ^ string_of_t v in
                   "{" ^ String.concat "," (List.map helper o) ^ "}"
| String(s) -> escape_string s

let serialize_bool b = Bool(b)
let serialize_int i = Int(i)
let serialize_null = Null
let serialize_string s = String(s)
let serialize_seq ts = Array(ts)
let serialize_struct _ vs = Object(vs)

let serialize_option = function
| Some(x) -> x
| None    -> Null

let serialize_variant ty value =
    Object([
        ("type", String(ty));
        ("value", value);
    ])
