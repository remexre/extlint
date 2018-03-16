open Core

type serializer
  = Datalog
  | Json

let main filename serializer =
    let src = In_channel.read_all filename in
    let ast = Ocaml_ast.parse_ast src filename in
    let out = match serializer with
        | Datalog -> let module S = Ocaml_ast.Make(Ser_datalog) in
            Ser_datalog.string_of_t (S.serialize_ast ast)
        | Json -> let module S = Ocaml_ast.Make(Ser_json) in
            Ser_json.string_of_t (S.serialize_ast ast)
    in
    print_endline out

let () =
    let args summary serializer =
        let open Command.Let_syntax in
        Command.basic
            ~summary:summary
            [%map_open
                let filename = anon ("filename" %: string)
                in
                fun () -> main filename serializer
            ]
    and serializer = Command.Arg_type.create
        (function
        | "datalog" -> Datalog
        | "json"    -> Json
        | s -> failwith ("Unknown serializer: " ^ s))
    in
    (match (Filename.basename Sys.executable_name) with
    | "datalog_of_ocaml" -> args "Converts an OCaml AST to Datalog literals." Datalog
    | "json_of_ocaml" -> args "Converts an OCaml AST to JSON." Json
    | _ ->
        let open Command.Let_syntax in
        Command.basic
            ~summary:"Converts an OCaml AST to a more usable data structure."
            [%map_open
                let filename =
                    anon ("filename" %: string)
                and serializer =
                    flag "format" (required serializer)
                        ~doc:"FORMAT The format to serialize to."
                in
                fun () -> main filename serializer
            ])
    |> Command.run
