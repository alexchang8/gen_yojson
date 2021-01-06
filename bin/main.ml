let ic_to_string ic =
  let rec ic_to_string acc =
    try
      let line = input_line ic in
      ic_to_string (line :: acc)
    with End_of_file -> acc
  in
  ic_to_string [] |> List.rev |> String.concat "\n"

let exec_cmd cmd =
  (* let ic, oc, ec = Unix.open_process_full cmd [||] in let output =
     ic_to_string ic in let status = Unix.close_process_full (ic, oc, ec) in
     output, status *)
  let ic = Unix.open_process_in cmd in
  let output = ic_to_string ic in
  let status = Unix.close_process_in ic in
  output, status

let ocamlformat s =
  let cmd =
    Printf.sprintf
      {|echo "%s" 2>/dev/null | opam exec ocamlformat -- --name="foo.ml" --enable-outside-detected-project - 2>/dev/null|}
      s
  in
  let output, status = exec_cmd cmd in
  match status with
  | Unix.WEXITED 0   -> output
  | Unix.WEXITED 127 ->
      Printf.eprintf "\n\nocamlformat missing";
      s
  | _                ->
      Printf.eprintf "\n\nunknown error executing ocamlformat";
      s

let run s =
  let contents =
    match s with
    | "-"      -> Core.In_channel.input_all Core.In_channel.stdin
    | filename -> Core.In_channel.read_all filename
  in
  contents |> Gen_yojson.Gen.gen_types |> ocamlformat |> print_endline

let command =
  let open Core.Command.Spec in
  Core.Command.basic_spec ~summary:"Generate OCaml types given a json object"
    ( empty
    +> anon (maybe_with_default "-" ("json file" %: Core.Filename.arg_type))
    )
    (fun file_name () -> run file_name)

let () = Core.Command.run ~version:"0.0.1" command
