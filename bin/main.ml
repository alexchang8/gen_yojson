let ic_to_string ic =
  let rec ic_to_string acc =
    try
      let line = input_line ic in
      ic_to_string (line :: acc)
    with End_of_file -> acc
  in
  ic_to_string [] |> List.rev |> String.concat "\n"

let exec_cmd cmd =
  let ic = Unix.open_process_in cmd in
  let output = ic_to_string ic in
  let status = Unix.close_process_in ic in
  output, status

let ocamlformat s =
  let tmp_name, tmp_oc = Filename.open_temp_file "tmp" ".ml" in
  output_string tmp_oc s;
  flush tmp_oc;
  close_out_noerr tmp_oc;
  let cmd =
    Printf.sprintf
      {|opam exec ocamlformat -- --enable-outside-detected-project %s 2>/dev/null|}
      tmp_name
  in
  let output, status = exec_cmd cmd in
  Sys.remove tmp_name;
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
    | "-"      -> ic_to_string stdin
    | filename ->
        let ic = open_in filename in
        let res = ic_to_string ic in
        close_in ic;
        res
  in
  contents |> Gen_yojson.Gen.gen_types |> ocamlformat |> print_endline

let () =
  let num_args = Array.length Sys.argv in
  let summary = "Generate OCaml types given a json object" in
  let exec_name = Filename.basename Sys.argv.(0) in
  let usage = Printf.sprintf "%s [JSON FILE]" exec_name in
  if num_args = 2 then
    match Sys.argv.(1) with
    | "-h" | "--help" -> Printf.printf "%s\n\n  %s\n" summary usage
    | "" ->
        Printf.eprintf "Error: filename must be nonempty\n";
        exit 1
    | x when String.length x >= 2 && x.[0] = '-' ->
        Printf.eprintf "%s: unknown option '%s'\n" exec_name x;
        Printf.eprintf "Usage: %s\n" usage
    | f -> run f
  else print_endline ("Expected a single arg. Usage: " ^ usage)
