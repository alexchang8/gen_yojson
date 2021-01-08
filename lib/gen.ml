module StringMap = Map.Make (String)

type field = { name : string; tpe : tree; optional : bool }

and tree =
  | Assoc   of field StringMap.t
  | Bool
  | Float
  | Int
  | Lst     of tree
  | String
  | Variant of tree list

let get_keys m = StringMap.bindings m |> List.map fst

let rec merge_trees t1 t2 =
  match t1, t2 with
  | a, b when a = b -> a
  | Assoc map1, Assoc map2 ->
      let keys = get_keys map1 @ get_keys map2 in
      let map =
        List.fold_left
          (fun acc x ->
            match StringMap.find_opt x map1, StringMap.find_opt x map2 with
            | Some field1, Some field2 ->
                let t = merge_trees field1.tpe field2.tpe in
                let field =
                  {
                    name = x;
                    tpe = t;
                    optional = field1.optional || field2.optional;
                  }
                in
                StringMap.add x field acc
            | Some field, None | None, Some field ->
                StringMap.add x { field with optional = true } acc
            | None, None -> failwith "impossible case")
          StringMap.empty keys
      in
      Assoc map
  | Variant v1, Variant v2 -> Variant (v1 @ v2)
  | (Lst _ as a), (Lst _ as b) -> Variant [ a; b ]
  | a, b -> Variant [ a; b ]

let rec tree_of_json (json : Yojson.Basic.t) : tree =
  match json with
  | `Assoc lst            ->
      let fields =
        List.map
          (fun (f_name, f_json) ->
            { name = f_name; tpe = tree_of_json f_json; optional = false })
          lst
      in
      let map =
        List.fold_left
          (fun acc x -> StringMap.add x.name x acc)
          StringMap.empty fields
      in
      Assoc map
  | `Bool _               -> Bool
  | `Float _              -> Float
  | `Int _                -> Int
  | `List (_ :: _ as lst) ->
      (* Merge records -> variants -> else, so that records are merged
         consistently regardless of list order*)
      let sorted =
        lst |> List.map tree_of_json
        |> List.sort (fun a b ->
               match a, b with
               | Assoc _, Assoc _     -> 0
               | Assoc _, _           -> -1
               | _, Assoc _           -> 1
               | Variant _, Variant _ -> 0
               | Variant _, _         -> -1
               | _, Variant _         -> 1
               | _, _                 -> 0)
      in
      let tpe =
        List.fold_left
          (fun acc x -> merge_trees acc x)
          (List.hd sorted) (List.tl sorted)
      in
      Lst tpe
  | `List []              -> failwith "unimplemented"
  | `Null                 -> failwith "unimplemented"
  | `String _             -> String

type lifted_field = {
  optional : bool;
  type_name : string;
  field_name : string;
  fixed_name : string option;
}

and lifted =
  | Inline  of string
  | Variant of string * string list
  | Record  of string * lifted_field StringMap.t

module MemoMap = Map.Make (struct
  type t = tree

  let compare = compare
end)

module StringSet = Set.Make (String)

let empty_used_names = "a", StringSet.empty

let incr_name s =
  let c = s.[String.length s - 1] in
  let c' = Char.code c + 1 |> Char.chr in
  match c' with
  | 'a' .. 'z' -> String.sub s 0 (String.length s - 1) ^ Char.escaped c'
  | _          -> s ^ "a"

let ocaml_keywords = StringSet.of_list Keywords.ocaml_keywords_lst

let valid_char = function
  | 'a' .. 'z' | '0' .. '9' | '_' | 'A' .. 'Z' -> true
  | _ -> false

let string_for_all s f =
  let rec h s f i =
    if i >= String.length s then true else f s.[i] && h s f (i + 1)
  in
  h s f 0

let valid_name s set =
  if s = "" || StringSet.mem s set then false
  else
    match s.[0] with
    | 'a' .. 'z' | '_' -> string_for_all s valid_char
    | _                -> false

let rec get_new_name suggested (next_generated, set) =
  let bad_name s =
    (not (valid_name s set)) || StringSet.mem s ocaml_keywords
  in
  match suggested with
  | None when bad_name next_generated ->
      let next_generated' = incr_name next_generated in
      get_new_name None (next_generated', set)
  | None ->
      let next_generated' = incr_name next_generated in
      let set' = StringSet.add next_generated set in
      next_generated, (next_generated', set')
  | Some s when bad_name s ->
      if s <> "" && s.[0] <> '_' then
        get_new_name (Some ("_" ^ s)) (next_generated, set)
      else get_new_name None (next_generated, set)
  | Some s -> s, (next_generated, StringSet.add s set)

let hoist lst =
  let rec hoist_h lst ((acc_inline, acc_hoisted) as acc) =
    match lst with
    | Inline s :: t -> hoist_h t (s :: acc_inline, acc_hoisted)
    | h :: t        -> hoist_h t (acc_inline, h :: acc_hoisted)
    | []            -> acc
  in
  let a, b = hoist_h lst ([], []) in
  List.rev a, List.rev b

let lift tree =
  let rec lift tree acc names suggested_name memo =
    let lifted, acc', names', memo' =
      lift_tree tree acc names suggested_name memo
    in
    match lifted with
    | Inline s -> s, acc', names', memo'
    | Variant (s, _) when MemoMap.mem tree memo -> s, acc', names', memo'
    | Variant (s, _) as v -> s, v :: acc', names', memo'
    | Record (s, _) when MemoMap.mem tree memo -> s, acc', names', memo'
    | Record (s, _) as r -> s, r :: acc', names', memo'
  and lift_tree tree acc names suggested_name memo =
    match MemoMap.find_opt tree memo with
    | Some x -> x, acc, names, memo
    | None   ->
        let lifted, acc', names', memo' =
          match tree with
          | Assoc map   ->
              let field_map, acc', names', memo' =
                List.fold_left
                  (fun (field_map, acc, names, memo) (name, field) ->
                    let s, acc', names', memo' =
                      lift field.tpe acc names (Some name) memo
                    in
                    let lifted_field =
                      {
                        optional = field.optional;
                        type_name = s;
                        field_name = field.name;
                        fixed_name = None;
                      }
                    in
                    let field_map' =
                      StringMap.add name lifted_field field_map
                    in
                    field_map', acc', names', memo')
                  (StringMap.empty, acc, names, memo)
                  (StringMap.bindings map)
              in
              let type_name, names'' = get_new_name suggested_name names' in
              Record (type_name, field_map), acc', names'', memo'
          | Bool        -> Inline "bool", acc, names, memo
          | Float       -> Inline "float", acc, names, memo
          | Int         -> Inline "int", acc, names, memo
          | Lst t       ->
              let s, acc', names', memo' = lift t acc names None memo in
              Inline (s ^ " list"), acc', names', memo'
          | String      -> Inline "string", acc, names, memo
          | Variant lst ->
              let var_lst, acc', names', memo' =
                List.fold_left
                  (fun (var_lst, acc, names, memo) x ->
                    let s, acc', names', memo' =
                      lift x acc names None memo
                    in
                    s :: var_lst, acc', names', memo')
                  ([], acc, names, memo) lst
              in
              let type_name, names'' = get_new_name suggested_name names' in
              Variant (type_name, var_lst), acc', names'', memo'
        in
        lifted, acc', names', MemoMap.add tree lifted memo'
  in
  let lifted, lst, _, _ =
    lift_tree tree [] ("a", StringSet.empty) (Some "t") MemoMap.empty
  in
  lifted, lst

let rec gen_next_field next used =
  if StringSet.mem next used then gen_next_field (incr_name next) used
  else next, incr_name next

let fix_field_name name next used =
  if valid_name name used then name, next, StringSet.add name used
  else
    let s = "_" ^ name in
    if valid_name s used then s, next, StringSet.add s used
    else
      let name', next' = gen_next_field next used in
      name', next', StringSet.add name' used

let fix_field_names lst =
  let _, _, res =
    List.fold_left
      (fun (next, used, acc) x ->
        let name, next', used' = fix_field_name x.field_name next used in
        next', used', { x with field_name = name } :: acc)
      ("a", StringSet.empty, [])
      lst
  in
  res

let lifted_toplevel_to_string = function
  | Inline _           -> failwith "should be no inline here"
  | Variant (s, types) ->
      let var_names =
        if List.for_all (fun x -> String.index_opt x ' ' = None) types then
          types
        else
          List.fold_left
            (fun (x, lst) _ -> incr_name x, x :: lst)
            ("a", []) types
          |> snd
      in
      let variant_s =
        List.map
          (fun (name, tpe) ->
            Printf.sprintf "| %s of %s" (String.capitalize_ascii name) tpe)
          (List.combine var_names types)
        |> String.concat "\n    "
      in
      Printf.sprintf "type %s = %s [@@deriving yojson]" s variant_s
  | Record (s, map)    ->
      let s_tpe =
        map |> StringMap.bindings |> List.map snd |> fix_field_names
        |> List.map (fun x ->
               let field_name, suffix =
                 if StringSet.mem x.field_name ocaml_keywords then
                   ( x.field_name ^ "_",
                     Printf.sprintf {|[@key "%s"];|} x.field_name )
                 else x.field_name, ";"
               in
               if x.optional then
                 Printf.sprintf "%s:(%s) option [@default None]%s" field_name
                   x.type_name suffix
               else Printf.sprintf "%s:%s%s" field_name x.type_name suffix)
      in
      let record_contents = String.concat "\n    " s_tpe in
      Printf.sprintf "type %s = {%s} [@@deriving yojson]" s record_contents

let gen_types s =
  let json = Yojson.Basic.from_string s in
  let tree = tree_of_json json in
  let lifted, lst = lift tree in
  let types = List.map lifted_toplevel_to_string lst in
  let root_str =
    match lifted with
    | Inline s -> "type t = " ^ s
    | x        -> lifted_toplevel_to_string x
  in
  root_str :: types |> List.rev |> String.concat "\n"
