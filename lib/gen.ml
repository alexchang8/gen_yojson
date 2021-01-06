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
  | `Assoc lst     ->
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
  | `Bool _        -> Bool
  | `Float _       -> Float
  | `Int _         -> Int
  | `List (h :: t) ->
      let tpe =
        List.fold_left
          (fun acc x -> merge_trees acc (tree_of_json x))
          (tree_of_json h) t
      in
      Lst tpe
  | `List []       -> failwith "unimplemented"
  | `Null          -> failwith "unmplemented"
  | `String _      -> String

type lifted_field = {
  optional : bool;
  type_name : string;
  field_name : string;
}

and lifted =
  | Inline  of string
  | Variant of string * string list
  | Record  of string * lifted_field StringMap.t

module StringSet = Set.Make (String)

let empty_used_names = "a", StringSet.empty

let incr_name s =
  let c = s.[String.length s - 1] in
  let c' = Char.code c + 1 |> Char.chr in
  match c' with
  | 'a' .. 'z' -> String.sub s 0 (String.length s - 1) ^ Char.escaped c'
  | _          -> s ^ "a"

let rec get_new_name suggested (next_generated, set) =
  match suggested with
  | None when StringSet.mem next_generated set ->
      let next_generated' = incr_name next_generated in
      get_new_name None (next_generated', set)
  | None ->
      let next_generated' = incr_name next_generated in
      let set' = StringSet.add next_generated set in
      next_generated, (next_generated', set')
  | Some s when StringSet.mem s set -> get_new_name None (next_generated, set)
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
  let rec lift tree acc names suggested_name =
    let lifted, acc', names' = lift_tree tree acc names suggested_name in
    match lifted with
    | Inline s            -> s, acc', names'
    | Variant (s, _) as v -> s, v :: acc', names'
    | Record (s, _) as r  -> s, r :: acc', names'
  and lift_tree tree acc names suggested_name =
    match tree with
    | Assoc map   ->
        let field_map, acc', names' =
          List.fold_left
            (fun (field_map, acc, names) (name, field) ->
              let s, acc', names' = lift field.tpe acc names (Some name) in
              let lifted_field =
                {
                  optional = field.optional;
                  type_name = s;
                  field_name = field.name;
                }
              in
              let field_map' = StringMap.add name lifted_field field_map in
              field_map', acc', names')
            (StringMap.empty, acc, names)
            (StringMap.bindings map)
        in
        let type_name, names'' = get_new_name suggested_name names' in
        Record (type_name, field_map), acc', names''
    | Bool        -> Inline "bool", acc, names
    | Float       -> Inline "float", acc, names
    | Int         -> Inline "int", acc, names
    | Lst t       ->
        let s, acc', names' = lift t acc names None in
        Inline (s ^ " list"), acc', names'
    | String      -> Inline "string", acc, names
    | Variant lst ->
        let var_lst, acc', names' =
          List.fold_left
            (fun (var_lst, acc, names) x ->
              let s, acc', names' = lift x acc names None in
              s :: var_lst, acc', names')
            ([], acc, names) lst
        in
        let type_name, names'' = get_new_name suggested_name names' in
        Variant (type_name, var_lst), acc', names''
  in
  let lifted, lst, _ = lift_tree tree [] ("a", StringSet.empty) (Some "t") in
  lifted, lst

let lifted_toplevel_to_string = function
  | Inline _        -> failwith "should be no inline here"
  | Variant _       -> failwith "unimplemented"
  | Record (s, map) ->
      (*TODO: deal with optional...*)
      let s_tpe =
        map |> StringMap.bindings
        |> List.map (fun (_, x) ->
               if x.optional then
                 Printf.sprintf "%s:(%s) option;" x.field_name x.type_name
               else Printf.sprintf "%s:%s;" x.field_name x.type_name)
      in
      Printf.sprintf "type %s = {%s}" s (String.concat "\n" s_tpe)

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
