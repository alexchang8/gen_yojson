open Js_of_ocaml

let _ =
  Js.export "GenYojson"
    (object%js
       method gen s =
         s |> Js.to_string |> Gen_yojson.Gen.gen_types |> Js.string
    end)
