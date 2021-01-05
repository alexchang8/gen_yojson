# gen_yojson [WIP]

`gen_yojson` is a command-line utility for generating ocaml types based on a JSON object.
The types are annotated with
[ppx_deriving_yojson](https://github.com/ocaml-ppx/ppx_deriving_yojson) to automatically
generate serializers and deserializers using [yojson](https://github.com/ocaml-community/yojson). This project was inspired by
[MakeTypes](https://jvilk.com/MakeTypes/) for TypeScript.
