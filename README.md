# gen_yojson
`gen_yojson` is a command-line and web utility for generating ocaml types based on a JSON object.
The types are annotated with
[ppx_deriving_yojson](https://github.com/ocaml-ppx/ppx_deriving_yojson) to automatically
generate serializers and deserializers using [yojson](https://github.com/ocaml-community/yojson). This project was inspired by
[MakeTypes](https://jvilk.com/MakeTypes/) for TypeScript.

A web version, made possible with jsoo is available at: https://alexchang8.github.io/gen_yojson/

TODO:
- some better names (i.e. elements)
- keys with spaces have field name changed, but are not annotated w/ `@key`
