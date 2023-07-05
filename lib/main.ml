open Eval
open Typing

let base = Env.empty
let parse _ = failwith "Not implemented"
let interpret s = s |> parse |> typecheck |> eval base
