open Eval
open Typing
open Parse

let base = Env.empty
let interpret (s : string) : value = s |> parse |> typecheck |> eval base
