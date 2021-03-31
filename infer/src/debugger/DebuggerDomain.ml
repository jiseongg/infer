(* Empty domain for debugger *)

open! IStd
module F = Format

type t = unit

let leq ~lhs:_ ~rhs:_ = assert false

let join _a _b = assert false

let widen ~prev:_ ~next:_ ~num_iters:_ = assert false

let pp fmt () = F.fprintf fmt "(nothing)"

let initial = ()

type summary = t
