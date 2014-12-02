(* header *)
open P4_lib

(* state created (particularly hashtables), so place inside a function that takes a unit *)
let p () = (
<<g<<
E -> E E E   {{ fun ((x,y),z) -> x+y+z }}
  | "1"      {{ fun _ -> 1 }}
  | ""       {{ fun _ -> 0 }}
  | F        {{ fun x -> x }}

F -> E       {{ fun x -> x }}

>>g>>
_E)

(* footer *)

let _E = p ()

let _ = run_parser_string (!_E) "1111111"

