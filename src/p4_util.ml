(**********************************************************************)
(* utility *)


(* copied from ocaml/utils/misc.ml *)
let string_of_ic' ic =
  let b = Buffer.create 0x10000 in
  let buff = String.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then Buffer.contents b else
      (Buffer.add_substring b buff 0 n; copy())
  in copy()

let read_ic_as_string ic = (
  try
    string_of_ic' ic
  with _ -> failwith "read_ic_as_string")

(* get contents of file as string; return option Some s if successful, None otherwise *)
let read_file_as_string fn = (
  try
    let ic = open_in fn in
    let s = string_of_ic' ic in
    let _ = close_in ic in
    Some s
  with _ -> None)

let write_string_to_oc s oc = (
  try
    let () = output_string oc s in
    true
  with _ -> false)

(* write a string to a file ; return true if successful, false otherwise *)
let write_string_to_file s fn = (
  try
    let oc = open_out fn in
    let _ = output_string oc s in
    let _ = close_out oc in
    true
  with _ -> false)
