open P3_core

(* interactively develop this file with P3_lib, so we get better type info 
open P3_lib
*)

let memo f = (
  let tbl = Hashtbl.create 100 in
  let key_of_input i = i in
  fun i -> 
    let k = key_of_input i in
    if (Hashtbl.mem tbl k) then 
      (Hashtbl.find tbl k) 
    else
      let v = f i in
      let _ = Hashtbl.add tbl k v in
      v)

(* default action *)
let default_act = fun x -> [x]

(* string -> parser3' *)
let a = 
  let rp lit = (fun (`SS(s,i,j)) -> 
    let n = String.length lit in
    if
      (n <= j-i)
      && (String.sub s i n = lit)
    then
      [i+n]
    else
      [])
  in
  let f lit = mktmparser (rp lit) default_act in
  memo f

let (_:string -> (string, string ty_span)parser3) = a

(* FIXME change this to take an underlying parser *)
let until_a =
  let rp lit = fun (`SS(s,i,j)) ->
    let llit = String.length lit in
    let rec f1 n =
      if
        n+llit <= j-i
        && (String.sub s (i+n) llit) = lit
      then
        [i+n]
      else if
          n+llit <= (j-i)
      then
        f1 (n+1)
      else
        [j] (* read till the end of the string *)
    in
    f1 0
  in
  let f lit = mktmparser (rp lit) default_act in
  memo f

let (_:string -> (string, string ty_span)parser3) = until_a


let f1 re = (
  let re = Str.regexp re in
  let f2 = fun (`SS(s,i,j)) -> (
    let b = Str.string_match re s i in
    if b then
      let e = Str.match_end () in
      if e<=j then 
        [e]
      else
        []
    else
      [])
  in
  mktmparser f2 default_act)

let parse_RE = memo f1

let (_:string -> (string, string ty_span)parser3) = parse_RE


let f1 re = (
  let re = Str.regexp re in
  let f2 = fun (`SS(s,i,j)) -> (
    try
      let k = Str.search_forward re s i in
      if k<=j then [k] else []
    with Not_found -> [j])  (* read till end of string *)
  in
  mktmparser f2 default_act)

let parse_not_RE = memo f1

let (_:string -> (string, string ty_span)parser3) = parse_not_RE
