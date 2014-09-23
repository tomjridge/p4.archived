open P4_lib
(*
open P4_core
open P4_e3
*)

let c = content

(* comparing two lists, which represent sets, ignoring ordering *)
let set_equal xs ys = (
  let subset xs ys = List.for_all (fun x -> List.mem x ys) xs in
  subset xs ys && subset ys xs)


let mkntparser_ref r alts = 
  let _ = r := mkntparser (!r) alts in
  ()

let mkntparser_lazy p lazy_alts = mkntparser p (fun () -> Lazy.force lazy_alts)

(* examples *)
let parse_1 = (a "1") >>> (fun (`SS(s,i,j)) -> 1)
let parse_eps = (a "") >>> (fun _ -> 0)


(* example with no memoization *)
let parse_E = ref (mk_pre_parser ())

(* FIXME are we really sure the actions are getting set properly? *)

let _ = 
  mkntparser_ref parse_E (
    let alts = lazy(alts [
      (!parse_E >-- !parse_E >- !parse_E) >> (fun ((x,y),z) -> x+y+z);
      rhs parse_1;
      rhs parse_eps])
    in
    fun () -> Lazy.force alts)

let p = !parse_E
let txt = "111111"
let _ = assert([6] = run_parser3_string p txt)


(* example with explicit memoization *)

let parse_E = ref (mk_pre_parser ())

let _ = 
  mkntparser_ref parse_E (
    let alts = lazy(alts [
      (!parse_E >-- !parse_E >- !parse_E) >> (fun ((x,y),z) -> x+y+z);
      rhs parse_1;
      rhs parse_eps])
    in
    fun () -> Lazy.force alts)

let _ = 
  let tbl = Hashtbl.create 100 in
  parse_E := memo_p3 tbl (!parse_E)

let p = !parse_E
(* if running in try.ocamlpro.com, this blows the stack :( 

   try.ocamlpro.com has 15538 stack size; "normal" ocaml is about 262067  

   http://stackoverflow.com/questions/7826992/browser-javascript-stack-size-limit

   http://rosettacode.org/wiki/Find_limit_of_recursion#OCaml
*)
let txt = "111111111111111111111111111111"
let _ = assert ([30] = run_parser3_string p txt)


(* parse trees *)
type pt = [ `LF of string | `Node of pt * pt * pt ]

let parse_E : (string,pt)parser3 identified ref = ref (mk_pre_parser ())

let _ = 
  mkntparser_ref parse_E (
    let alts = lazy(alts [
      (!parse_E >-- !parse_E >- !parse_E) >> (fun ((x,y),z) -> `Node(x,y,z));
      (rhs parse_1) >> (fun _ -> `LF("1"));
      (rhs parse_eps) >> (fun _ -> `LF(""))])
    in
    fun () -> Lazy.force alts)

let p = !parse_E
let txt = "11"
let _ = assert (
  set_equal 
    [`Node (`LF "", `LF "1", `LF "1"); `Node (`LF "1", `LF "", `LF "1"); `Node (`LF "1", `LF "1", `LF "")]
    (run_parser3_string p txt))


(* defining other combinators; note that these definitions work
   regardless of the input type (lexed tokens, strings, etc) *)
let parse_maybe p =
  let p' = mk_pre_parser () in
  let alts = alts [
    (rhs parse_eps) >> (fun _ -> None);
    (rhs p) >> (fun x -> Some x)]
  in
  mkntparser p' (fun () -> alts)

let p = (parse_maybe parse_1)
let _ = assert ([Some 1] = run_parser3_string p "1")
let _ = assert ([None] = run_parser3_string p "")
let _ = assert ([] = run_parser3_string p "11")


(* iterate a parser n times; following is purely meta - don't need to
   define new nts *)
let rec itern n p = (
  let p' = mk_pre_parser () in
  let rec rhs' m = (match m with 
    | 0 -> (rhs parse_eps >> (fun _ -> []))
    | _ -> ((rhs' (m-1) >- p) >> (fun (xs,x) -> xs@[x])))
  in
  let alts = alts[rhs' n] in
  mkntparser p' (fun () -> alts))

let p = itern 5 parse_1
let txt = "11111"
let _ = assert ([[1; 1; 1; 1; 1]] = run_parser3_string p txt)


(* no memo; star aka many *)
let star p = (
  let star_p = ref(mk_pre_parser ()) in
  let alts = lazy(alts[
    rhs parse_eps >> (fun _ -> []);
    (p >-- !star_p) >> (fun (x,xs) -> x::xs)])
  in
  let _ = star_p := mkntparser !star_p (fun () -> Lazy.force alts) in
  !star_p)


let rec parse_E = (star parse_1)

let p = parse_E
let _ = assert([[1; 1; 1; 1; 1]] = run_parser3_string p "11111")
let _ = assert ([[]] = run_parser3_string p "")

(* the following gives the same (perhaps unexpected) result as the
   above; we only allow good trees! Note the star(parse_eps |||| ...) *)
let rec parse_E = 
  let alts = lazy(alts[
    rhs parse_eps;
    rhs parse_1])
  in
  let p = mkntparser (mk_pre_parser ()) (fun () -> Lazy.force alts) in
  star p

let _ = assert([[1; 1; 1; 1; 1]] = run_parser3_string parse_E "11111")


(* 1 or more *)
let many1 p = 
  let alts = lazy(alts[
    (p >-- (star p)) >> (fun (x,xs) -> x::xs)])
  in
  let q = mkntparser_lazy (mk_pre_parser ()) alts in
  q

let rec parse_E = (many1 parse_1)

let p = parse_E
let _ = assert([[1; 1; 1; 1; 1]] = run_parser3_string p "11111")
let _ = assert ([] = run_parser3_string p "")


(* sepby1, from "Monadic Parser Combinators", Hutton & Meijer, 1996 *)
let sepby1 p sep = (
  let sep_p = mkntparser_lazy (mk_pre_parser ()) (lazy(alts[
    sep >-- p >> (fun (_,x) -> x)]))
  in
  let alts = lazy(alts[
    rhs p >> (fun x -> [x]);
    p >-- (many1 sep_p) >> (fun (x,xs) -> x::xs)])
  in
  mkntparser_lazy (mk_pre_parser ()) alts)

let p = sepby1 parse_1 (a ";")
let _ = assert([[1;1;1;1]] = run_parser3_string p "1;1;1;1")


(* bracket, from "Monadic Parser Combinators" *)
let bracket op p cl = 
  let alts = lazy(alts[
    (op >-- p >- cl) >> (fun ((_,x),_) -> x)])
  in
  mkntparser_lazy (mk_pre_parser()) alts

let asemi = a ";"
let p1 = sepby1 parse_1 asemi
let abra = a "["
let aket = a "]"
let p = bracket abra p1 aket
let txt = "[1;1;1;1]"
let _ = assert([[1;1;1;1]] = run_parser3_string p txt)

(* etc etc *)

(* parse_not_RE parses until it hits a regular expression, or the end of string *)
let p = mkntparser_lazy (mk_pre_parser()) (lazy(alts[
  ((parse_not_RE "X") >-- (parse_RE ".*")) >> (fun (x,y) -> (c x,c y))]))

let [("abc","Xdef")] = run_parser3_string p "abcXdef"
let [("abcdef","")] = run_parser3_string p "abcdef"


(**********************************************************************)
(* going beyond context-free grammars *)

(* _G 1 parses "1", or "1" followed by _G 2; in general, _G n parses
   the number n, optionally followed by _G (n+1); there are an infinite
   number of nonterminals (_G n) generated lazily on demand *)


let _G = ref (fun n -> failwith "")

let _ = _G := fun n -> 
  let an = a (string_of_int n) in
  let alts = lazy(alts[
    (rhs an) >> (fun s -> c s);
    (an >-- (!_G (n+1))) >> (fun (x,y) -> (c x)^y)])
  in
  mkntparser_lazy (mk_pre_parser ()) alts

let memo tbl f i = (
  let k = i in
  if (Hashtbl.mem tbl k) then 
    (Hashtbl.find tbl k) 
  else
    let v = f i in
    let _ = Hashtbl.add tbl k v in
    v)

let _ = 
  let tbl = Hashtbl.create 100 in
  let _ = _G := memo tbl !_G in
  ()

let p = !_G 1

let _ = assert(["1234567891011"] = run_parser3_string p "1234567891011")
let _ = assert([] = run_parser3_string p "1234567891012")
