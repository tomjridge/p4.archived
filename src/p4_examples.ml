open P4_lib
(*
open P4_core
open P4_e3
*)

(* update GC params *)

let _ = 
  let open Gc in
  set { (get()) with max_overhead=1000000; space_overhead=1000000 }

(* to get a visual indication of runtime *)
let start_stop s f = 
  let t1 = Sys.time () in
  let _ = print_string ("Start "^s^" ...") in
  let _ = f () in
  let t2 = Sys.time () in
  let _ = print_endline ("...stop in "^(string_of_float (t2 -. t1))^" seconds") in
  ()

let c = content

(* comparing two lists, which represent sets, ignoring ordering *)
let set_equal xs ys = (
  let subset xs ys = List.for_all (fun x -> List.mem x ys) xs in
  subset xs ys && subset ys xs)


(*
let mkntparser_ref r alts = 
  let _ = r := mkntparser (!r) alts in
  ()
*)


(**********************************************************************)
(* examples *)


let parse_1 = (a "1") >>> (fun (`SS(s,i,j)) -> 1)
let parse_eps = (a "") >>> (fun _ -> 0)


(* example with no memoization *)

(* FIXME are we really sure the actions are getting set properly? *)

let parse_E = 
  let _E = ref (mk_pre_parser ()) in
  let _ = _E := mkntparser_lazy (!_E) (lazy(alts [
      (!_E >-- !_E >- !_E) >> (fun ((x,y),z) -> x+y+z);
      rhs parse_1;
      rhs parse_eps]))
  in
  !_E

let p = parse_E
let txt = "111111"
let _ = assert([6] = run_parser_string p txt)


(* example with explicit memoization *)


let parse_E = 
  let _E = ref (mk_pre_parser ()) in
  let _ = _E := mkntparser_lazy (!_E) (lazy(alts[
      (!_E >-- !_E >- !_E) >> (fun ((x,y),z) -> x+y+z);
      rhs parse_1;
      rhs parse_eps]))
  in
  _E

let _ =
  let tbl = Hashtbl.create 100 in
  parse_E := memo_p tbl (!parse_E)

let p = !parse_E
(* if running in try.ocamlpro.com, this blows the stack :( 

   try.ocamlpro.com has 15538 stack size; "normal" ocaml is about 262067  

   http://stackoverflow.com/questions/7826992/browser-javascript-stack-size-limit

   http://rosettacode.org/wiki/Find_limit_of_recursion#OCaml
*)
let txt = "111111111111111111111111111111"
let _ = assert ([30] = run_parser_string p txt)


(* mutual recursion *)

let _E = ref (mk_pre_parser ()) 
let _F = ref (mk_pre_parser ()) 
let _ = 
  _E := mkntparser_lazy (!_E) (lazy(alts[
      (!_E >-- !_E >- !_E) >> (fun ((x,y),z) -> x+y+z);
      (rhs !_F) >> (fun x -> x);
      rhs parse_1;
      rhs parse_eps]));
  _F := mkntparser_lazy (!_F) (lazy(alts[
      (!_E >-- !_E) >> (fun (x,y) -> x+y) ]))

let p = !_E
let txt = "11111"
let _ = assert ([5] = run_parser_string p txt)


(**********************************************************************)
(* some timing examples *)

let parse_E () = 
  let _E = ref (mk_pre_parser ()) in
  let _ = _E := mkntparser_lazy (!_E) (lazy(alts [
      (!parse_E >-- !parse_E >- !parse_E) >> (fun ((x,y),z) -> x+y+z);
      rhs parse_1;
      rhs parse_eps]))
  in
  let _ = 
    let tbl = Hashtbl.create 100 in
    _E := memo_p tbl (!_E)
  in
  !_E

let p = parse_E ()
let txt = String.make 20 '1'
let _ = assert ([20] = run_parser_string p txt)

let f () = String.make 20 '1' |> run_parser_string (parse_E ())
let _ = start_stop "example ldf" f

let f () = String.make 40 '1' |> run_parser_string (parse_E ())
let _ = start_stop "example nkv" f

let f () = String.make 60 '1' |> run_parser_string (parse_E ())
let _ = start_stop "example yq5" f

(* with dummy actions *)
let parse_E () = 
  let _E = ref (mk_pre_parser ()) in
  let _ = _E := mkntparser_lazy (!_E) (lazy(alts[
      (!_E >-- !_E >- !_E) >> (fun _ -> ());
      (rhs parse_1) >> (fun _ -> ());
      (rhs parse_eps) >> (fun _ -> ())]))
  in
  let _ = 
    let tbl = Hashtbl.create 100 in
    _E := memo_p tbl (!_E)
  in
  !_E

let f () = String.make 20 '1' |> run_parser_string (parse_E ())
let _ = start_stop "example 7jv" f

let f () = String.make 40 '1' |> run_parser_string (parse_E ())
let _ = start_stop "example mqu" f

let f () = String.make 60 '1' |> run_parser_string (parse_E ())
let _ = start_stop "example ls4" f

(* Sample output:
Start example ldf ......stop in 0.01427 seconds
Start example nkv ......stop in 0.166413 seconds
Start example yq5 ......stop in 0.774756 seconds
Start example 7jv ......stop in 0.015108 seconds
Start example mqu ......stop in 0.165307 seconds
Start example ls4 ......stop in 0.772494 seconds
*)


(**********************************************************************)
(* further examples, combinators *)



(* parse trees *)
type pt = [ `LF of string | `Node of pt * pt * pt ]

let parse_E : (string,pt) parser3 identified = 
  let _E = ref (mk_pre_parser ()) in
  let _ = _E := mkntparser_lazy (!_E) (lazy(alts [
      (!_E >-- !_E >- !_E) >> (fun ((x,y),z) -> `Node(x,y,z));
      (rhs parse_1) >> (fun _ -> `LF("1"));
      (rhs parse_eps) >> (fun _ -> `LF(""))]))
  in
  !_E

let p = parse_E
let txt = "11"
let _ = assert (
  set_equal 
    [`Node (`LF "", `LF "1", `LF "1"); `Node (`LF "1", `LF "", `LF "1"); `Node (`LF "1", `LF "1", `LF "")]
    (run_parser_string p txt))


(* defining other combinators; note that these definitions work
   regardless of the input type (lexed tokens, strings, etc) *)
let parse_maybe p =
  let alts = alts [
    (rhs parse_eps) >> (fun _ -> None);
    (rhs p) >> (fun x -> Some x)]
  in
  mkntparser (mk_pre_parser()) (fun () -> alts)

let p = (parse_maybe parse_1)
let _ = assert ([Some 1] = run_parser_string p "1")
let _ = assert ([None] = run_parser_string p "")
let _ = assert ([] = run_parser_string p "11")


(* iterate a parser n times; following is purely meta - don't need to
   define new nts *)
let rec itern n p = (
  let rec rhs' m = (match m with 
    | 0 -> (rhs parse_eps >> (fun _ -> []))
    | _ -> ((rhs' (m-1) >- p) >> (fun (xs,x) -> xs@[x])))
  in
  let alts = alts[rhs' n] in
  mkntparser (mk_pre_parser ()) (fun () -> alts))

let p = itern 5 parse_1
let txt = "11111"
let _ = assert ([[1; 1; 1; 1; 1]] = run_parser_string p txt)


(* no memo; star aka many *)
let star p = (
  let star_p = ref(mk_pre_parser ()) in
  let alts = lazy(alts[
    rhs parse_eps >> (fun _ -> []);
    (p >-- !star_p) >> (fun (x,xs) -> x::xs)])
  in
  let _ = star_p := mkntparser_lazy !star_p alts in
  !star_p)


let rec parse_E = (star parse_1)

let p = parse_E
let _ = assert([[1; 1; 1; 1; 1]] = run_parser_string p "11111")
let _ = assert ([[]] = run_parser_string p "")

(* the following gives the same (perhaps unexpected) result as the
   above; we only allow good trees! Note the star(parse_eps |||| ...) *)
let rec parse_E = 
  let alts = lazy(alts[
    rhs parse_eps;
    rhs parse_1])
  in
  let p = mkntparser_lazy (mk_pre_parser ()) alts in
  star p

let _ = assert([[1; 1; 1; 1; 1]] = run_parser_string parse_E "11111")


(* 1 or more *)
let many1 p = 
  let alts = lazy(alts[
    (p >-- (star p)) >> (fun (x,xs) -> x::xs)])
  in
  let q = mkntparser_lazy (mk_pre_parser ()) alts in
  q

let rec parse_E = (many1 parse_1)

let p = parse_E
let _ = assert([[1; 1; 1; 1; 1]] = run_parser_string p "11111")
let _ = assert ([] = run_parser_string p "")


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
let _ = assert([[1;1;1;1]] = run_parser_string p "1;1;1;1")


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
let _ = assert([[1;1;1;1]] = run_parser_string p txt)

(* etc etc *)

(* parse_not_RE parses until it hits a regular expression, or the end of string *)
let p = mkntparser_lazy (mk_pre_parser()) (lazy(alts[
  ((parse_not_RE "X") >-- (parse_RE ".*")) >> (fun (x,y) -> (c x,c y))]))

let [("abc","Xdef")] = run_parser_string p "abcXdef"
let [("abcdef","")] = run_parser_string p "abcdef"


(**********************************************************************)
(* lambda calculus example *)

type lambda = [ 
    `App of 'a * 'a
  | `Bracket of 'a 
  | `Lam of string * 'a
  | `Var of string ] 
  constraint 'a = lambda

let parse_lambda : (string,lambda) parser3 identified = 
  let p = ref (mk_pre_parser()) in
  let w = parse_RE "[ ]*" in (* whitespace *)
  let v = parse_RE "[a-z]" in (* variables *)
  let ( >- ) p q = (p >- w >- q) >> (fun ((x,_),y) -> (x,y)) in (* parsers separated by whitespace *)
  let alts = lazy(alts[
      ((rhs (a "\\")) >- v >- !p)      >> (fun ((_,x),body) -> `Lam(c x,body));  (* \\ x body *)
      ((rhs !p) >- !p)                 >> (fun (x,y) -> `App(x,y));  (* p q - application *)
      (rhs v)                          >> (fun x -> `Var (c x));  (* x - variable *)
      ((rhs (a "(")) >- !p >- (a ")")) >> (fun ((_,body),_) -> `Bracket(body))  (* ( body ) *)
      ])
  in
  let _ = p := mkntparser_lazy (!p) alts in
  !p

let _ = assert(
  [`Lam ("x", `Bracket (`App (`Var "x", `Bracket (`App (`Var "y", `Var "z")))))]
  =
  run_parser_string parse_lambda "\\ x (x (y z))")

(**********************************************************************)
(* logic *)

(* individuals from the domain of quantification *)
type ind = [
  `Var of string
  | `Int of int]

(* propositions *)
type prop = [  
  `Equals of ind * ind]

(* 'b is the type of propositions e.g. including x=y *)
type 'b logic = [ 
    `Forall of string * 'a
  | `Exists of string * 'a
  | `Bracket of 'a
  | `Conj of 'a * 'a
  | `Disj of 'a * 'a
  | `Implies of 'a * 'a
  | `Not of 'a 
  | `Prop of 'b] 
  constraint 'a = 'b logic

let ind : (string,ind) parser3 identified = 
  mkntparser_lazy (mk_pre_parser()) (lazy(alts[
      (rhs(parse_RE "[a-z]+")) >> (fun x -> x|>c|>(fun x -> `Var x));
      (rhs(parse_RE "[0-9]+")) >> (fun x -> x|>c|>int_of_string|>(fun x -> `Int x))
    ]))

let prop : (string,prop) parser3 identified = 
  let w = parse_RE "[ ]*" in (* whitespace *)
  let ( >- ) p q = (p >- w >- q) >> (fun ((x,_),y) -> (x,y)) in (* parsers separated by whitespace *)
  mkntparser_lazy (mk_pre_parser()) (lazy(alts[
      ((rhs ind) >- (a "=") >- ind) >> (fun ((l,_),r) -> `Equals(l,r))
    ]))

(* parameterized parsing - the parameter is used to parse domain expressions *)
let parse_logic : (string,prop) parser3 identified -> (string,prop logic) parser3 identified = fun h0 ->
  let p = ref (mk_pre_parser()) in
  let w = parse_RE "[ ]*" in (* whitespace *)
  let v = parse_RE "[a-z]+" in (* variables *)
  let ( >- ) p q = (p >- w >- q) >> (fun ((x,_),y) -> (x,y)) in (* parsers separated by whitespace *)
  let alts = lazy(alts[
      ((rhs (a "!")) >- v >- !p)      >> (fun ((_,x),body) -> `Forall(c x,body));  (* ! x body *)
      ((rhs (a "?")) >- v >- !p)      >> (fun ((_,x),body) -> `Exists(c x,body));  (* ? x body *)
      ((rhs !p) >- (a "/\\") >- !p)   >> (fun ((x,_),y) -> `Conj(x,y));
      ((rhs !p) >- (a "\\/") >- !p)   >> (fun ((x,_),y) -> `Disj(x,y));
      ((rhs !p) >- (a "-->") >- !p)   >> (fun ((x,_),y) -> `Implies(x,y));
      ((rhs h0)                       >> (fun x -> `Prop x));
      ((rhs (a "(")) >- !p >- (a ")")) >> (fun ((_,body),_) -> `Bracket(body));
      ])
  in
  let _ = p := mkntparser_lazy (!p) alts in
  !p

let _ = 
  run_parser_string (parse_logic prop) "! x ! y (((1=2) /\\ (x=y)) --> (3=4))"


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

let _ = assert(["1234567891011"] = run_parser_string p "1234567891011")
let _ = assert([] = run_parser_string p "1234567891012")


(**********************************************************************)
(* indentation-sensitive parsing *)

(* whitespace is as before, except that any newline must be followed by exactly n spaces *)
let re n = "[ ]*\\(\n"^(String.make n ' ')^"\\)?"

(*
let txt = "  \n  "
let reg = re 3
let _ = 
  let b = Str.string_match (Str.regexp reg) txt 0 in
  try Some(Str.matched_string txt) with _ -> None
*)

let w n = parse_RE (re n)


let _L : (int -> (string,lambda) parser3 identified) ref= ref(fun _ -> failwith "ljx")

let _ = _L := fun n  -> 
  let p = ref (mk_pre_parser()) in
  let v = parse_RE "[a-z]" in (* variables *)
  let alts = lazy(alts[
      ((rhs (a "\\")) >- (w n) >- v >- (w n) >- !p)
        >> (fun ((((_,_),x),_),body) -> `Lam(c x,body));  (* \\ x body *)
      ((rhs !p) >- (w (n+1)) >- (!_L (n+1)))
        >> (fun ((x,_),y) -> `App(x,y));  (* p q - application *)
      (rhs v)
        >> (fun x -> `Var (c x));  (* x - variable *)
      ((rhs (a "(")) >- (w n) >- !p >- (w n) >- (a ")")) 
        >> (fun ((((_,_),body),_),_) -> `Bracket(body))  (* ( body ) *)
    ])
  in
  let _ = p := mkntparser_lazy (!p) alts in
  !p

let _ = 
  let tbl = Hashtbl.create 100 in
  _L := memo tbl !_L

let p = !_L 0

(* note two possible parses *)
let _ = assert(
  set_equal
    [`App (`App (`Var "f", `Var "x"), `Var "y");
     `App (`Var "f", `App (`Var "x", `Var "y"))]
    (run_parser_string p "f x y"))

(* one parse, because of indentation *)
let _ = assert(
[`App (`App (`Var "f", `Var "x"), `Var "y")] =  
run_parser_string p "f 
 x
 y")

(* again, one parse, disambiguated via indentation; here the y must be
   part of an application with x as first component *)
let _ = assert(
  [`App (`Var "f", `App (`Var "x", `Var "y"))] =
run_parser_string p "f
 x
  y")

(* here y and z are part of an application with x as the first component *)
let _ = assert(
  [`App (`Var "f", `App (`App (`Var "x", `Var "y"), `Var "z"))] = 
run_parser_string p "f
 x
  y
  z")

(* now z is part of an application with y as first component *)
let _ = assert(
  [`App (`Var "f", `App (`Var "x", `App (`Var "y", `Var "z")))] = 
run_parser_string p "f
 x
  y
   z")

let _ = assert(
[`App
   (`App
      (`App (`Var "f", `App (`Var "x", `App (`Var "y", `Var "z"))),
       `App (`App (`Var "a", `Var "b"), `Var "c")),
    `Var "d")] = 
run_parser_string p "f
 x
  y
   z
 a
  b
  c
 d")

(* a python-esque example *)

let re n = "[ ]*\\(\n"^(String.make n ' ')^"\\)?"
let w n = parse_RE (re n)

let _if = a "if"
let _then = a "then"
let _else = a "else"
let _atomic = a "atomic_statement"
let _bexp = a "bexp"

type statement = [  
  | `Atomic
  | `Seq of 'a list
  | `If_then_else of ( string * 'a * 'a)
  | `If_then of ( string * 'a)]
  constraint 'a = statement

(* _STMT is a (non-sequence) statement; _STMTS is a sequence of statements *)
let _STMT : (int -> (string,statement) parser3 identified) ref = ref(fun _ -> failwith "ljx")
let _STMTS : (int -> (string,statement) parser3 identified) ref = ref(fun _ -> failwith "ljx")

let _ = _STMT := fun n  -> 
  let p = ref (mk_pre_parser()) in
  let alts = lazy(alts[
      (rhs _atomic) >> (fun _ -> `Atomic);
      ((rhs _if) >- (w (n+1)) >- _bexp >- (w n) >- 
       _then >- (w (n+1)) >- 
         (!_STMTS (n+1)) >- (w n) >-
       _else >- (w (n+1)) >- 
         (!_STMTS (n+1)))
      >> (fun (((((((((((_,_),b),_),_),_),ss1),_),_),_),ss2)) -> `If_then_else (c b,ss1,ss2));
      ((rhs _if) >- (w (n+1)) >- _bexp >- (w n) >- 
       _then >- (w (n+1)) >- 
         (!_STMTS (n+1)))
      >> (fun ((((((_,_),b),_),_),_),ss1) -> `If_then (c b,ss1))
    ])
  in
  let _ = p := mkntparser_lazy (!p) alts in
  !p

let _ = 
  let tbl = Hashtbl.create 100 in
  _STMT := memo tbl !_STMT

let _ = _STMTS := fun n  -> 
  let p = ref (mk_pre_parser()) in
  let alts = lazy(alts[
      (rhs (sepby1 (!_STMT n) (w n)))
        >> (fun xs -> `Seq(xs));
    ])
  in
  let _ = p := mkntparser_lazy (!p) alts in
  !p

let _ = 
  let tbl = Hashtbl.create 100 in
  _STMTS := memo tbl !_STMTS


let p = !_STMTS 0

(* simple example: an atomic statement followed by an if-then-else *)
let _ = assert(
[`Seq [`Atomic; `If_then_else ("bexp", `Seq [`Atomic], `Seq [`Atomic])]] =
run_parser_string p "atomic_statement 
if bexp then
 atomic_statement
else
 atomic_statement")

(* using indentation to disambiguate: the else belongs to the outer
   if-then-else *)
let _ = assert(
[`Seq
   [`Atomic;
    `If_then_else
      ("bexp", `Seq [`If_then ("bexp", `Seq [`Atomic])], `Seq [`Atomic])]] =
run_parser_string p "atomic_statement 
if bexp then
 if bexp then
  atomic_statement
else
 atomic_statement")

(* three statements in a sequence *)
let _ = assert(
[`Seq
   [`Atomic;
    `If_then_else
      ("bexp", `Seq [`If_then ("bexp", `Seq [`Atomic])], `Seq [`Atomic]);
    `Atomic]] =
run_parser_string p "atomic_statement 
if bexp then
 if bexp then
  atomic_statement
else
 atomic_statement
atomic_statement")

(* two statements; here the indentation of final atomic_statement
   means that it is attached to the then clause of the first if-then *)
let _ = assert(
[`Seq
   [`Atomic;
    `If_then
      ("bexp",
       `Seq [`If_then_else ("bexp", `Seq [`Atomic], `Seq [`Atomic]); `Atomic])]] =
run_parser_string p "atomic_statement 
if bexp then
 if bexp then
  atomic_statement
 else
  atomic_statement
 atomic_statement")
