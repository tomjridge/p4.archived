(**
{1 P4_core: core P4 definitions}

{2 Prelude}
*)

(*
module Std_map_int = Map.Make(struct type t = int let compare : int -> int -> int = compare end)

module NT_map = Map.Make(struct type t = int let compare : int -> int -> int = compare end)

module TM_map = Map.Make(struct type t = int let compare : int -> int -> int = compare end)
*)

let exn_option f x = try Some(f x) with _ -> None

(* module Std_set_int = Set.Make(struct type t = int let compare : int -> int -> int = compare end) *)

module Gensym = struct

  let counter = ref 0

  let gen_int () = (
    let c = !counter in
    let _ = counter := c+1 in 
    c)

  let gen_even () = (
    let c = !counter in
    let c = (if c mod 2=0 then c else c+1) in
    let _ = counter := c+1 in
    c)

  let gen_odd () = (
    let c = !counter in
    let c = (if c mod 2=1 then c else c+1) in
    let _ = counter := c+1 in
    c)

end

module Box = struct

  open Gensym

  type 'a box = [ `Box of int * 'a ] (* FIXME change `Box to Box *)

  let box x = (`Box(gen_int(),x))

  let box_even x = (`Box(gen_even(),x))

  let box_odd x = (`Box(gen_odd(),x))

  let unbox x = (match x with
    | `Box((c:int),x) -> x)

  let box_get_key x = (match x with
    | `Box((c:int),x) -> c)

end

open Box

let rec itlist f l b =
  match l with
    [] -> b
  | (h::t) -> f h (itlist f t b)

let rec allpairs f l1 l2 =
  match l1 with
   h1::t1 ->  itlist (fun x a -> f h1 x :: a) l2 (allpairs f t1 l2)
   | [] -> []

let list_product l1 l2 = allpairs (fun x -> fun y -> (x,y)) l1 l2

let rec myinsert cmp elt lst = match lst with
  [] -> [elt]
| head :: tail -> let r = cmp elt head in if r < 0  then elt :: lst else (
  if r = 0 then failwith "myinsert" else head :: myinsert cmp elt tail)

let unique_f res e = if List.mem e res then res else e::res

(* this is insertion sort; alternatives? *)
let unique = fun e -> List.fold_left unique_f [] e

(* upto' i j = [i+1..j-1] *)
let rec upto' i j = (if i+1<j then (i+1)::(upto' (i+1) j) else [])

(* upto i j = [i..j] *)
let rec upto i j = (if i<=j then i::(upto (i+1) j) else [])


(* version of map that avoids some stack overflows with List.map *)
let rev_map f l = List.rev_map


(**
{2 Types}
*)

(* invariant: SS(s,i,j): i<=j<=(String.length s) *)
type 'a ty_span = [ `SS of 'a * int * int ]
type ty_span' = string ty_span

let dest_SS (`SS(s,i,j)) = (s,i,j)

let content (`SS(s,i,j)) = String.sub s i (j-i)

let concatenate_two (`SS(s1,i1,j1)) (`SS(s2,i2,j2)) = (
  if (s1=s2) && (j1=i2) then
    Some (`SS(s1,i1,j2))
  else
    None)

let rec concatenate_list ss = (match ss with
|  [] -> None
| s1::ss -> (match ss with
  | [] -> Some s1
  | _ -> (match concatenate_list ss with
      None -> None
  |   Some s2 -> concatenate_two s1 s2)))

(* FIXME now that we have a separate indexing mechanism, we don't need to assume nts and tms are separate; in fact, we don't need these types at all *)
type term = int (* odd *)

type nonterm = int (* even *)

type symbol = [ `NT of nonterm | `TM of term ]
let mk_NT () = (Gensym.gen_even ())
let mk_TM () = (Gensym.gen_odd ())

let dest_NT sym = (match sym with `NT x -> x | _ -> failwith "dest_NT")

let string_of_tm (tm:term) = string_of_int tm
let string_of_nt (nt:nonterm) = string_of_int nt

(* we assume nonterminals and terminals correspond to disjoint subsets of string *)
let string_of_symbol sym = (match sym with 
  | `NT nt -> (string_of_nt nt)
  | `TM tm -> (string_of_tm tm))

let int_of_symbol sym = (match sym with
  | `NT i -> i | `TM i -> i)

type 'a raw_parser = 'a ty_span -> int list
type ('string,'a) raw_act = 'string ty_span -> 'a list


(* invariant: LC(lc): forall (_,s1) (_,s2) in lc. s1=s2 *)
(* FIXME nonterm_int rather than nonterm *)
type 'string local_context = LC of (nonterm * 'string ty_span) list 
type local_context' = string local_context
let empty_context = (LC [])



type 'string tm_plus = {
  tp_tm: term;
  tp_raw_parser: 'string raw_parser }

type 'string ty_rhs_syms_list = (int * 'string sym_plus) list

(*
and 'string ty_rule = 'string ty_rhs_syms_list
*)


and 'string nt_plus = {
  np_nt: nonterm;
  np_rhss: unit -> 'string ty_rhs_syms_list list; }

and 'string sym_plus = 
  | NP of 'string nt_plus 
  | TP of 'string tm_plus

let dest_NP (NP x) = x

let dest_TP (TP x) = x

type 'string ty_oracle = ('string ty_rhs_syms_list * 'string sym_plus) -> (int * int) -> int list
(* let empty_oracle = (fun (sym1,sym2) -> fun ss -> []) *)

(* we also cache the results of terminal parsing; note that the first argument is a term, not a string! *)
type 'string ty_tmoracle = 'string tm_plus -> int * int -> bool

type 'string inr = {
  ss4: 'string ty_span;
  box4: 'string box; (* the underlying value is a boxed version of the value in .ss4 *) 
  lc4: int local_context; (* contexts work with box keys, not 'string *)
  oracle4: 'string ty_oracle;
  tmoracle4: 'string ty_tmoracle;
}
type inr' = string inr
type 'a outr = 'a list


(* a clause (a ref) *)
type ('string,'a) pre_clause = { 
  sp: 'string sym_plus;
  act:'string inr -> 'a outr }
type ('a,'b) parser3 = ('a,'b) pre_clause

type ('string,'a) clause = ('string,'a) pre_clause ref


type ('string,'a) rhs = {
  rhs_syms:unit -> 'string ty_rhs_syms_list; 
  rhs_act:'string inr -> 'a outr }

type ('string,'a) alts = { 
  alts_rhss:unit -> ('string ty_rhs_syms_list) list; 
  alts_act: 'string inr -> 'a outr }

let rhs cl0 = 
  let i = Gensym.gen_int() in
  {
    rhs_syms=(fun () -> [i,(cl0).sp]);
    rhs_act=cl0.act }
let (_:('a,'b)parser3 -> ('a,'b) rhs) = rhs


let seq rhs cl0 =
  let i = Gensym.gen_int() in
  let rhs_syms = lazy (rhs.rhs_syms ()) in
  let rev_rhs_syms = lazy (List.rev (Lazy.force rhs_syms)) in (* FIXME note ugliness with reversing the order of the syms when looking up in the oracle; so rhs_syms should be returned in reverse order perhaps? *)
  {
    rhs_syms=(fun () -> (Lazy.force rhs_syms)@[i,cl0.sp]);
    rhs_act=(fun i0 -> 
      let `SS(s,i,j) = i0.ss4 in
      let ks = i0.oracle4 (Lazy.force rev_rhs_syms,cl0.sp) (i,j) in
      let f1 k = (
        let rs1 = rhs.rhs_act { i0 with ss4=(`SS(s,i,k)) } in
        let rs2 = (cl0).act { i0 with ss4=(`SS(s,k,j)) } in
        list_product rs1 rs2)
      in      
      List.concat (List.map f1 ks)) }

let (_:('a,'b)rhs -> ('a,'c)parser3 -> ('a,'b*'c)rhs) = seq

(*
  let syms = rhs.rhs_syms in
  let sym = !cl0.sym in
  { rhs_syms=sym::syms;  (* N.B. reversed *)
    rhs_syms=(fun x -> x |> rhs.rhs_syms |> (!cl0.rules));
    rhs_act=(fun i0 -> 
      let `SS(s,i,j) = i0.ss4 in
      let ks = i0.oracle4 (syms,sym) (i,j) in
      let f1 k = (
        let rs1 = rhs.rhs_act { i0 with ss4=(`SS(s,i,k)) } in
        let rs2 = (!cl0).act { i0 with ss4=(`SS(s,k,j)) } in
        list_product rs1 rs2)
      in
      List.concat (List.map f1 ks)) }
*)

let ( >- ) = seq


let rhs_seq = (fun x y -> (rhs x) >- y)
let (_:('a,'b)parser3 -> ('a,'c) parser3 -> ('a,'b*'c)rhs) = rhs_seq

let ( >-- ) = rhs_seq


let set_act_rhs = fun rhs act -> {rhs with rhs_act=fun x -> x |> rhs.rhs_act |> (List.map act) }
let (_:('a,'b)rhs -> ('b->'c) -> ('a,'c)rhs) = set_act_rhs
let ( >> ) = set_act_rhs

let set_act = fun p act -> { p with act=fun x -> x |> p.act |> (List.map act) }
let (_:('a,'b)parser3 -> ('b -> 'c) -> ('a,'c)parser3) = set_act
let ( >>> ) = set_act

let rec alts rhss = (match rhss with
  | [] -> failwith "alts: []" 
(* { alts_rhss=(fun () -> failwith "alts: impossible"); alts_act=(fun x -> failwith "alts: impossible")} *)
  | rhs::[] -> { 
    alts_rhss=(fun () -> [rhs.rhs_syms ()]); 
    alts_act=rhs.rhs_act }
  | rhs::rest -> (
    let rest = alts rest in
    { alts_rhss=(fun () -> (rhs.rhs_syms ()) :: (rest.alts_rhss ()));
      alts_act=(fun i -> unique((rhs.rhs_act i) @ (rest.alts_act i))) }))

let (_:('a,'b)rhs list -> ('a,'b) alts) = alts


let mkntparser' p alts = (match p.sp with
  | TP _ -> (failwith "mkntparser': TP")
  | NP np -> (
    let z = lazy (alts ()) in
    let p = {
      sp=NP { np with
        np_rhss=fun () -> (Lazy.force z).alts_rhss () };
      act=(fun i0 -> i0 |> (Lazy.force z).alts_act) }
    in
    p))

let (_:('a, 'b) pre_clause -> (unit -> ('c, 'd) alts) -> ('c, 'd) pre_clause) = mkntparser'




let mkclause sym = (match sym with 
  | `NT nt -> (
    {
      sp=NP {
        np_nt=nt;
        np_rhss=(fun () -> failwith ((string_of_symbol sym)^" np_rhss")); };
      act=(fun _ -> failwith ((string_of_symbol sym)^" act"))})
  | `TM tm -> (
    {
      sp=TP {
        tp_tm=tm;
        tp_raw_parser=(fun _ -> failwith ((string_of_symbol sym)^" tp_raw_parser")) };
      act=(fun _ -> failwith ((string_of_symbol sym)^" act"))}))

let mk_pre_parser () = mkclause (`NT (mk_NT()))
let (_:unit -> ('a,'b)parser3) = mk_pre_parser


let mk_nt nt = (mkclause (`NT nt))



(* FIXME we don't expect .sp.sp_rules to be called on a terminal *)
(* FIXME what to do with rp? FIXME there are really two different types of things*)
let mk_tm tm rp act = 
  let cl = mkclause (`TM tm) in
  let sp = (match cl.sp with
    | NP _ -> (failwith "mk_tm: NP")
    | TP tp -> TP {tp with tp_raw_parser=rp }) in 
  let cl = { sp=sp; act=act} in
  cl

(* FIXME don't need this and mk_tm? FIXME following is horrible *)
let mktmparser rp ract = 
  let cl = mk_tm (mk_TM()) rp (fun x -> failwith "mktmparser") in
  let tp = (match cl.sp with | TP tp -> tp | _ -> failwith "mktmparser: tp") in
  let f1 = (fun i0 -> 
    let `SS(s,i,j) = i0.ss4 in
    if i0.tmoracle4 tp (i,j) then
      ract (`SS(s,i,j))
    else
      [])
  in
  {cl with act=f1}

let (_:'a raw_parser -> ('a,'b)raw_act -> ('a,'b) parser3) = mktmparser

(**********************************************************************)
(* context *)

(**
{2 Context definitions}
*)


(* memoization works best if contexts are represented using sorted lists *)
(*
let lc_cmp (nt1,`SS(s1,i1,j1)) (nt2,`SS(s2,i2,j2)) = (
  Pervasives.compare (nt1,(i1,j1)) (nt2,(i2,j2)))
*)

let lc_cmp (nt1,ss1) (nt2,ss2) = (
  Pervasives.compare (nt1,ss1) (nt2,ss2))

(* this version is equivalent to the previous if we use normalized contexts *)
(*
let lc_cmp (nt1,ss1) (nt2,ss2) = (
  if ss1 <> ss2 then failwith "lc_cmp: impossible" 
  else Pervasives.compare nt1 nt2)
*)

(* when parsing the input between l and h, the only part of the
 context that matters are those entries (nt,(l',h')) st (l',h') =
 (l,h); so there is a notion of a normalized context (important
 for memoization) *)

(* memoization works best if contexts are normalized *)
(*
let normalize_context (LC(lc)) (`SS(s,i,j)) = (
  LC(List.filter (fun (nt,`SS(s',i',j')) -> (i',j')=(i,j)) lc))
*)

let normalize_context i0 (LC(lc)) (`SS(s,l,h)) = (
  let `SS(s,l,h) = `SS(box_get_key i0.box4,l,h) in
  LC(List.filter (fun (nt,ss') -> ss'=`SS(s,l,h)) lc))

(* for contexts, we work with substrings whose values are ints *)
let update_context i0 c (nt,`SS(s,l,h)) = (
  let LC(lc) = normalize_context i0 c (`SS(s,l,h)) in
  let `SS(s,l,h) = `SS(box_get_key i0.box4,l,h) in
  LC(myinsert lc_cmp (nt,`SS(s,l,h)) lc))

(* simpler definition; don't need lc_cmp, normalize_context and previous update_context

let update_context (LC(lc)) (nt,`SS(s,l,h)) = (
  LC((nt,`SS(s,l,h))::lc))

*)

(*
let context_contains (LC(lc)) (nt,`SS(s,l,h)) = (
  List.exists (fun (nt',`SS(_,i',j')) -> (nt,i',j') = (nt,l,h)) lc)
*)

let context_contains i0 (LC(lc)) (nt,`SS(s,l,h)) = (
  let `SS(s,l,h) = `SS(box_get_key i0.box4,l,h) in
  List.exists (fun (nt',ss') -> (nt',ss') = (nt,`SS(s,l,h))) lc)

let update_lc4 nt h i0 = (h { i0 with lc4=(update_context i0 i0.lc4 (nt,i0.ss4)) })


let check_and_upd_lc4 p = (match p.sp with 
  | TP _ -> p
  | NP np -> (
    let h = p.act in
    let h' i = (
      let nt = np.np_nt in
      let should_trim = context_contains i i.lc4 (nt,i.ss4) in
      if should_trim then 
        []
      else
        update_lc4 nt h i)
    in
    {p with act=h'}))

let mkntparser p alts = (
  check_and_upd_lc4 (mkntparser' p alts))

(* convenience method *)
let mkntparser_lazy : ('a, 'b) pre_clause -> ('c, 'd) alts Lazy.t -> ('c, 'd) pre_clause = 
  fun p lazy_alts -> mkntparser p (fun () -> Lazy.force lazy_alts)


module P4_memo = struct

  type hashkey = int local_context * int ty_span
  type 'a hashvalue = 'a outr

  let memo tbl key_of_input f i = (
    let k = key_of_input i in
    match k with 
    | None -> (f i)
    | Some k -> (
      if (Hashtbl.mem tbl k) then 
        (Hashtbl.find tbl k) 
      else
        let v = f i in
        let _ = Hashtbl.add tbl k v in
        v))

  let key_of_input i = (
    let ss = i.ss4 in
    let lc4 = normalize_context i i.lc4 ss in
    let `SS(s,l,h) = ss in
    let k = (lc4,`SS(Box.box_get_key i.box4,l,h)) in (* about 10% slowdown if we include box key *)
    Some k)
  
  let memo_p tbl p = (
    let act' = memo tbl key_of_input p.act in
    {p with act=act'})

  let (_: (hashkey,'b hashvalue) Hashtbl.t -> ('a,'b)parser3 -> ('a,'b)parser3) = memo_p

  (* some additional functions used by the code generator - slight abbreviations of the above *)
  let memo_mkntparser tbl p alts = memo_p tbl (mkntparser p alts)

  let (_: (hashkey,'b hashvalue) Hashtbl.t -> ('a,'b)parser3 -> (unit -> ('a,'b)alts) -> ('a,'b)parser3) = memo_mkntparser

end

include P4_memo      




(**********************************************************************)
(* examples *)

(*

let raw_a1 = (fun x -> match x with `SS(s,i,j) -> if i < j && s.[i]='1' then [i+1] else [])

(* invariant: on creation, syms are set and are not subsequently altered *)
let _1 = mktmparser raw_a1 (fun ss -> if content ss = "1" then [1] else [])

let rec _E = 
  let nt = mk_nt 2 in
  let alts = lazy (alts [
      ((_E () >-- _E ()  >- _E ()) >> (fun ((z,x),y) -> z+x+y));
      rhs (_F ());
      rhs _1])
  in
  (fun () -> mkntparser nt (fun () -> Lazy.force alts))
and _F = 
  let nt = mk_nt 4 in
  let alts = lazy (alts [
      ((_E () >-- _E ()) >> (fun (x,y) -> x+y)) ])
  in
  (fun () -> 
    mkntparser nt (fun () -> Lazy.force alts))

let r = _E ()

let dest_NP (NP x) = x

let _ = r.sp |> dest_NP |> (fun x -> x.np_rhss ())

(* this looks like a reasonable interface; could specialize the combinators to include the () arguments everywhere *)



(* another example using laziness ; use of laziness twice is potentially confusing! *)
let rec _E = lazy (
  let nt = mk_nt 2 in
  let alts = lazy (
    let (_E,_F) = (Lazy.force _E, Lazy.force _F) in
    alts [
      ((_E >-- _E >- _E) >> (fun ((z,x),y) -> z+x+y));
      rhs _F;
      rhs _1])
  in
  mkntparser nt (fun () -> Lazy.force alts))
and _F = lazy (
  let nt = mk_nt 4 in
  let alts = lazy (
    let (_E,_F) = (Lazy.force _E, Lazy.force _F) in
    alts [
      ((_E >-- _E) >> (fun (x,y) -> x+y)) ])
  in
  mkntparser nt (fun () -> Lazy.force alts))

let r = Lazy.force _E 

let _ = r.sp |> dest_NP |> (fun x -> x.np_rhss ())

(* and again we could redefine the combinators; we still have the horrible mkntparser *)








let get_rules cl = (match cl.sp with
  | TP _ -> (failwith "get_rules: TP")
  | NP np -> (np.np_rhss ()))



let mkntparser_ref r alts = 
  let _ = r := mkntparser (!r) alts in
  ()

let _E = ref (mkclause (`NT 2))

let _F = ref (mkclause (`NT 4))

(* the example using refs makes it clear when the ids are generated ;
   this won't work for lazily generated nts of course *)
(* FIXME this doesn't look right - how do the actions get bound correctly? *)
let _ = mkntparser_ref _E (
  let alts = alts [
    ((!_E >-- !_E >- !_E) >> (fun ((z,x),y) -> z+x+y));
    rhs !_F;
    rhs _1]
  in
  (fun () -> alts))

let _ = mkntparser_ref _F (
  let alts = alts [
    (!_E >-- !_E)>>(fun (x,y) -> x+y)]
  in
  (fun () -> alts))

let _ = get_rules !_E

let _ = get_rules !_F

 
(* example going beyond CFGs - infinitely many nonterminals *)
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

let _G = ref(fun n -> failwith "")

let _ = _G := fun n -> 
  let cl = (mkclause (`NT (mk_NT()))) in
  let cl = mkntparser cl (
    let alts = lazy(alts([
    (rhs _1);
    ((_1 >-- (!_G (n+1))) >> (fun (x,y) -> x+y))]
    @ (if n=0 then [] else [rhs (!_G (n-1)) ])))
    in
    fun () -> Lazy.force alts)
  in
  cl

let _ = _G := memo !_G

let _ = !_G 0
let _ = !_G 0

let _ = !_G 1
let _ = !_G 1


let _ = get_rules (!_G 0)
let NP p = match (get_rules (!_G 0)) with [_;[_;(_,nt)]] -> nt
let _ = p.np_rhss ()
(* yes! this is what we want! except that the evaluation of nts gets
   new ones all the time; FIXED by making _G a reference, and memoizing
   it after declaration *)

(* interface to earley *)

let rec _E = lazy (1 + Lazy.force _F) 
and _F = lazy 2

let _ = Lazy.force _E



*)
