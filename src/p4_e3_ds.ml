(* a standard interface to e3_core *)


(**********************************************************************)
(* P4_core dependencies *)

(* P4_core type dependencies *)
module S = struct 
  open P4_core

  type 'a tm = 'a tm_plus
  type 'a nt = 'a nt_plus
  type 'a sym = 'a sym_plus
  type 'a sym_list = 'a ty_rhs_syms_list
  type 'a sym_item = 'a sym_plus * int * int
  type 'a tm_item = 'a tm_plus * int
  type 'a nt_item = 'a nt_plus * 'a sym_list * 'a sym_list * int * int
  type 'a item = [ `NTITM of 'a nt_item | `TMITM of 'a tm_item ]
(*  type 'a p_of_tm = ('a tm -> 'a substring -> int list)*)

  let int_of_sp sp = (match sp with
    | NP np -> np.np_nt
    | TP tp -> tp.tp_tm)
  
  let sym_case x = (match x with | NP np -> `NT np | TP tp -> `TM tp)
  
  let np_rhss x = x.np_rhss
  
  let np_nt x = x.np_nt

end



(* we expect to be able to convert p3 types to ints *)
type ('a,'b) sum = Inl of 'a | Inr of 'b

type 'a index = {
  nt_int: 'a S.nt -> int;
  tm_int: 'a S.tm -> int;
  sym_int: 'a S.sym -> int;
  sym_item_int: 'a S.sym_item -> int * int * int;
  sym_list_int: 'a S.sym_list -> int;
  nt_item_int : 'a S.nt_item -> int * int * int;
  item_int : 'a S.item -> (int * int * int,int * int) sum  (* FIXME inefficient for TM_TIM - don't need 3rd component *)
}


let sym_list_int = (fun syms -> match syms with
  | [] -> 0 (* FIXME hack - is this ok? (failwith "syms_list_int: impossible ?") *)
  | (i,_)::_ -> i)

let nt_item_int = (fun ((nt,syms1,syms2,i,j):'a S.nt_item) -> 
  match syms2 with 
  | [] -> (S.np_nt nt,i,j) (* FIXME hack - hopefully we never store nts with nt_items *)
  | _ -> (sym_list_int syms2,i,j))

let index = 
  let open P4_core in
  {
    nt_int = (fun (np:'a S.nt) -> np.np_nt);
    tm_int = (fun (tp:'a S.tm) -> tp.tp_tm);
    sym_int = (fun (sp: 'a S.sym) -> S.int_of_sp sp);
    sym_list_int = sym_list_int;
    sym_item_int = (fun (sp,i,j) -> (S.int_of_sp sp,i,j));
    nt_item_int = nt_item_int;
    item_int = (fun x -> match x with 
    | `NTITM ntitm -> Inl(nt_item_int ntitm)
    | `TMITM ((tp:'a S.tm),i) -> Inr(S.int_of_sp (TP tp),i)) (* FIXME doesn't need third component *)
  }



(************************************************************************)
(* the interface for this module *)


type 'a std_setup = {
  std_sym    : 'a S.sym;
  std_string : 'a; (* int arg is length of 'a; assumed to start from 0 *)
  std_length : int;
}

(* FIXME for ty_oracle we may want to identify syms with ints, on the basis that this make memo efficient *)
type 'a ty_oracle = ('a S.sym * 'a S.sym) -> (int*int) -> int list
type 'a ty_tmoracle = 'a S.tm -> int * int -> bool 
type 'a ty_result = <
  oracle: 'a ty_oracle;
  tmoracle: 'a ty_tmoracle
>



(**********************************************************************)
(* the implementation of this module *)

open E3_core


let nt_items_for_nt (np:'a S.nt) (_,_,i) = 
  let rhss = (S.np_rhss np) () in
  ((List.map (fun r -> (np,[],r,i,i)) rhss): 'a S.nt_item list)

let (_: 'a S.nt -> ('a*int*int) -> 'a S.nt_item list) = nt_items_for_nt

(* if we omit the dummy argument (_:'string), then later we appear to be unable to use mkops nt_items_for_nt polymorphically; an alternative might be to define these after nt_items_for_nt *)

let mk_ops (_:'string) = (
  let id = fun x -> x in
  let open P4_core in
  {
    sym_case       =(S.sym_case:('string S.sym -> [ `NT of 'string S.nt | `TM of 'string S.tm]));
    sym_of_tm      =(fun (tp:'string S.tm) -> TP tp);
    mk_tm_coord    =(fun (tp,i) -> (tp,i));
    tm5            =(fun (tp,i) -> tp);
    mk_sym_coord   =(fun (s,i,j) -> (s,i,j));
    sym6           =(fun (sym,i,j) -> sym);
    nt2            =(fun (nt,_,_,_,_) -> NP nt);
    shift_a2_b2_c2 =(fun (nt,_as,b::bs,i,j) -> (nt,b::_as,bs,i,j));
(*    a2_length_1    =(fun (nt,_as,_,_,_) -> match _as with [x] -> true | _ -> false); *)
    b2_nil         =(fun (nt,_,bs,_,_) -> match bs with [] -> true | _ -> false);
(*    hd_a2          =(fun (_,(_,a)::_,_,_,_) -> a); *)
    a2             =(fun (_,_as,_,_,_) -> _as);
    hd_b2          =(fun (_,_,(_,b)::_,_,_) -> b);
    nt_items_for_nt=nt_items_for_nt;
    mk_item        =id;
    dest_item      =(fun (x: 'string S.item) -> x);
    tm_dot_i9      =(fun (tm,i) -> i);
    sym_dot_i9     =(fun (sym,i,j) -> i);
    sym_dot_j9     =(fun (sym,i,j) -> j);
    nt_dot_i9      =(fun (nt,_,_,i,j) -> i);
    nt_dot_j9      =(fun (nt,_,_,i,j) -> j);
    with_j9        =(fun (nt,_as,bs,i,_) -> fun j -> (nt,_as,bs,i,j));
    p_of_tm        =(fun (tm:'a S.tm) -> fun ((s,i,j):'a substring) -> tm.tp_raw_parser (`SS(s,i,j)));
  })

let _ = mk_ops "" (* FIXME problematic because the types are not general *)


let compare_i x1 y1 = (x1 - y1)

let compare_ii (x1,x2) (y1,y2) = (
  let x = x1 - y1 in
  if x<>0 then x else
    x2-y2)
  
let compare_iii (x1,x2,x3) (y1,y2,y3) = (
  let x = x1 - y1 in
  if x<>0 then x else
    let x=x2 - y2 in
    if x<>0 then x else
      x3 - y3)

let compare_iiii (x1,x2,x3,x4) (y1,y2,y3,y4) = (
  let x = x1 - y1 in
  if x<>0 then x else
    let x=x2 - y2 in
    if x<>0 then x else
      let x=x3 - y3 in
      if x<>0 then x else
        x4 - y4)

(*
let compare_nt_item i1 i2 = (
  let x = compare_iii (index.item_int ((nt,i,j) (nt',i',j') in
  if x<>0 then x else
    Pervasives.compare (_as,bs) (_as',bs'))
*)

(*
let compare_item i1 i2 = (
  compare_iii (index.item_int i1) (index.item_int i2))  
*)

let compare_item i1 i2 = (
  let (i1,i2) = (index.item_int i1, index.item_int i2) in
  match (i1,i2) with
  | Inl _, Inr _ -> -1
  | Inr _, Inl _ -> 1
  | Inl x, Inl y -> compare_iii x y
  | Inr x, Inr y -> compare_ii x y)

module Sets_maps = (struct

  let max_array_size = 1000

  (* implementation as a hashtbl to unit *)
  let set_todo_done n = {
    std_empty=(fun () -> Hashtbl.create (if n < max_array_size then n else max_array_size));
    std_add=(fun k t -> let k = index.item_int k in Hashtbl.replace t k (); t);
    std_mem=(fun k t -> let k = index.item_int k in Hashtbl.mem t k);
  }

  let sets n = { set_todo_done=(set_todo_done n) }
 
  (* implement cod as a hashtbl *)
  let map_blocked_key n = {
    mbk_empty=(fun () -> Hashtbl.create (if n < max_array_size then n else max_array_size));
    mbk_add_cod=(fun (k:int*'a S.sym) (v:'a S.nt_item) t -> 
      let (k1,k2) = k in
      let k = (k1,index.sym_int k2) in
      let h = 
        try Hashtbl.find t k with | Not_found -> 
          let h = Hashtbl.create (if n < max_array_size then n else max_array_size) in
          let _ = Hashtbl.replace t k h in
          h
      in
      let _ = Hashtbl.replace h (index.nt_item_int v) v in
      t);
    mbk_fold_cod=(fun k f t acc -> 
      try 
      let (k1,k2) = k in
      let k = (k1,index.sym_int k2) in
        let h = Hashtbl.find t k in
        Hashtbl.fold (fun k v acc -> f v acc) h acc
      with | Not_found -> acc)
  }

  let map_complete_key n = {
    mck_empty=(fun () -> Hashtbl.create (if n < max_array_size then n else max_array_size));
    mck_add_cod=(fun (k:int * 'a S.sym) (v: 'a S.sym_item) t -> 
      let (k1,k2) = k in
      let k = (k1,index.sym_int k2) in
      let h = 
        try Hashtbl.find t k with | Not_found -> 
          let h = Hashtbl.create (if n < max_array_size then n else max_array_size) in
          let _ = Hashtbl.replace t k h in
          h
      in
      let _ = Hashtbl.replace h (index.sym_item_int v) v in
      t);
    mck_fold_cod=(fun k f t acc -> 
      let (k1,k2) = k in
      let k = (k1,index.sym_int k2) in
      try 
        let h = Hashtbl.find t k in
        Hashtbl.fold (fun k v acc -> f v acc) h acc
      with | Not_found -> acc)
  }

  let map_tm_int n = {
    mti_empty=(fun () -> Hashtbl.create (if n < max_array_size then n else max_array_size));
    mti_add_cod=(fun (k: 'a S.tm * int) (v:int) t -> 
      let (k1,k2) = k in
      let k = (index.tm_int k1,k2) in
      let h = 
        try Hashtbl.find t k with | Not_found -> 
          let h = Hashtbl.create (if n < max_array_size then n else max_array_size) in
          let _ = Hashtbl.replace t k h in
          h
      in
      let _ = Hashtbl.replace h v () in
      t);
    mti_find_cod=(fun k v t -> 
      let (k1,k2) = k in
      let k = (index.tm_int k1,k2) in
      try 
        let h = Hashtbl.find t k in
        Hashtbl.mem h v
      with | Not_found -> false)
  }

  (* FIXM Eusing an int list is terribly inefficient - we should be able to map a list of sym to an int using the enclosing np *)
  let map_sym_sym_int_int n = {
    mssii_empty=(fun () -> Hashtbl.create (if n < max_array_size then n else max_array_size));
    mssii_add_cod=(fun (k:'a S.sym_list * 'a S.sym * int * int) (v:int) t -> 
      let (k1,k2,k3,k4) = k in
      let k = (index.sym_list_int k1, index.sym_int k2,k3,k4) in 
      let h = 
        try Hashtbl.find t k with | Not_found -> 
          let h = Hashtbl.create (if n < max_array_size then n else max_array_size) in
          let _ = Hashtbl.replace t k h in
          h
      in
      let _ = Hashtbl.replace h v () in
      t);
    mssii_elts_cod=(fun k t -> 
      let (k1,k2,k3,k4) = k in
      let k = (index.sym_list_int k1, index.sym_int k2,k3,k4) in 
      try 
        let h = Hashtbl.find t k in
        let f1 k' v' acc = k'::acc in
        Hashtbl.fold f1 h []
      with | Not_found -> [])
  }

  let maps n = {
    map_blocked_key=(map_blocked_key n); 
    map_complete_key=(map_complete_key n); 
    map_sym_sym_int_int=(map_sym_sym_int_int n); 
    map_tm_int=(map_tm_int n); 
  }

end)


(* maps that do nothing - useful for benchmarking oracle creation *)
module Dummy = struct

  let map_sym_sym_int_int = {
    mssii_empty=(fun () -> ());
    mssii_add_cod=(fun k v t -> t);
    mssii_elts_cod=(fun k t -> []); 
  }

  let maps n = (
    let m = Sets_maps.maps n in
    {m with map_sym_sym_int_int=map_sym_sym_int_int })

end


let mk_init_loop2 ctxt init_items = (
  let sets = ctxt.sets in
  let maps = ctxt.maps in
  let s0 = {
    todo_done5=(
      let f1 = (fun s itm -> sets.set_todo_done.std_add itm s) in
      List.fold_left f1 (sets.set_todo_done.std_empty ()) init_items);
    todo5=(init_items);
    oracle5=maps.map_sym_sym_int_int.mssii_empty ();
    tmoracle5=maps.map_tm_int.mti_empty ();
    blocked5=maps.map_blocked_key.mbk_empty ();
    complete5=maps.map_complete_key.mck_empty ()
  } in
  s0)

let post_process (s,ctxt) = (
  let open E3_core in
  (* oracle *)
  let o = s.oracle5 in
  (*  let tbl = Hashtbl.create 100 in *)
  let f1 (sym1,sym2,i,j) = (
    ctxt.maps.map_sym_sym_int_int.mssii_elts_cod (sym1,sym2,i,j) o)
  in 
  (* FIXME we really want to memoize using the int arguments - otherwise this is grossly inefficient *)
  let o = fun (sym1,sym2) (i,j) -> (* memo tbl *) f1 (sym1,sym2,i,j) in
  (* tmoracle *)
  let tmo = s.tmoracle5 in
  let tmo tm (i,j) = ( (* FIXME are we sure this is as efficient as it can be? *)
    (ctxt.maps.map_tm_int.mti_find_cod (tm,i) j tmo))
  in
  (* now construct the oracles *)
  object
    method oracle=o;
    method tmoracle=tmo;
  end)

let earley_full_from_record_type setup0 = (
  let nt = P4_core.(match setup0.std_sym with | NP np -> np | _ -> failwith "earley_full_from_record_type: start symbol not an nt") in
  let init_items = List.map (fun x -> `NTITM x) (nt_items_for_nt nt (setup0.std_string,0,0)) in
  let len = setup0.std_length in
  let ctxt = { 
    string5=setup0.std_string; 
    length5=len; 
    item_ops5=mk_ops (setup0.std_string); 
    sets=(Sets_maps.sets len); maps=(Sets_maps.maps len) } 
  in
  let init_state = mk_init_loop2 ctxt init_items in
  let s = E3_core.earley ctxt init_state in
  post_process (s,ctxt))
  

let earley_full setup0 = (
  let init_items = List.map (fun x -> `NTITM x) (nt_items_for_nt setup0#std_sym (setup0#std_string,0,0)) in
  let len = setup0#std_length in
  match setup0#std_sets_maps with
  | None -> (
      let ctxt = { 
        string5=setup0#std_string; 
        length5=len; 
        item_ops5=mk_ops (setup0#std_string); 
        sets=(Sets_maps.sets len); maps=(Sets_maps.maps len) } 
      in
      let init_state = mk_init_loop2 ctxt init_items in
      let s = E3_core.earley ctxt init_state in
      post_process (s,ctxt))
  | Some (sets,maps) -> (
      let ctxt = { 
        string5=setup0#std_string; 
        length5=len; 
        item_ops5=mk_ops setup0#std_string; 
        sets=sets; 
        maps=maps } 
      in
      let init_state = mk_init_loop2 ctxt init_items in
      let s = E3_core.earley ctxt init_state in
      post_process (s,ctxt)))

  
(*

examples

open P4_core
open E3_std

let _E = ref (mkclause (`NT 2))

let _F = ref (mkclause (`NT 4))

let _ = mkntparser_ref _E (fun () -> alts [
  ((!_E >-- !_E >- !_E) >> (fun ((z,x),y) -> z+x+y));
  rhs !_F;
  rhs _1])

let _ = mkntparser_ref _F (fun () -> alts [
  (!_E >-- !_E)>>(fun (x,y) -> x+y)])

let txt = "1111111"

let setup0 = {
  std_sym=((!_E).sp);
  std_string=txt;
  std_length=String.length txt;
}

let r = earley_full_from_record_type setup0

let [r1;r2;r3] = (!_E.sp) |> dest_NP |> fun x -> x.np_rhss ()

this is now quite confusing! what do we need to give to the oracle?

let _EEdotE = r1 |> List.rev |> List.tl

let [2] = r#oracle (_EEdotE,!_E.sp) (0,3)

let inr = 
  let ss = (`SS(txt,0,String.length txt)) in
  {
  ss4=ss;
  box4=Box.box txt;
  lc4=LC[];
  oracle4=r#oracle;
  tmoracle4=r#tmoracle
}

let _ = !_E.act inr 


*)
