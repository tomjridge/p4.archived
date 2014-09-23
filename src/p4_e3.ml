open P4_core

let run_earley p txt len = (
  let open P4_e3_ds in
  let setup0 = { 
    std_sym=p.sp;
    std_string=txt;
    std_length=len }
  in
  let r = earley_full_from_record_type setup0 in
  (r#oracle,r#tmoracle))

let (_:('a,'b)parser3 -> 'a -> int -> ('a ty_oracle * 'a ty_tmoracle)) = run_earley

let run_parser3' p txt len (o,tmo) = (
  let ss = (`SS(txt,0,len)) in
  let inr = {
    ss4=ss;
    box4=Box.box txt;
    lc4=LC[];
    oracle4=o;
    tmoracle4=tmo }
  in
  p.act inr)

(* NB staging! run_parser3 p does significant work *)
let run_parser3 p txt len = (
  let (o,tmo) = run_earley p txt len in
  run_parser3' p txt len (o,tmo))

let (_: ('a,'b)parser3 -> 'a -> int -> 'b list) = run_parser3

let run_parser3_string p txt = run_parser3 p txt (String.length txt)
let (_: (string,'b)parser3 -> string -> 'b list) = run_parser3_string



(*

examples

let raw_a1 = (fun x -> match x with `SS(s,i,j) -> if i < j && s.[i]='1' then [i+1] else [])

(* invariant: on creation, syms are set and are not subsequently altered *)
let _1 = mktmparser raw_a1 (fun ss -> if content ss = "1" then [1] else [])

let _E = ref (mkclause (`NT 2))

let _F = ref (mkclause (`NT 4))

let _ = mkntparser_ref _E (fun () -> alts [
  ((!_E >-- !_E >- !_E) >> (fun ((z,x),y) -> z+x+y));
  rhs !_F;
  rhs _1])

let _ = mkntparser_ref _F (fun () -> alts [
  (!_E >-- !_E)>>(fun (x,y) -> x+y)])

let txt = "1111111"
let len = String.length txt

let _ = run_parser3 !_E txt len

*)




(**********************************************************************)
(* signature - for testing prior to editing p3_lib.{ml,mli} *)

module type T = sig 

open P4_core

type 'a identified



(* to allow arbitrary user-constructed terminal parsers, we need to
   expose these types for mktmparser *)
type 'string ty_span = [ `SS of ('string * int * int) ]
val content: string ty_span -> string



type 'string raw_parser = 'string ty_span -> int list
type ('string,'a) raw_act = 'string ty_span -> 'a list

type ('string,'a) parser3

val mktmparser: 'string raw_parser -> ('string,'a) raw_act -> ('string,'a) parser3 identified



type ('string,'a)rhs

val rhs: ('a,'b)parser3 -> ('a,'b)rhs identified

val seq: ('a,'b)rhs -> ('a,'c)parser3 -> ('a,'b*'c)rhs identified


type ('string,'a)alts

val alts: ('a,'b)rhs identified list -> ('a,'b)alts



val mk_pre_parser: unit -> ('a,'b)parser3 identified  (* parser for nonterminal; the alts and act will fail *)

val mkntparser: ('a,'b)parser3 identified -> (unit -> ('c,'d) alts) -> ('c,'d)parser3 identified

val run_parser3: ('a,'b)parser3 -> 'a -> int -> 'b list


val read_file_as_string: string -> string option

end

(**********************************************************************)
(* implementation *)


module I : T = struct

type 'a identified = 'a


type 'a ty_span = 'a P4_core.ty_span
let content = P4_core.content


type 'a raw_parser = 'a P4_core.raw_parser
type ('a,'b) raw_act = ('a,'b) P4_core.raw_act

type ('a,'b) parser3 = ('a,'b) P4_core.parser3

let mktmparser = P4_core.mktmparser


type ('a,'b)rhs = ('a,'b) P4_core.rhs

let rhs = P4_core.rhs

let seq = P4_core.seq


type ('a,'b) alts = ('a,'b) P4_core.alts

let alts = P4_core.alts

let mk_pre_parser = P4_core.mk_pre_parser

let mkntparser = P4_core.mkntparser

let run_parser3 = run_parser3


let read_file_as_string = P4_util.read_file_as_string

end

