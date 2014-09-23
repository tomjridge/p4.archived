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

val rhs: ('a,'b)parser3 identified -> ('a,'b)rhs identified

val seq: ('a,'b)rhs identified -> ('a,'c)parser3 identified -> ('a,'b*'c)rhs identified
val ( >- ): ('a,'b)rhs identified -> ('a,'c)parser3 identified -> ('a,'b*'c)rhs identified
val ( >-- ): ('a,'b)parser3 identified -> ('a,'c) parser3 identified -> ('a,'b*'c)rhs identified
val ( >> ): ('a,'b)rhs identified -> ('b->'c) -> ('a,'c)rhs identified
val ( >>> ): ('a,'b)parser3 identified -> ('b->'c) -> ('a,'c)parser3 identified


type ('string,'a)alts

val alts: ('a,'b)rhs identified list -> ('a,'b)alts 



val mk_pre_parser: unit -> ('a,'b)parser3 identified  (* parser for nonterminal; the alts and act will fail *)

val mkntparser: ('a,'b)parser3 identified -> (unit -> ('c,'d) alts) -> ('c,'d)parser3 identified



type hashkey
type 'b hashvalue
val memo_p3: (hashkey,'b hashvalue) Hashtbl.t -> ('a,'b)parser3 identified -> ('a,'b)parser3 identified


val run_parser3: ('a,'b)parser3 identified -> 'a -> int -> 'b list
val run_parser3_string: (string,'b)parser3 identified -> string -> 'b list



val a: string -> (string, string ty_span)parser3 identified
val until_a: string -> (string, string ty_span)parser3 identified
val parse_RE: string -> (string, string ty_span)parser3 identified
val parse_not_RE: string -> (string, string ty_span)parser3 identified


val read_file_as_string: string -> string option
