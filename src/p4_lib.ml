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
let ( >- ) = P4_core.( >- )
let ( >-- ) = P4_core.( >-- )
let ( >> ) = P4_core.( >> )
let ( >>> ) = P4_core.( >>> )


type ('a,'b) alts = ('a,'b) P4_core.alts

let alts = P4_core.alts

let mk_pre_parser = P4_core.mk_pre_parser

let mkntparser = P4_core.mkntparser


type hashkey = P4_core.hashkey
type 'b hashvalue = 'b P4_core.hashvalue
let memo_p = P4_core.memo_p


let run_parser = P4_e3.run_parser
let run_parser_string = P4_e3.run_parser_string


let a = P4_basic_parsers.a
let until_a = P4_basic_parsers.until_a
let parse_RE = P4_basic_parsers.parse_RE
let parse_not_RE = P4_basic_parsers.parse_not_RE


let read_file_as_string = P4_util.read_file_as_string
