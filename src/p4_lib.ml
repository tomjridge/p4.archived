type 'a identified = 'a


type 'a ty_span = 'a P3_core.ty_span
let content = P3_core.content


type 'a raw_parser = 'a P3_core.raw_parser
type ('a,'b) raw_act = ('a,'b) P3_core.raw_act

type ('a,'b) parser3 = ('a,'b) P3_core.parser3

let mktmparser = P3_core.mktmparser


type ('a,'b)rhs = ('a,'b) P3_core.rhs

let rhs = P3_core.rhs

let seq = P3_core.seq
let ( >- ) = P3_core.( >- )
let ( >-- ) = P3_core.( >-- )
let ( >> ) = P3_core.( >> )
let ( >>> ) = P3_core.( >>> )


type ('a,'b) alts = ('a,'b) P3_core.alts

let alts = P3_core.alts

let mk_pre_parser = P3_core.mk_pre_parser

let mkntparser = P3_core.mkntparser


type hashkey = P3_core.hashkey
type 'b hashvalue = 'b P3_core.hashvalue
let memo_p3 = P3_core.memo_p3


let run_parser3 = P3_e3.run_parser3
let run_parser3_string = P3_e3.run_parser3_string


let a = P3_basic_parsers.a
let until_a = P3_basic_parsers.until_a
let parse_RE = P3_basic_parsers.parse_RE
let parse_not_RE = P3_basic_parsers.parse_not_RE


let read_file_as_string = Tr_simple_file.read_file_as_string
