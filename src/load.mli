type 'a numtable =
  { num_cnt: int;               (* The next number *)
    num_tbl: ('a, int) Tbl.t }  (* The table of already numbered objects *)

type bytecode_exe = 
  {
    toc : (bytes * int) list;
    crcs : (string * Digest.t option) list;
    dplt : string;
    dlls : string;
    code : Int32.t array;
    prim : string array;
    data : string;
    symb : Ident.t numtable option (* XXX err - numtable is not exposed in compiler-libs *)
  }

val empty : bytecode_exe

type repr64 = 
  [ `f of int64 * int64 array 
  | `b of int64 * repr64 array 
  | `i of int64 ]

val get_repr64 : Obj.t -> repr64

val get_data64 : repr64 -> int -> int64 array

val get_global_data64 : bytecode_exe -> int -> int64 array

val bytecode_exe : string -> bytecode_exe

