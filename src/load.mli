type 'a numtable =
  { num_cnt: int;               (* The next number *)
    num_tbl: ('a, int) Tbl.t }  (* The table of already numbered objects *)

type bytecode_exe = 
  {
    toc : (bytes * int) list;
    crcs : (string * Digest.t option) list;
    dplt : string;
    dlls : string;
    code : string;
    prim : string list;
    data : string;
    symb : Ident.t numtable (* XXX err - numtable is not exposed in compiler-libs *)
  }

val bytecode_exe : string -> bytecode_exe

