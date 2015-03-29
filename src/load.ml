(* XXX copied from bytecomp/symtable.ml - MUST MATCH EXACTLY *)
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

let prims str = 
  let pos = ref 0 in
  let prims = ref [] in
  while !pos < String.length str do
    let i = String.index_from str !pos '\000' in
    prims := (String.sub str !pos (i - !pos)) :: !prims;
    pos := i + 1
  done;
  !prims

let bytecode_exe exe_name = 
  Symtable.reset ();
  Bytesections.reset ();
  let f = open_in exe_name in
  Bytesections.read_toc f;
  let toc = Bytesections.toc () in
  let str s = try Bytesections.read_section_string f s with Not_found -> "" in
  let exe = 
    {
      toc = toc;
      crcs = 
        (Obj.magic (Bytesections.read_section_struct f "CRCS") : (string * Digest.t option) list);
      dplt = str "DPLT";
      dlls = str "DLLS";
      code = str "CODE";
      prim = prims (str "PRIM");
      data = str "DATA";
      symb = 
        (Obj.magic (Bytesections.read_section_struct f "SYMB") : Ident.t numtable);
    }
  in
  close_in f;
  exe


