(* XXX copied from bytecomp/symtable.ml - MUST MATCH EXACTLY *)
(* type 'a numtable =
 *   { num_cnt: int;               (\* The next number *\)
 *     num_tbl: ('a, int) Tbl.t }  (\* The table of already numbered objects *\) *)

type bytecode_exe = {
  toc : (string * int) list;
  crcs : (string * Digest.t option) list;
  dplt : string;
  dlls : string;
  code : Int32.t array;
  prim : string array;
  data : string;
      (* symb : Ident.t numtable option (\* XXX err - numtable is not exposed in compiler-libs *\) *)
}

let empty =
  {
    toc = [];
    crcs = [];
    dplt = "";
    dlls = "";
    code = [||];
    prim = [||];
    data = "" (* symb = None; *);
  }

let prims str =
  let pos = ref 0 in
  let prims = ref [] in
  while !pos < String.length str do
    let i = String.index_from str !pos '\000' in
    prims := String.sub str !pos (i - !pos) :: !prims;
    pos := i + 1
  done;
  Array.of_list @@ List.rev !prims

let byte_codes str =
  let len = String.length str in
  Array.init (len / 4) (fun i ->
      let a =
        Array.init 4 (fun j ->
            Int32.shift_left (Int32.of_int (Char.code str.[(i * 4) + j])) (j * 8))
      in
      Array.fold_left Int32.logor 0l a)

let get_global_data64 exe offset =
  Repr.data64_of_repr64
    (Repr.repr64_of_obj (Obj.repr (Marshal.from_string exe.data 0)))
    offset

let bytecode_exe exe_name =
  Symtable.reset ();
  Bytesections.reset ();
  let f = open_in exe_name in
  Bytesections.read_toc f;
  let toc = Bytesections.toc () in
  let str s = try Bytesections.read_section_string f s with Not_found -> "" in
  let exe =
    {
      toc;
      crcs =
        (Obj.magic (Bytesections.read_section_struct f "CRCS") : ( string
                                                                 * Digest.t
                                                                   option )
                                                                 list);
      dplt = str "DPLT";
      dlls = str "DLLS";
      code = byte_codes (str "CODE");
      prim = prims (str "PRIM");
      data =
        str "DATA"
        (* symb =
         *   Some
         *     ( Obj.magic (Bytesections.read_section_struct f "SYMB")
         *       : Ident.t numtable ); *);
    }
  in
  close_in f;
  Gc.major ();
  exe
