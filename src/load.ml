(* XXX copied from bytecomp/symtable.ml - MUST MATCH EXACTLY *)
(* type 'a numtable =
 *   { num_cnt: int;               (\* The next number *\)
 *     num_tbl: ('a, int) Tbl.t }  (\* The table of already numbered objects *\) *)
open Base
module Gc = Caml.Gc
module Digest = Caml.Digest
module Obj = Caml.Obj
module Marshal = Caml.Marshal

type digest = (Digest.t[@sexp.opaque]) [@@deriving sexp_of]
type code = (Int32.t array[@sexp.opaque]) [@@deriving sexp_of]

type bytecode_exe =
  { toc : (string * int) list
  ; crcs : (string * digest option) list
  ; dplt : string
  ; dlls : string
  ; code : code
  ; prim : string array
  ; data : string
  }
[@@deriving sexp_of]

let empty =
  { toc = []
  ; crcs = []
  ; dplt = ""
  ; dlls = ""
  ; code = [||]
  ; prim = [||]
  ; data = "" (* symb = None; *)
  }
;;

let prims str =
  let pos = ref 0 in
  let prims = ref [] in
  while !pos < String.length str do
    let i = String.index_from_exn str !pos '\000' in
    prims := String.sub str ~pos:!pos ~len:(i - !pos) :: !prims;
    pos := i + 1
  done;
  Array.of_list @@ List.rev !prims
;;

let byte_codes str =
  let len = String.length str in
  Array.init (len / 4) ~f:(fun i ->
      let a =
        Array.init 4 ~f:(fun j ->
            Int32.shift_left (Int32.of_int_exn (Char.to_int str.[(i * 4) + j])) (j * 8))
      in
      Array.fold a ~init:0l ~f:Int32.( lor ))
;;

let get_global_data64 exe offset =
  Repr.to_data64 (Repr.of_obj (Obj.repr (Marshal.from_string exe.data 0))) offset
;;

let bytecode_exe exe_name =
  Symtable.reset ();
  Bytesections.reset ();
  let f = Stdio.In_channel.create exe_name in
  Bytesections.read_toc f;
  let toc = Bytesections.toc () in
  let str s =
    try Bytesections.read_section_string f s with
    | Caml.Not_found -> ""
  in
  let exe =
    { toc
    ; crcs =
        (Obj.magic (Bytesections.read_section_struct f "CRCS") : (string
                                                                 * Digest.t option)
                                                                 list)
    ; dplt = str "DPLT"
    ; dlls = str "DLLS"
    ; code = byte_codes (str "CODE")
    ; prim = prims (str "PRIM")
    ; data =
        str "DATA"
        (* symb =
         *   Some
         *     ( Obj.magic (Bytesections.read_section_struct f "SYMB")
         *       : Ident.t numtable ); *)
    }
  in
  Stdio.In_channel.close f;
  Gc.major ();
  exe
;;
