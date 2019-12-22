(* XXX this has been changed in later OCaml version.*)

(* type 'a numtable = {
 *   num_cnt : int;
 *   (\* The next number *\)
 *   num_tbl : ('a, int) Tbl.t;
 * } *)

(* The table of already numbered objects *)

type bytecode_exe = {
  toc : (bytes * int) list;
  crcs : (string * Digest.t option) list;
  dplt : string;
  dlls : string;
  code : Int32.t array;
  prim : string array;
  data : string; (* XXX changed*)
                 (* symb : Ident.t numtable option; *)
}

val empty : bytecode_exe

val get_global_data64 : bytecode_exe -> int -> int64 array

val bytecode_exe : string -> bytecode_exe
