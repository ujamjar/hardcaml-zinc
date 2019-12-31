open Base

type bytecode_exe =
  { toc : (string * int) list
  ; crcs : (string * Caml.Digest.t option) list
  ; dplt : string
  ; dlls : string
  ; code : Int32.t array
  ; prim : string array
  ; data : string
  }
[@@deriving sexp_of]

(* XXX We used to have a symb field of numtable entries, but OCaml seems to have
   changed here and we weren't actually using it - probably more for debug then
   anything else. *)

val empty : bytecode_exe
val get_global_data64 : bytecode_exe -> int -> int64 array
val bytecode_exe : string -> bytecode_exe
