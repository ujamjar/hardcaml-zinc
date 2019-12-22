type memory = (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t

type repr64 =
  [ `f of int64 * int64 array | `b of int64 * repr64 array | `i of int64 ]
(** intermediate representation of ocaml memory values *)

val int64_of_obj : Obj.t -> int64
(** bit pattern of an obj, including the low order hidden bit *)

val repr64_of_obj : ?closure:bool -> Obj.t -> repr64
(** convert Obj.t to repr64.  If closure is false, then blocks with
    closure tags are converted to unit *)

val obj_of_repr64 : repr64 -> Obj.t
(** conver repr64 to Obj.t *)

val data64_of_repr64 : repr64 -> int -> int64 array
(** int64 array of repr64.  Internally pointers are offset. *)

(* repr64 of int64 array *)
val repr64_of_data64 : ?closure:bool -> memory -> int64 -> repr64
