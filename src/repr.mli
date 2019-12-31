open Base
module Obj = Caml.Obj

(** intermediate representation of ocaml memory values *)
type t =
  | Flat of
      { header : Int64.t
      ; data : Int64.t array
      }
  | Block of
      { header : Int64.t
      ; data : t array
      }
  | Int of Int64.t
[@@deriving sexp_of]

(** bit pattern of an obj, including the low order hidden bit *)
val int64_of_obj : Obj.t -> int64

(** convert [Obj.t] to [t]. If closure is false, then blocks with closure tags
   are converted to unit *)
val of_obj : ?closure:bool -> Obj.t -> t

(** convert [t] to [Obj.t] *)
val to_obj : t -> Obj.t

(** int64 array of [t].  Internally pointers are offset. *)
val to_data64 : t -> int -> int64 array

(** [t] of int64 array **)
val of_data64 : ?closure:bool -> Memory.t -> int64 -> t
