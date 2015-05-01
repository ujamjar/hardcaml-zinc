type memory = (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t 

type repr64 = 
  [ `f of int64 * int64 array 
  | `b of int64 * repr64 array 
  | `i of int64 ]

val int64_of_obj : Obj.t -> int64

val repr64_of_obj : Obj.t -> repr64

val obj_of_repr64 : repr64 -> Obj.t

val data64_of_repr64 : repr64 -> int -> int64 array

val repr64_of_data64 : memory -> int64 -> repr64

