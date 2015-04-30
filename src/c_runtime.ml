(* c-runtime hacks *)

type state = 
  {
    env : int64;
    accu : int64;
    sp : int;
    memory : (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t;
  }

type c_call = 
  | C1 of (state -> int64 -> int64)
  | C2 of (state -> int64 -> int64 -> int64)
  | C3 of (state -> int64 -> int64 -> int64 -> int64)
  | C4 of (state -> int64 -> int64 -> int64 -> int64 -> int64)
  | C5 of (state -> int64 -> int64 -> int64 -> int64 -> int64 -> int64)
  | CN

open Int64
open Ops.Int64
include Mlvalues.Make(Ops.Int64)

(* maintain a small seperate heap section of c-allocation *)
let c_heap_address = ref 0
let c_heap_address_max = ref 0
let init addr size = 
  c_heap_address := addr/8;
  c_heap_address_max := !c_heap_address + (size/8)

let bump size = 
  c_heap_address := !c_heap_address + size;
  (*Printf.printf "C_runtime.bump %i (%i/%i)\n" size !c_heap_address !c_heap_address_max;*)
  if not (!c_heap_address <= !c_heap_address_max) then failwith "c-heap out of memory"

(* allocate a block in the c-heap. *)
let alloc_block st size colour tag = 
  let p = !c_heap_address in
  bump ((Int64.to_int size) + 1);
  st.memory.{p} <- Int64.(make_header size colour tag);
  Int64.of_int ((p+1)*8)

let get_repr : 'a -> int -> int64 array = fun a ofs ->
  Load.get_data64 (Load.get_repr64 (Obj.repr a)) ofs 

let alloc_block_from : state -> 'a -> int64 = fun st a -> 
  let p = !c_heap_address in
  let a = get_repr a (p * 8) in
  let size = Array.length a in
  bump size;
  for i=0 to size - 1 do
    st.memory.{i+p} <- a.(i)
  done;
  Int64.of_int ((p+1)*8)

let header st v = st.memory.{((to_int v) / 8)-1} 
let field st v i = st.memory.{((to_int v) / 8)+i} 
let set_field st v i d = st.memory.{((to_int v) / 8)+i} <- d

let c1_unit = C1 (fun _ _ -> val_unit)
let c2_unit = C2 (fun _ _ _ -> val_unit)
let c3_unit = C3 (fun _ _ _ _ -> val_unit)
let c4_unit = C4 (fun _ _ _ _ _ -> val_unit)
let c5_unit = C5 (fun _ _ _ _ _ _ -> val_unit)

let c1_id = C1 (fun _ a -> a)

let c1_int i = C1 (fun _ _ -> val_int i)
let c1_true = C1 (fun _ _ -> val_true)
let c1_false = C1 (fun _ _ -> val_false)

let set_oo_id = 
  let c = ref 1L in
  C1 (fun st ptr ->
    set_field st ptr 0 !c;
    c := add !c 2L;
    ptr)

let int64_float_of_bits = 
  C1 (fun st v ->
    let p = alloc_block st 1L black double_tag in
    set_field st p 0 (field st v 1);
    p)

(* just supporting stdio for now *)
let open_descriptor =
  C1 (fun st fd ->
    if int_val fd < 0L || int_val fd > 2L then failwith "open_descriptor"
    else
      (* allocate a simple custom block for the result storing just the fd *)
      let p = alloc_block st 2L white custom_tag in
      set_field st p 0 1L;
      set_field st p 1 fd;
      p)

let out_channels_list = C1 (fun st _ -> val_unit)

let output_char = 
  C2 (fun st fd c ->
    output_char stdout (Char.chr (to_int (int_val c)));
    val_unit)

let string_length = 
  C1 (fun st v ->
    assert (string_tag = tag (header st v));
    let size = to_int @@ size (header st v) in
    let pad = to_int @@ shift_right_logical (field st v (size-1)) 56 in
    val_int @@ of_int @@ ((size * 8) - pad - 1)
  )

let get_string st v = 
  let size = to_int @@ size (header st v) in
  let pad = to_int @@ shift_right_logical (field st v (size-1)) 56 in
  String.init ((size*8)-pad-1) 
    (fun i ->
      Char.chr (to_int @@ 
        logand 0xFFL @@
        shift_right_logical 
          (field st v (i/8)) 
          ((i mod 8)*8)))

let output_string = 
  C4 (fun st chan bytes ofs len ->
    let str = get_string st bytes in
    output_substring stdout str (to_int @@ int_val ofs) (to_int @@ int_val len);
    val_unit)

let alloc_string = 
  C1 (fun st len ->
    let x = String.make (to_int len) (Char.chr 0) in
    alloc_block_from st x)

let get_argv = 
  C1 (fun st _ ->
    let x = ("hardcamlzinc", [|"hardcamlzinc"|]) in
    alloc_block_from st x)

let get_config = 
  C1 (fun st _ -> alloc_block_from st ("hardcamlzinc", 64, false))

let get_env = 
  C1 (fun st v ->
    let v = get_string st v in
    alloc_block_from st (Sys.getenv v))

let make_vect =
  C2 (fun st len init ->
    let len = int_val len in
    if len = 0L then failwith "make_vect: fixme - atom0"
    else
      let tag = 
        if is_block init = 1L && tag (header st init) = double_tag then double_array_tag 
        else 0L
      in
      let p = alloc_block st len tag white in
      for i=0 to (to_int len)-1 do
        set_field st p i init
      done;
      p)

let obj_block =
  C2 (fun st tag size -> 
    let size = int_val size in
    let tag = int_val size in
    let p = alloc_block st size tag white in
    for i=0 to (to_int size)-1 do
      set_field st p i (val_int 0L)
    done;
    p)

let obj_dup = 
  C1 (fun st obj ->
    let size = size (header st obj) in
    if size=0L then obj
    else 
      let tag = tag (header st obj) in
      let p = alloc_block st size tag white in
      for i=0 to (to_int size)-1 do
        set_field st p i (field st obj i)
      done;
      p)

module type Int = sig
  type t
  val add : t -> t -> t
  val logand : t -> t -> t
  (* bits_of_float *)
  (* bswap *)
  (* compare *)
  val div : t -> t -> t
  (* float_of_bits *)
  (* format *)
  val rem : t -> t -> t (* mod? *)
  val mul : t -> t -> t
  val neg : t -> t
  (* of_float *)
  (* of_int *)
  (* of_int32 *)
  (* of_nativeint *)
  (* of_string *)
  val logor : t -> t -> t
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_logical : t -> int -> t
  val sub : t -> t -> t
  (* to_float *)
  (* to_int *)
  (* to_int32 *)
  (* to_nativeint *)
  val logxor : t -> t -> t

  val name : string
  val to_t : int64 -> t
end

module Nativeint_c_ops = struct
  include Nativeint
  let name = "nativeint"
  let to_t = Int64.to_nativeint
end

module Int64_c_ops = struct
  include Nativeint
  let name = "nativeint"
  let to_t = Int64.to_nativeint
end

module Int32_c_ops = struct
  include Nativeint
  let name = "nativeint"
  let to_t = Int64.to_nativeint
end

module Int_c_calls(I : Int) = struct
  let op2 f =
    C2 (fun st a b ->
      let p = alloc_block st 2L custom_tag white in
      let c = f (I.to_t a) (I.to_t b) in
      set_field st p 0 0L; (* XXX should point to custom allocation block... *)
      set_field st p 1 (Load.int64_of_obj Obj.(field (repr c) 1));
      p)

  let op1 f =
    C1 (fun st a ->
      let p = alloc_block st 2L custom_tag white in
      let c = f (I.to_t a) in
      set_field st p 0 0L; (* XXX should point to custom allocation block... *)
      set_field st p 1 (Load.int64_of_obj Obj.(field (repr c) 1));
      p)

  let sftop f =
    C2 (fun st a b ->
      let p = alloc_block st 2L custom_tag white in
      let c = f (I.to_t a) (to_int (int_val b)) in
      set_field st p 0 0L; (* XXX should point to custom allocation block... *)
      set_field st p 1 (Load.int64_of_obj Obj.(field (repr c) 1));
      p)

  let c_calls = [
    "caml_" ^ I.name ^ "_add", op2 I.add;
    "caml_" ^ I.name ^ "_and", op2 I.logand;
    "caml_" ^ I.name ^ "_div", op2 I.div;
    "caml_" ^ I.name ^ "_mod", op2 I.rem;
    "caml_" ^ I.name ^ "_mul", op2 I.mul;
    "caml_" ^ I.name ^ "_neg", op1 I.neg;
    "caml_" ^ I.name ^ "_or", op2 I.logor;
    "caml_" ^ I.name ^ "_shift_left", sftop I.shift_left;
    "caml_" ^ I.name ^ "_shift_right", sftop I.shift_right;
    "caml_" ^ I.name ^ "_shift_right_unsigned", sftop I.shift_right_logical;
    "caml_" ^ I.name ^ "_sub", op2 I.sub;
    "caml_" ^ I.name ^ "_xor", op2 I.logxor;
  ]

end

module Nativeint_c_calls = Int_c_calls(Nativeint_c_ops)
module Int64_c_calls = Int_c_calls(Int64_c_ops)
module Int32_c_calls = Int_c_calls(Int32_c_ops)

let c_calls = [
  "caml_register_named_value", c2_unit;
  "caml_set_oo_id", set_oo_id;
  "caml_int64_float_of_bits", int64_float_of_bits;
  "caml_ml_open_descriptor_in", open_descriptor;
  "caml_ml_open_descriptor_out", open_descriptor;
  "caml_ml_output_char", output_char;
  "caml_ml_string_length", string_length;
  "caml_ml_output", output_string;
  "caml_ml_out_channels_list", out_channels_list;
  "caml_create_string", alloc_string;
  "caml_sys_get_argv", get_argv;
  "caml_sys_get_config", get_config;
  "caml_sys_const_big_endian", c1_false;
  "caml_sys_const_word_size", c1_int 64L;
  "caml_sys_const_ostype_unix", c1_true;
  "caml_sys_const_ostype_win32", c1_false;
  "caml_sys_const_ostype_cygwin", c1_false;
  "caml_sys_getenv", get_env;
  "caml_make_vect", make_vect;
  "caml_obj_block", obj_block;
  "caml_obj_dup", obj_dup;
] @ Nativeint_c_calls.c_calls
  @ Int64_c_calls.c_calls
  @ Int32_c_calls.c_calls

let run bc prim st =
  let arg ofs = st.memory.{st.sp/8 + ofs} in
  match List.assoc bc.Load.prim.(prim) c_calls with
  | C1(f) -> f st st.accu
  | C2(f) -> f st st.accu (arg 1)
  | C3(f) -> f st st.accu (arg 1) (arg 2)
  | C4(f) -> f st st.accu (arg 1) (arg 2) (arg 3)
  | C5(f) -> f st st.accu (arg 1) (arg 2) (arg 3) (arg 4)
  | CN -> failwith "C_CALLN not yet implemented"
  | exception Not_found -> failwith ("C primitive " ^ bc.Load.prim.(prim) ^ " not found")
  | exception _ -> failwith "C primitive error"

