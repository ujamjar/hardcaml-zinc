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

include Mlvalues.Make(Ops.Int64)

(* maintain a small seperate heap section of c-allocation *)
let c_heap_address = ref 0
let init addr = c_heap_address := addr/8

(* allocate a block in the c-heap. *)
let alloc_block st size colour tag = 
  let p = !c_heap_address in
  c_heap_address := !c_heap_address + (Int64.to_int size) + 1;
  st.memory.{p} <- Int64.(make_header size colour tag);
  Int64.of_int ((p+1)*8)

(*let is_int v = (logand v 1L) <> 0L 
let is_block v = not (is_int v) 
let tag v = logand v 0xFFL 
let size v = shift_right_logical v 10 
let int_val v = shift_right v 1

let val_unit = 1L
let val_int x = logor (shift_left x 1) 1L
*)

let header st v = st.memory.{((to_int v) / 8)-1} 
let field st v i = st.memory.{((to_int v) / 8)+i} 
let set_field st v i d = st.memory.{((to_int v) / 8)+i} <- d

let c1_unit = C1 (fun _ _ -> val_unit)
let c2_unit = C2 (fun _ _ _ -> val_unit)
let c3_unit = C3 (fun _ _ _ _ -> val_unit)
let c4_unit = C4 (fun _ _ _ _ _ -> val_unit)
let c5_unit = C5 (fun _ _ _ _ _ _ -> val_unit)

let c1_id = C1 (fun _ a -> a)

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


let c_calls = [
  "caml_register_named_value", c2_unit;
  "caml_set_oo_id", set_oo_id;
  "caml_int64_float_of_bits", int64_float_of_bits;
  "caml_ml_open_descriptor_in", open_descriptor;
  "caml_ml_open_descriptor_out", open_descriptor;
  "caml_ml_output_char", output_char;
  "caml_ml_string_length", string_length;
  "caml_ml_output", output_string;
]

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

