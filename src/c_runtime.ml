(* c-runtime hacks *)

open Machine

type result = [ `ok of int64 | `exn of int64 ]

type c_call = 
  | C1 of (state -> int64 -> result)
  | C2 of (state -> int64 -> int64 -> result)
  | C3 of (state -> int64 -> int64 -> int64 -> result)
  | C4 of (state -> int64 -> int64 -> int64 -> int64 -> result)
  | C5 of (state -> int64 -> int64 -> int64 -> int64 -> int64 -> result)
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
  (*Printf.printf "C_runtime.bump %i %x\n" size (!c_heap_address * 8);*)
  if not (!c_heap_address <= !c_heap_address_max) then failwith "c-heap out of memory"

(* allocate a block in the c-heap. *)
let alloc_block st size colour tag = 
  let p = !c_heap_address in
  bump ((Int64.to_int size) + 1);
  st.memory.{p} <- Int64.(make_header size colour tag);
  Int64.of_int ((p+1)*8)

let get_repr : 'a -> int -> int64 array = fun a ofs ->
  Repr.data64_of_repr64 (Repr.repr64_of_obj (Obj.repr a)) ofs 

let get_obj : state -> int64 -> 'a = fun st p ->
  Obj.magic (Repr.obj_of_repr64 (Repr.repr64_of_data64 st.memory p))

let alloc_block_from : state -> 'a -> int64 = fun st a -> 
  let p = !c_heap_address in
  let a = get_repr a p in
  let size = Array.length a in
  bump size;
  for i=0 to size - 1 do
    st.memory.{i+p} <- a.(i)
  done;
  Int64.of_int ((p+1)*8)

let header st v = st.memory.{((to_int v) / 8)-1} 
let field st v i = st.memory.{((to_int v) / 8)+i} 
let set_field st v i d = st.memory.{((to_int v) / 8)+i} <- d

let c1_unit = C1 (fun _ _ -> `ok val_unit)
let c2_unit = C2 (fun _ _ _ -> `ok val_unit)
let c3_unit = C3 (fun _ _ _ _ -> `ok val_unit)
let c4_unit = C4 (fun _ _ _ _ _ -> `ok val_unit)
let c5_unit = C5 (fun _ _ _ _ _ _ -> `ok val_unit)

let c1_id = C1 (fun _ a -> `ok a)

let c1_int i = C1 (fun _ _ -> `ok (val_int i))
let c1_true = C1 (fun _ _ -> `ok val_true)
let c1_false = C1 (fun _ _ -> `ok val_false)

let caml_set_oo_id = 
  let c = ref 1L in
  C1 (fun st ptr ->
    set_field st ptr 0 !c;
    c := add !c 2L;
    `ok ptr)

let caml_int64_float_of_bits = 
  C1 (fun st v ->
    let p = alloc_block st 1L black double_tag in
    set_field st p 0 (field st v 1);
    `ok p)

let file_descrs_in = ref []
let file_descrs_out = ref []

let descr_block st fd = 
  let p = alloc_block st 2L white custom_tag in (* int * int, fd is second field *)
  set_field st p 0 1L;
  set_field st p 1 fd;
  p

let get_descr x = (* XXX assumption; file_descr <=> int *) 
  (Obj.magic (Int64.to_int (int_val x)) : Unix.file_descr)

let find_chan_in st p = 
  try fst @@ List.assoc (field st p 1) !file_descrs_in
  with _ -> failwith ("in channel not found (" ^ Int64.to_string (field st p 1) ^ ")")

let find_chan_out st p = 
  try fst @@ List.assoc (field st p 1) !file_descrs_out
  with _ -> failwith ("out channel not found (" ^ Int64.to_string (field st p 1) ^ ")")

let caml_ml_open_descriptor_in = 
  C1 (fun st fd -> 
    let fd' = get_descr fd in
    let p = descr_block st fd in
    file_descrs_in := (fd, (Unix.in_channel_of_descr fd', p)) :: !file_descrs_in;
    `ok p)

let caml_ml_open_descriptor_out = 
  C1 (fun st fd -> 
    let fd' = get_descr fd in
    let p = descr_block st fd in
    file_descrs_out := (fd, (Unix.out_channel_of_descr fd', p)) :: !file_descrs_out;
    `ok p)

let caml_ml_close_channel = 
  C1 (fun st fd ->
    (try close_in (find_chan_in st fd)
    with _ -> close_out (find_chan_out st fd));
    `ok val_unit)

let mk_list st l = List.fold_right
  (fun p l -> 
    let x = alloc_block st 2L 0L white in
    set_field st x 0 p;
    set_field st x 1 l;
    x)
  l (val_int 0L) 

let caml_ml_out_channels_list = 
  C1 (fun st _ -> `ok (mk_list st (List.map (fun (_,(_,p)) -> p) !file_descrs_out)))

let caml_ml_output_char = 
  C2 (fun st chan c ->
    output_char (find_chan_out st chan) (Char.chr (to_int (int_val c)));
    `ok val_unit)

let caml_ml_flush = 
  C1 (fun st chan ->
    let chan = find_chan_out st chan in
    flush chan;
    `ok val_unit)

let caml_ml_string_length = 
  C1 (fun st v ->
    assert (string_tag = tag (header st v));
    let size = to_int @@ size (header st v) in
    let pad = to_int @@ shift_right_logical (field st v (size-1)) 56 in
    `ok (val_int @@ of_int @@ ((size * 8) - pad - 1))
  )

let get_string st v = (get_obj st v : string)

let caml_ml_output = 
  C4 (fun st chan bytes ofs len ->
    let str = get_string st bytes in
    output_substring (find_chan_out st chan) str 
      (to_int @@ int_val ofs) (to_int @@ int_val len);
    `ok val_unit)

let caml_ml_input_scan_line =
  C1 (fun st fd ->
    let chan = find_chan_in st fd in
    let pos = pos_in chan in
    let rec f n = 
      try 
        if input_char chan = '\n' then n+1
        else f (n+1)
      with _ -> -n
    in
    let n = f 0 in
    let () = seek_in chan pos in (* restore file pos (I think) *)
    `ok (val_int (Int64.of_int n)))

let set_byte st s ofs b = 
  let w = field st s (ofs / 8) in
  let sft = of_int ((ofs land 7) * 8)  in
  let mask = ~: (sll 255L sft) in
  let b = Int64.of_int (Char.code b) in
  let b = sll b sft in
  set_field st s  (ofs / 8) ((w &: mask) |: b)

let get_byte st s ofs = 
  let w = field st s (ofs / 8) in
  let sft = of_int ((ofs land 7) * 8)  in
  (srl w sft) &: 255L 

let caml_ml_input =
  C4 (fun st ic s ofs len -> 
    let ofs = Int64.to_int (int_val ofs) in
    let len = Int64.to_int (int_val len) in
    let b = Bytes.create len in
    let ichan = find_chan_in st ic in
    let len = input ichan b 0 len in
    for i=0 to len-1 do
      set_byte st s (ofs+i) (Bytes.get b i)
    done;
    `ok (val_int (Int64.of_int len)))

let caml_ml_input_char = 
  C1 (fun st ic ->
    let chan = find_chan_in st ic in
    let ch = Char.code (input_char chan) in
    `ok (val_int (of_int ch)))

external open_desc : string -> open_flag list -> int -> int = "caml_sys_open"

let caml_sys_open =
  C3 (fun st fname mode perm ->
    let fname = get_string st fname in
    let mode = (get_obj st mode : open_flag list) in
    let perm = Int64.to_int (int_val perm) in
    (*Printf.printf "sys_open %s, mode=%i, perm=%x\n"
      fname (List.length mode) perm;*)
    let fd = open_desc fname mode perm in
    `ok (val_int (Int64.of_int fd)))

let caml_create_string = 
  C1 (fun st len ->
    let x = String.make (to_int (int_val len)) (Char.chr 0) in
    `ok (alloc_block_from st x))

let caml_blit_string = 
  C5 (fun st s1 ofs1 s2 ofs2 n -> 
    let n = to_int (int_val n) in
    let ofs1 = to_int (int_val ofs1) in
    let ofs2 = to_int (int_val ofs2) in
    for i=0 to n-1 do
      let b = Char.chr @@ to_int @@ (get_byte st s1 (ofs1+i)) in
      set_byte st s2 (ofs2+i) b
    done;
    `ok val_unit)

let argv = ref ("hardcamlzinc", [|"hardcamlzinc"|])

let caml_sys_get_argv = 
  C1 (fun st _ ->
    `ok (alloc_block_from st !argv))

let caml_sys_get_config = 
  C1 (fun st _ -> `ok (alloc_block_from st ("hardcamlzinc", 64, false)))

let caml_sys_getenv = 
  C1 (fun st v ->
    let v = get_string st v in
    `ok (alloc_block_from st (Sys.getenv v)))

let caml_make_vect =
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
      `ok p)

let caml_array_get_addr = 
  C2 (fun st a idx ->
    let idx = int_val idx in
    let size = size (header st a) in
    (* XXX bounds check *)
    if idx >= size || idx < 0L then `exn (val_false)
    else `ok (field st a (to_int idx)))

let caml_obj_block =
  C2 (fun st tag size -> 
    let size = int_val size in
    let tag = int_val size in
    let p = alloc_block st size tag white in
    for i=0 to (to_int size)-1 do
      set_field st p i (val_int 0L)
    done;
    `ok p)

let caml_obj_dup = 
  C1 (fun st obj ->
    let size = size (header st obj) in
    if size=0L then `ok obj
    else 
      let tag = tag (header st obj) in
      let p = alloc_block st size tag white in
      for i=0 to (to_int size)-1 do
        set_field st p i (field st obj i)
      done;
      `ok p)

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
      set_field st p 1 (Repr.int64_of_obj Obj.(field (repr c) 1));
      `ok p)

  let op1 f =
    C1 (fun st a ->
      let p = alloc_block st 2L custom_tag white in
      let c = f (I.to_t a) in
      set_field st p 0 0L; (* XXX should point to custom allocation block... *)
      set_field st p 1 (Repr.int64_of_obj Obj.(field (repr c) 1));
      `ok p)

  let sftop f =
    C2 (fun st a b ->
      let p = alloc_block st 2L custom_tag white in
      let c = f (I.to_t a) (to_int (int_val b)) in
      set_field st p 0 0L; (* XXX should point to custom allocation block... *)
      set_field st p 1 (Repr.int64_of_obj Obj.(field (repr c) 1));
      `ok p)

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
  "caml_set_oo_id", caml_set_oo_id;
  "caml_int64_float_of_bits", caml_int64_float_of_bits;
  "caml_ml_open_descriptor_in", caml_ml_open_descriptor_in;
  "caml_ml_open_descriptor_out", caml_ml_open_descriptor_out;
  "caml_ml_close_channel", caml_ml_close_channel;
  "caml_ml_output_char", caml_ml_output_char;
  "caml_ml_string_length", caml_ml_string_length;
  "caml_ml_output", caml_ml_output;
  "caml_ml_out_channels_list", caml_ml_out_channels_list;
  "caml_ml_flush", caml_ml_flush;
  "caml_ml_input_scan_line", caml_ml_input_scan_line;
  "caml_ml_input", caml_ml_input;
  "caml_ml_input_char", caml_ml_input_char;
  "caml_create_string", caml_create_string;
  "caml_blit_string", caml_blit_string;
  "caml_sys_open", caml_sys_open;
  "caml_sys_get_argv", caml_sys_get_argv;
  "caml_sys_get_config", caml_sys_get_config;
  "caml_sys_const_big_endian", c1_false;
  "caml_sys_const_word_size", c1_int 64L;
  "caml_sys_const_ostype_unix", c1_true;
  "caml_sys_const_ostype_win32", c1_false;
  "caml_sys_const_ostype_cygwin", c1_false;
  "caml_sys_getenv", caml_sys_getenv;
  "caml_make_vect", caml_make_vect;
  "caml_array_get_addr", caml_array_get_addr;
  "caml_obj_block", caml_obj_block;
  "caml_obj_dup", caml_obj_dup;
] @ Nativeint_c_calls.c_calls
  @ Int64_c_calls.c_calls
  @ Int32_c_calls.c_calls

let run bc prim st =
  let arg ofs = st.memory.{(to_int st.sp) / 8 + ofs} in
  match List.assoc bc.Load.prim.(prim) c_calls with
  | C1(f) -> f st st.accu
  | C2(f) -> f st st.accu (arg 1)
  | C3(f) -> f st st.accu (arg 1) (arg 2)
  | C4(f) -> f st st.accu (arg 1) (arg 2) (arg 3)
  | C5(f) -> f st st.accu (arg 1) (arg 2) (arg 3) (arg 4)
  | CN -> failwith "C_CALLN not yet implemented"
  | exception Not_found -> failwith ("C primitive " ^ bc.Load.prim.(prim) ^ " not found")
  | exception _ -> failwith "C primitive error"

