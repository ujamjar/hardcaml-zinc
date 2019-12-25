(* c-runtime hacks *)
open Base
open Machine
module Obj = Caml.Obj
module Parsing = Caml.Parsing

type open_flag = Caml.open_flag

let in_channel_length = Caml.in_channel_length
let input = Caml.input
let input_value = Caml.input_value
let seek_in = Caml.seek_in
let pos_in = Caml.pos_in
let input_char = Caml.input_char
let input_binary_int = Caml.input_binary_int
let close_in = Caml.close_in
let out_channel_length = Caml.out_channel_length
let seek_out = Caml.seek_out
let output_char = Caml.output_char
let output_substring = Caml.output_substring
let close_out = Caml.close_out
let flush = Caml.flush

type result = (Int64.t, Int64.t) Result.t

type c_call =
  | C1 of (state -> int64 -> result)
  | C2 of (state -> int64 -> int64 -> result)
  | C3 of (state -> int64 -> int64 -> int64 -> result)
  | C4 of (state -> int64 -> int64 -> int64 -> int64 -> result)
  | C5 of (state -> int64 -> int64 -> int64 -> int64 -> int64 -> result)
  | CN

type std_exn =
  | OUT_OF_MEMORY_EXN (* "Out_of_memory" *)
  | SYS_ERROR_EXN (* "Sys_error" *)
  | FAILURE_EXN (* "Failure" *)
  | INVALID_EXN (* "Invalid_argument" *)
  | END_OF_FILE_EXN (* "End_of_file" *)
  | ZERO_DIVIDE_EXN (* "Division_by_zero" *)
  | NOT_FOUND_EXN (* "Not_found" *)
  | MATCH_FAILURE_EXN (* "Match_failure" *)
  | STACK_OVERFLOW_EXN (* "Stack_overflow" *)
  | SYS_BLOCKED_IO (* "Sys_blocked_io" *)
  | ASSERT_FAILURE_EXN (* "Assert_failure" *)
  | UNDEFINED_RECURSIVE_MODULE_EXN (* "Undefined_recursive_module" *)
[@@deriving sexp_of, variants]

open Ops.Int64
include Mlvalues.Make (Ops.Int64)

(* maintain a small seperate heap section of c-allocation *)
let c_heap_address = ref 0
let c_heap_address_max = ref 0

let init addr size =
  c_heap_address := addr / 8;
  c_heap_address_max := !c_heap_address + (size / 8)
;;

let bump size =
  c_heap_address := !c_heap_address + size;
  (*Printf.printf "C_runtime.bump %i %x\n" size (!c_heap_address * 8);*)
  if not (!c_heap_address <= !c_heap_address_max) then failwith "c-heap out of memory"
;;

(* alplocate a block in the c-heap. *)
let alloc_block ~st ~size ~colour ~tag =
  let p = !c_heap_address in
  bump (Int64.to_int_exn size + 1);
  st.memory.{p} <- make_header size colour tag;
  Int64.of_int ((p + 1) * 8)
;;

exception Get_repr
exception Get_obj
exception Alloc_block_from

let get_repr : ?closure:bool -> 'a -> int -> int64 array =
 fun ?(closure = true) a ofs ->
  try Repr.data64_of_repr64 (Repr.repr64_of_obj ~closure (Obj.repr a)) ofs with
  | _ -> raise Get_repr
;;

let get_obj : ?closure:bool -> state -> int64 -> 'a =
 fun ?(closure = true) st p ->
  try Obj.magic (Repr.obj_of_repr64 (Repr.repr64_of_data64 ~closure st.memory p)) with
  | _ -> raise Get_obj
;;

let alloc_block_from : state -> 'a -> int64 =
 fun st a ->
  try
    let p = !c_heap_address in
    let a = get_repr a p in
    let size = Array.length a in
    if size = 1
    then a.(0) (* must be a scalar value *)
    else (
      bump size;
      for i = 0 to size - 1 do
        st.memory.{i + p} <- a.(i)
      done;
      Int64.of_int ((p + 1) * 8))
  with
  | _ -> raise Alloc_block_from
;;

let header st v = st.memory.{(Int64.to_int_exn v / 8) - 1}
let field st v i = st.memory.{(Int64.to_int_exn v / 8) + i}
let set_field st v i d = st.memory.{(Int64.to_int_exn v / 8) + i} <- d
let set_header st v d = set_field st v (-1) d
let modify = set_field
let c1_unit = C1 (fun _ _ -> Ok val_unit)
let c2_unit = C2 (fun _ _ _ -> Ok val_unit)
let _c3_unit = C3 (fun _ _ _ _ -> Ok val_unit)
let _c4_unit = C4 (fun _ _ _ _ _ -> Ok val_unit)
let _c5_unit = C5 (fun _ _ _ _ _ _ -> Ok val_unit)
let _c1_id = C1 (fun _ a -> Ok a)
let c1_int i = C1 (fun _ _ -> Ok (val_int i))
let c2_int i = C2 (fun _ _ _ -> Ok (val_int i))
let c1_true = C1 (fun _ _ -> Ok val_true)
let c1_false = C1 (fun _ _ -> Ok val_false)

let caml_copy_string st s =
  let size = size (header st s) in
  let p = alloc_block ~st ~size ~colour:white ~tag:string_tag in
  for i = 0 to Int64.to_int_exn size - 1 do
    set_field st p i (field st s i)
  done;
  p
;;

let caml_copy_string' st s =
  let s = Obj.repr s in
  let size = Obj.size s in
  let p = alloc_block ~st ~size:(Int64.of_int size) ~colour:white ~tag:string_tag in
  for i = 0 to size - 1 do
    set_field st p i (Repr.int64_of_obj (Obj.field s i))
  done;
  p
;;

(* exceptions (fail.c) *)
let caml_raise _ v = Error v
let caml_raise_constant = caml_raise

let caml_raise_with_arg st tag arg =
  let p = alloc_block ~st ~size:2L ~colour:white ~tag:0L in
  set_field st p 0 tag;
  set_field st p 1 arg;
  caml_raise st p
;;

let caml_raise_with_string st tag s =
  let s' = caml_copy_string st s in
  caml_raise_with_arg st tag s'
;;

let caml_raise_with_string' st tag s =
  let s' = caml_copy_string' st s in
  caml_raise_with_arg st tag s'
;;

let exn_field st x = field st st.global_data (Variants_of_std_exn.to_rank x)
let _caml_failwith st s = caml_raise_with_string st (exn_field st FAILURE_EXN) s
let _caml_invalid_argument st s = caml_raise_with_string st (exn_field st INVALID_EXN) s
let caml_invalid_argument' st s = caml_raise_with_string' st (exn_field st INVALID_EXN) s
let caml_array_bound_error st = caml_invalid_argument' st "index out of bounds"
let _caml_raise_out_of_memory st = caml_raise_constant st (exn_field st OUT_OF_MEMORY_EXN)

let _caml_raise_stack_overflow st =
  caml_raise_constant st (exn_field st STACK_OVERFLOW_EXN)
;;

let _caml_raise_sys_error st s = caml_raise_with_arg st (exn_field st SYS_ERROR_EXN) s
let caml_raise_sys_error' st s = caml_raise_with_string' st (exn_field st SYS_ERROR_EXN) s
let _caml_raise_end_of_file st = caml_raise_constant st (exn_field st END_OF_FILE_EXN)
let _caml_raise_zero_divide st = caml_raise_constant st (exn_field st ZERO_DIVIDE_EXN)
let caml_raise_not_found st = caml_raise_constant st (exn_field st NOT_FOUND_EXN)
let _caml_raise_sys_blocked_io st = caml_raise_constant st (exn_field st SYS_BLOCKED_IO)

let _caml_is_special_exception st exn =
  Int64.equal exn (exn_field st MATCH_FAILURE_EXN)
  || Int64.equal exn (exn_field st ASSERT_FAILURE_EXN)
  || Int64.equal exn (exn_field st UNDEFINED_RECURSIVE_MODULE_EXN)
;;

let aofs x = sll x 3L

let caml_alloc st size tag =
  if Int64.equal size 0L
  then st.atom_table +: aofs tag
  else (
    let p = alloc_block ~st ~size ~colour:white ~tag in
    if Int64.equal (tag <+ no_scan_tag) 1L
    then
      for i = 0 to Int64.to_int_exn size - 1 do
        set_field st p i val_unit
      done;
    p)
;;

let caml_alloc_dummy = C1 (fun st size -> Ok (caml_alloc st (int_val size) 0L))

let caml_update_dummy =
  C2
    (fun st dummy newval ->
      let hdummy = header st dummy in
      let hnewval = header st newval in
      assert (Int64.equal (size hdummy) (size hnewval));
      assert (
        Int64.compare (tag newval) no_scan_tag < 0
        || Int64.equal (tag newval) double_array_tag);
      set_header st dummy (make_header (size hdummy) (colour hdummy) (tag hnewval));
      for i = 0 to Int64.to_int_exn (size hdummy) - 1 do
        modify st dummy i (field st newval i)
      done;
      Ok val_unit)
;;

let caml_fresh_oo_id, caml_set_oo_id =
  let c = ref 1L in
  ( C1
      (fun _ _ ->
        let oo_id = !c in
        c := !c +: 2L;
        Ok oo_id)
  , C1
      (fun st ptr ->
        set_field st ptr 1 !c;
        c := !c +: 2L;
        Ok ptr) )
;;

let caml_get_section_table = C1 (fun st _ -> caml_raise_not_found st)

let caml_int64_float_of_bits =
  C1
    (fun st v ->
      let p = alloc_block ~st ~size:1L ~colour:black ~tag:double_tag in
      set_field st p 0 (field st v 1);
      Ok p)
;;

let file_descrs = ref []
let add_descr fd chan ptr = file_descrs := (fd, (chan, ptr)) :: !file_descrs

let remove_descr fd =
  file_descrs := List.filter !file_descrs ~f:(fun (fd', _) -> not (Int64.equal fd' fd))
;;

let descr_block st fd =
  let p = alloc_block ~st ~size:2L ~colour:white ~tag:custom_tag in
  (* int * int, fd is second field *)
  set_field st p 0 1L;
  set_field st p 1 fd;
  p
;;

let get_descr fd =
  (* XXX assumption; file_descr <=> int *)
  let fd : int = Int64.to_int_exn (int_val fd) in
  (Obj.magic fd : Unix.file_descr)
;;

let find_chan st p =
  match List.Assoc.find ~equal:Int64.equal !file_descrs (field st p 1) with
  | Some x -> x
  | None -> failwith ("in channel not found (" ^ Int64.to_string (field st p 1) ^ ")")
;;

let unlink_chan st p =
  let f = find_chan st p in
  remove_descr (field st p 1);
  f
;;

let find_chan_in st p =
  match find_chan st p with
  | `i f, _ -> f
  | _ -> raise Caml.Not_found
;;

let find_chan_out st p =
  match find_chan st p with
  | `o f, _ -> f
  | _ -> raise Caml.Not_found
;;

let caml_ml_open_descriptor_in =
  C1
    (fun st fd ->
      let fd' = get_descr fd in
      let p = descr_block st fd in
      add_descr fd (`i (Unix.in_channel_of_descr fd')) p;
      Ok p)
;;

let caml_ml_open_descriptor_out =
  C1
    (fun st fd ->
      let fd' = get_descr fd in
      let p = descr_block st fd in
      add_descr fd (`o (Unix.out_channel_of_descr fd')) p;
      Ok p)
;;

let caml_ml_close_channel =
  C1
    (fun st fd ->
      (match unlink_chan st fd with
      (* raise exn? *)
      | `i f, _ -> close_in f
      | `o f, _ -> close_out f);
      Ok val_unit)
;;

let mk_list st l =
  List.fold_right l ~init:(val_int 0L) ~f:(fun p l ->
      let x = alloc_block ~st ~size:2L ~tag:0L ~colour:white in
      set_field st x 0 p;
      set_field st x 1 l;
      x)
;;

let caml_ml_out_channels_list =
  C1
    (fun st _ ->
      let l =
        List.filter !file_descrs ~f:(function
            | _, (`o _, _) -> true
            | _ -> false)
      in
      let l = List.map l ~f:(fun (_, (_, p)) -> p) in
      Ok (mk_list st l))
;;

let caml_ml_output_char =
  C2
    (fun st chan c ->
      output_char (find_chan_out st chan) (Char.of_int_exn (Int64.to_int_exn (int_val c)));
      Ok val_unit)
;;

let caml_ml_flush =
  C1
    (fun st chan ->
      let chan = find_chan_out st chan in
      flush chan;
      Ok val_unit)
;;

let string_length st v =
  assert (Int64.equal string_tag (tag (header st v)));
  let size = Int64.to_int_exn @@ size (header st v) in
  let pad = Int64.to_int_exn @@ Int64.shift_right_logical (field st v (size - 1)) 56 in
  Int64.of_int @@ ((size * 8) - pad - 1)
;;

let caml_ml_string_length = C1 (fun st v -> Ok (val_int @@ string_length st v))
let get_string st v = (get_obj st v : string)

let caml_ml_output =
  C4
    (fun st chan bytes ofs len ->
      let str = get_string st bytes in
      output_substring
        (find_chan_out st chan)
        str
        (Int64.to_int_exn @@ int_val ofs)
        (Int64.to_int_exn @@ int_val len);
      Ok val_unit)
;;

let caml_ml_input_scan_line =
  C1
    (fun st fd ->
      let chan = find_chan_in st fd in
      let pos = pos_in chan in
      let rec f n =
        try if Char.equal (input_char chan) '\n' then n + 1 else f (n + 1) with
        | _ -> -n
      in
      let n = f 0 in
      let () = seek_in chan pos in
      (* restore file pos (I think) *)
      Ok (val_int (Int64.of_int n)))
;;

let caml_ml_seek_in =
  C2
    (fun st chan pos ->
      try
        seek_in (find_chan_in st chan) (Int64.to_int_exn (int_val pos));
        Ok val_unit
      with
      | _ -> caml_raise_sys_error' st "caml_ml_seek_in")
;;

let caml_ml_seek_out =
  C2
    (fun st chan pos ->
      try
        seek_out (find_chan_out st chan) (Int64.to_int_exn (int_val pos));
        Ok val_unit
      with
      | _ -> caml_raise_sys_error' st "caml_ml_seek_out")
;;

let set_byte' st s ofs b =
  let w = field st s (ofs / 8) in
  let sft = Int64.of_int (ofs land 7 * 8) in
  let mask = ~:(sll 255L sft) in
  let b = sll (b &: 255L) sft in
  set_field st s (ofs / 8) (w &: mask |: b)
;;

let set_byte st s ofs b = set_byte' st s ofs (Int64.of_int (Char.to_int b))

let get_byte st s ofs =
  let w = field st s (ofs / 8) in
  let sft = Int64.of_int (ofs land 7 * 8) in
  srl w sft &: 255L
;;

let caml_string_get =
  C2
    (fun st s i ->
      let i = int_val i in
      if Int64.compare i 0L < 0 || Int64.compare i (string_length st s) >= 0
      then caml_array_bound_error st
      else Ok (val_int (get_byte st s (Int64.to_int_exn i))))
;;

let caml_string_set =
  C3
    (fun st s i b ->
      let i = int_val i in
      if Int64.compare i 0L < 0 || Int64.compare i (string_length st s) >= 0
      then caml_array_bound_error st
      else (
        set_byte' st s (Int64.to_int_exn i) b;
        Ok val_unit))
;;

let caml_ml_input =
  C4
    (fun st ic s ofs len ->
      let ofs = Int64.to_int_exn (int_val ofs) in
      let len = Int64.to_int_exn (int_val len) in
      let b = Bytes.create len in
      let ichan = find_chan_in st ic in
      let len = input ichan b 0 len in
      for i = 0 to len - 1 do
        set_byte st s (ofs + i) (Bytes.get b i)
      done;
      Ok (val_int (Int64.of_int len)))
;;

let caml_ml_input_char =
  C1
    (fun st ic ->
      let chan = find_chan_in st ic in
      let ch = Char.to_int (input_char chan) in
      Ok (val_int (Int64.of_int ch)))
;;

let caml_ml_input_int =
  C1
    (fun st ic ->
      let chan = find_chan_in st ic in
      let i = input_binary_int chan in
      Ok (val_int (Int64.of_int i)))
;;

let caml_input_value =
  (*let cnt = ref 0 in*)
  C1
    (fun st ic ->
      let chan = find_chan_in st ic in
      let p = alloc_block_from st (input_value chan) in
      (*let f = open_out ("value.txt" ^ string_of_int !cnt) in
    let () = Trace.root f st.memory p in
    let () = close_out f; incr cnt in*)
      Ok p)
;;

let caml_ml_channel_size =
  C1
    (fun st chan ->
      let x i = Ok (val_int (Int64.of_int i)) in
      try x (in_channel_length (find_chan_in st chan)) with
      | _ ->
        (try x (out_channel_length (find_chan_out st chan)) with
        | _ -> caml_raise_sys_error' st "caml_ml_channel_size"))
;;

(* XXX has this been removed from the runtime? *)
(* external is_printable : char -> bool = "caml_is_printable"
 * 
 * let caml_is_printable =
 *   C1
 *     (fun _ c ->
 *       Ok
 *         ( if is_printable (Char.chr (to_int (int_val c))) then val_true
 *         else val_false )) *)

let caml_is_printable =
  C1
    (fun _ c ->
      Ok
        (if Char.is_print (Char.of_int_exn (Int64.to_int_exn (int_val c)))
        then val_true
        else val_false))
;;

let caml_sys_file_exists =
  C1
    (fun st name ->
      Ok (if Caml.Sys.file_exists (get_obj st name : string) then val_true else val_false))
;;

external open_desc : string -> open_flag list -> int -> int = "caml_sys_open"

let caml_sys_open =
  C3
    (fun st fname mode perm ->
      let fname = get_string st fname in
      let mode = (get_obj st mode : open_flag list) in
      let perm = Int64.to_int_exn (int_val perm) in
      (*Printf.printf "sys_open %s, mode=%i, perm=%x\n"
      fname (List.length mode) perm;*)
      let fd = open_desc fname mode perm in
      Ok (val_int (Int64.of_int fd)))
;;

let caml_create_string =
  C1
    (fun st len ->
      let x = String.make (Int64.to_int_exn (int_val len)) (Char.of_int_exn 0) in
      Ok (alloc_block_from st x))
;;

let caml_blit_string =
  C5
    (fun st s1 ofs1 s2 ofs2 n ->
      let n = Int64.to_int_exn (int_val n) in
      let ofs1 = Int64.to_int_exn (int_val ofs1) in
      let ofs2 = Int64.to_int_exn (int_val ofs2) in
      for i = 0 to n - 1 do
        let b = Char.of_int_exn @@ Int64.to_int_exn @@ get_byte st s1 (ofs1 + i) in
        set_byte st s2 (ofs2 + i) b
      done;
      Ok val_unit)
;;

let caml_fill_string =
  C4
    (fun st s ofs len v ->
      let ofs = Int64.to_int_exn (int_val ofs) in
      let len = Int64.to_int_exn (int_val len) in
      for i = 0 to len - 1 do
        set_byte st s (ofs + i) (Char.of_int_exn (Int64.to_int_exn (int_val v)))
      done;
      Ok val_unit)
;;

let string_compare st s1 s2 =
  let s1 = (get_obj st s1 : string) in
  let s2 = (get_obj st s2 : string) in
  String.compare s1 s2
;;

let caml_string_equal =
  C2 (fun st s1 s2 -> Ok (if string_compare st s1 s2 = 0 then val_true else val_false))
;;

let caml_string_notequal =
  C2 (fun st s1 s2 -> Ok (if string_compare st s1 s2 <> 0 then val_true else val_false))
;;

let caml_string_compare =
  C2 (fun st s1 s2 -> Ok (val_int (Int64.of_int (string_compare st s1 s2))))
;;

let argv = ref ("hardcamlzinc", [| "hardcamlzinc" |])
let caml_sys_get_argv = C1 (fun st _ -> Ok (alloc_block_from st !argv))
let caml_sys_get_config = C1 (fun st _ -> Ok (alloc_block_from st ("Unix", 64, false)))

let caml_sys_getenv =
  C1
    (fun st v ->
      let v = get_string st v in
      match Sys.getenv v with
      | Some e -> Ok (alloc_block_from st e)
      | None -> caml_raise_not_found st)
;;

let caml_make_vect =
  C2
    (fun st len init ->
      let len = int_val len in
      if Int64.equal len 0L
      then Ok st.atom_table
      else (
        let tag =
          if Int64.equal (is_block init) 1L
             && Int64.equal (tag (header st init)) double_tag
          then double_array_tag
          else 0L
        in
        let p = alloc_block ~st ~size:len ~tag ~colour:white in
        for i = 0 to Int64.to_int_exn len - 1 do
          set_field st p i init
        done;
        Ok p))
;;

let caml_array_get_addr st a idx =
  let idx = int_val idx in
  let size = size (header st a) in
  if Int64.compare idx size >= 0 || Int64.compare idx 0L < 0
  then caml_array_bound_error st
  else Ok (field st a (Int64.to_int_exn idx))
;;

let caml_array_get_float st a idx =
  let idx = int_val idx in
  let size = size (header st a) in
  if Int64.compare idx size >= 0 || Int64.compare idx 0L < 0
  then caml_array_bound_error st
  else (
    let p = alloc_block ~st ~size:1L ~colour:white ~tag:double_tag in
    set_field st p 0 (field st a (Int64.to_int_exn idx));
    Ok p)
;;

let caml_array_get =
  C2
    (fun st a idx ->
      if Int64.equal (tag (header st a)) double_array_tag
      then caml_array_get_float st a idx
      else caml_array_get_addr st a idx)
;;

let caml_array_set_addr st a idx v =
  let idx = int_val idx in
  let size = size (header st a) in
  if Int64.compare idx size >= 0 || Int64.compare idx 0L < 0
  then caml_array_bound_error st
  else (
    modify st a (Int64.to_int_exn idx) v;
    Ok val_unit)
;;

let caml_array_set_float st a idx v =
  let idx = int_val idx in
  let size = size (header st a) in
  if Int64.compare idx size >= 0 || Int64.compare idx 0L < 0
  then caml_array_bound_error st
  else (
    set_field st a (Int64.to_int_exn idx) (field st v 0);
    Ok val_unit)
;;

let caml_array_set =
  C3
    (fun st a idx v ->
      if Int64.equal (tag (header st a)) double_array_tag
      then caml_array_set_float st a idx v
      else caml_array_set_addr st a idx v)
;;

let caml_array_sub =
  C3
    (fun st a ofs len ->
      let ofs = int_val ofs in
      let len = int_val len in
      let size = size (header st a) in
      if Int64.compare ofs 0L < 0
         || Int64.compare len 0L < 0
         || Int64.compare (ofs +: len) size > 0
      then caml_invalid_argument' st "Array.sub"
      else if Int64.equal len 0L
      then Ok st.atom_table
      else (
        let p = alloc_block ~st ~size:len ~tag:0L ~colour:white in
        for i = 0 to Int64.to_int_exn len - 1 do
          set_field st p i (field st a (Int64.to_int_exn ofs + i))
        done;
        Ok p))
;;

let caml_array_blit =
  C5
    (fun st src sofs dst dofs len ->
      let sofs = Int64.to_int_exn (int_val sofs) in
      let dofs = Int64.to_int_exn (int_val dofs) in
      let len = Int64.to_int_exn (int_val len) in
      if dofs < sofs
      then
        for i = 0 to len - 1 do
          set_field st dst (i + dofs) (field st src (i + sofs))
        done
      else
        for i = len - 1 downto 0 do
          set_field st dst (i + dofs) (field st src (i + sofs))
        done;
      Ok val_unit)
;;

let caml_obj_block =
  C2
    (fun st tag size ->
      let size = int_val size in
      let tag = int_val tag in
      if Int64.equal size 0L
      then Ok (st.atom_table +: aofs tag)
      else (
        let p = alloc_block ~st ~size ~tag ~colour:white in
        for i = 0 to Int64.to_int_exn size - 1 do
          set_field st p i (val_int 0L)
        done;
        Ok p))
;;

let caml_obj_dup =
  C1
    (fun st obj ->
      let size = size (header st obj) in
      if Int64.equal size 0L
      then Ok obj
      else (
        let tag = tag (header st obj) in
        let p = alloc_block ~st ~size ~tag ~colour:white in
        for i = 0 to Int64.to_int_exn size - 1 do
          set_field st p i (field st obj i)
        done;
        Ok p))
;;

let caml_obj_tag =
  C1
    (fun st obj ->
      if Int64.equal (is_int obj) 1L
      then Ok (val_int 1000L)
      else if not (Int64.equal (obj &: 7L) 0L)
      then Ok (val_int 1002L) (* else if obj < st.heap then Ok (val_int 1001L) *)
      else Ok (val_int (tag (header st obj))))
;;

let caml_obj_set_tag =
  C2
    (fun st arg tag ->
      let hdr = header st arg in
      set_header st arg (hdr &: ~:255L |: int_val tag);
      Ok val_unit)
;;

let caml_compare_val st a b =
  (* need better implementation *)
  let (a : Obj.t), (b : Obj.t) = get_obj st a, get_obj st b in
  Poly.compare a b
;;

let caml_hash =
  C4
    (fun st _ _ _ a ->
      let (a : Obj.t) = get_obj st a in
      let x = Hashtbl.hash a in
      Ok (val_int (Int64.of_int x)))
;;

(* capture and return possible exceptions *)
let caml_compare st a b = Ok (val_int (Int64.of_int (caml_compare_val st a b)))

let caml_equal st a b =
  Ok (val_int (Int64.of_int (if caml_compare_val st a b = 0 then 1 else 0)))
;;

let caml_notequal st a b =
  Ok (val_int (Int64.of_int (if caml_compare_val st a b <> 0 then 1 else 0)))
;;

let caml_lessthan st a b =
  Ok (val_int (Int64.of_int (if caml_compare_val st a b < 0 then 1 else 0)))
;;

let caml_lessequal st a b =
  Ok (val_int (Int64.of_int (if caml_compare_val st a b <= 0 then 1 else 0)))
;;

let caml_greaterthan st a b =
  Ok (val_int (Int64.of_int (if caml_compare_val st a b > 0 then 1 else 0)))
;;

let caml_greaterequal st a b =
  Ok (val_int (Int64.of_int (if caml_compare_val st a b >= 0 then 1 else 0)))
;;

let caml_weak_list_head = ref 0L
let caml_weak_none = 0L (* XXX ??? *)

let caml_weak_create =
  C1
    (fun st len ->
      let size = int_val len +: 1L in
      if Int64.compare size 0L <= 0 (*|| size > max_wosize*)
      then caml_invalid_argument' st "Weak.create"
      else (
        let p = alloc_block ~st ~size ~colour:white ~tag:abstract_tag in
        set_field st p 0 !caml_weak_list_head;
        caml_weak_list_head := p;
        for i = 1 to Int64.to_int_exn size - 1 do
          set_field st p i caml_weak_none
        done;
        Ok p))
;;

let caml_dynlink_get_current_libs = C1 (fun st _ -> Ok st.atom_table)

external format_int : string -> int -> string = "caml_format_int"

let caml_format_int =
  C2
    (fun st fmt arg ->
      let s = format_int (get_obj st fmt : string) (get_obj st arg : int) in
      Ok (alloc_block_from st s))
;;

let update_fields : state -> int64 -> 'a -> unit =
 fun st p o ->
  let o = Obj.repr o in
  for i = 0 to Obj.size o - 1 do
    set_field st p i (Repr.int64_of_obj (Obj.field o i))
  done
;;

(*
 
  This is input to the c-call and some fields are updated.
  note; we cannot convert functions across the c-call boundary, but refill_buff
  is not used anyway.

      type lexbuf =
        { refill_buff : lexbuf -> unit;     0 not used 
          mutable lex_buffer : bytes;       1 fields modified
          mutable lex_buffer_len : int;     2 read only
          mutable lex_abs_pos : int;        3 not used
          mutable lex_start_pos : int;      4 modified
          mutable lex_curr_pos : int;       5 modified
          mutable lex_last_pos : int;       6 modified
          mutable lex_last_action : int;    7 modified
          mutable lex_eof_reached : bool;   8 modified
          mutable lex_mem : int array;      9 fields modified
          mutable lex_start_p : position;  10 not used
          mutable lex_curr_p : position;   11 not used
        }
*)
external lex_c_engine
  :  Lexing.lex_tables
  -> int
  -> Lexing.lexbuf
  -> int
  = "caml_lex_engine"

external new_lex_c_engine
  :  Lexing.lex_tables
  -> int
  -> Lexing.lexbuf
  -> int
  = "caml_new_lex_engine"

let caml_lex_engine' lex_engine =
  C3
    (fun st tab start buf ->
      let tab' = (get_obj st tab : Lexing.lex_tables) in
      (* might be worth cacheing this? *)
      let start = Int64.to_int_exn (int_val start) in
      let buf' = (get_obj ~closure:false st buf : Lexing.lexbuf) in
      let res = lex_engine tab' start buf' in
      update_fields st (field st buf 1) buf'.Lexing.lex_buffer;
      set_field st buf 4 (val_int (Int64.of_int buf'.Lexing.lex_start_pos));
      set_field st buf 5 (val_int (Int64.of_int buf'.Lexing.lex_curr_pos));
      set_field st buf 6 (val_int (Int64.of_int buf'.Lexing.lex_last_pos));
      set_field st buf 7 (val_int (Int64.of_int buf'.Lexing.lex_last_action));
      set_field st buf 8 (if buf'.Lexing.lex_eof_reached then val_true else val_false);
      update_fields st (field st buf 9) buf'.Lexing.lex_mem;
      Ok (val_int (Int64.of_int res)))
;;

let caml_lex_engine = caml_lex_engine' lex_c_engine
let caml_new_lex_engine = caml_lex_engine' new_lex_c_engine

type position = Lexing.position

type parser_env =
  { mutable s_stack : int array
  ; (*   0 fields modified *)
    mutable v_stack : Obj.t array
  ; (*   1 fields modified *)
    mutable symb_start_stack : position array
  ; (*   2 fields modified *)
    mutable symb_end_stack : position array
  ; (*   3 fields modified *)
    mutable stacksize : int
  ; (*   4 read only *)
    mutable stackbase : int
  ; (*   5 read only *)
    mutable curr_char : int
  ; (*   6 modified  *)
    mutable lval : Obj.t
  ; (*   7 modified - xxx *)
    mutable symb_start : position
  ; (*   8 read only *)
    mutable symb_end : position
  ; (*   9 read only *)
    mutable asp : int
  ; (*  10 modified *)
    mutable rule_len : int
  ; (*  11 modified *)
    mutable rule_number : int
  ; (*  12 modified *)
    mutable sp : int
  ; (*  13 modified *)
    mutable state : int
  ; (*  14 modified *)
    mutable errflag : int
  }

(*  15 modified *)

external parse_engine
  :  Parsing.parse_tables
  -> parser_env
  -> int
  -> Obj.t
  -> int
  = "caml_parse_engine"

let caml_parse_engine =
  C4
    (fun st tab env inp obj ->
      let tab' = (get_obj ~closure:false st tab : Parsing.parse_tables) in
      let env' = (get_obj st env : parser_env) in
      let inp' = (get_obj st inp : int) in
      let obj' = (get_obj st obj : Obj.t) in
      (* not modified *)
      let res = parse_engine tab' env' inp' obj' in
      update_fields st (field st env 0) env'.s_stack;
      update_fields st (field st env 1) env'.v_stack;
      update_fields st (field st env 2) env'.symb_start_stack;
      update_fields st (field st env 3) env'.symb_end_stack;
      set_field st env 6 (val_int (Int64.of_int env'.curr_char));
      set_field st env 7 (alloc_block_from st env'.lval);
      (* I think this is code... *)
      set_field st env 10 (val_int (Int64.of_int env'.asp));
      set_field st env 11 (val_int (Int64.of_int env'.rule_len));
      set_field st env 12 (val_int (Int64.of_int env'.rule_number));
      set_field st env 13 (val_int (Int64.of_int env'.sp));
      set_field st env 14 (val_int (Int64.of_int env'.state));
      set_field st env 15 (val_int (Int64.of_int env'.errflag));
      Ok (val_int (Int64.of_int res)))
;;

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
  include Caml.Nativeint

  let name = "nativeint"
  let to_t = Int64.to_nativeint_exn
end

module Int64_c_ops = struct
  include Caml.Nativeint

  let name = "nativeint"
  let to_t = Int64.to_nativeint_exn
end

module Int32_c_ops = struct
  include Caml.Nativeint

  let name = "nativeint"
  let to_t = Int64.to_nativeint_exn
end

module Int_c_calls (I : Int) = struct
  let op2 f =
    C2
      (fun st a b ->
        let p = alloc_block ~st ~size:2L ~tag:custom_tag ~colour:white in
        let c = f (I.to_t a) (I.to_t b) in
        set_field st p 0 0L;
        (* XXX should point to custom allocation block... *)
        set_field st p 1 (Repr.int64_of_obj Obj.(field (repr c) 1));
        Ok p)
  ;;

  let op1 f =
    C1
      (fun st a ->
        let p = alloc_block ~st ~size:2L ~tag:custom_tag ~colour:white in
        let c = f (I.to_t a) in
        set_field st p 0 0L;
        (* XXX should point to custom allocation block... *)
        set_field st p 1 (Repr.int64_of_obj Obj.(field (repr c) 1));
        Ok p)
  ;;

  let sftop f =
    C2
      (fun st a b ->
        let p = alloc_block ~st ~size:2L ~tag:custom_tag ~colour:white in
        let c = f (I.to_t a) (Int64.to_int_exn (int_val b)) in
        set_field st p 0 0L;
        (* XXX should point to custom allocation block... *)
        set_field st p 1 (Repr.int64_of_obj Obj.(field (repr c) 1));
        Ok p)
  ;;

  let c_calls =
    [ "caml_" ^ I.name ^ "_add", op2 I.add
    ; "caml_" ^ I.name ^ "_and", op2 I.logand
    ; "caml_" ^ I.name ^ "_div", op2 I.div
    ; "caml_" ^ I.name ^ "_mod", op2 I.rem
    ; "caml_" ^ I.name ^ "_mul", op2 I.mul
    ; "caml_" ^ I.name ^ "_neg", op1 I.neg
    ; "caml_" ^ I.name ^ "_or", op2 I.logor
    ; "caml_" ^ I.name ^ "_shift_left", sftop I.shift_left
    ; "caml_" ^ I.name ^ "_shift_right", sftop I.shift_right
    ; "caml_" ^ I.name ^ "_shift_right_unsigned", sftop I.shift_right_logical
    ; "caml_" ^ I.name ^ "_sub", op2 I.sub
    ; "caml_" ^ I.name ^ "_xor", op2 I.logxor
    ]
  ;;
end

module Nativeint_c_calls = Int_c_calls (Nativeint_c_ops)
module Int64_c_calls = Int_c_calls (Int64_c_ops)
module Int32_c_calls = Int_c_calls (Int32_c_ops)

let c_calls =
  [ "caml_alloc_dummy", caml_alloc_dummy
  ; "caml_update_dummy", caml_update_dummy
  ; "caml_register_named_value", c2_unit
  ; "caml_set_oo_id", caml_set_oo_id
  ; "caml_fresh_oo_id", caml_fresh_oo_id
  ; "caml_get_section_table", caml_get_section_table
  ; "caml_int64_float_of_bits", caml_int64_float_of_bits
  ; "caml_ml_open_descriptor_in", caml_ml_open_descriptor_in
  ; "caml_ml_open_descriptor_out", caml_ml_open_descriptor_out
  ; "caml_ml_close_channel", caml_ml_close_channel
  ; "caml_ml_output_char", caml_ml_output_char
  ; "caml_ml_string_length", caml_ml_string_length
  ; "caml_ml_output", caml_ml_output
  ; "caml_ml_out_channels_list", caml_ml_out_channels_list
  ; "caml_ml_flush", caml_ml_flush
  ; "caml_ml_input_scan_line", caml_ml_input_scan_line
  ; "caml_ml_input", caml_ml_input
  ; "caml_ml_input_char", caml_ml_input_char
  ; "caml_ml_input_int", caml_ml_input_int
  ; "caml_input_value", caml_input_value
  ; "caml_ml_channel_size", caml_ml_channel_size
  ; "caml_is_printable", caml_is_printable
  ; "caml_ml_seek_in", caml_ml_seek_in
  ; "caml_ml_seek_out", caml_ml_seek_out
  ; "caml_create_string", caml_create_string
  ; "caml_blit_string", caml_blit_string
  ; "caml_fill_string", caml_fill_string
  ; "caml_string_equal", caml_string_equal
  ; "caml_string_notequal", caml_string_notequal
  ; "caml_string_compare", caml_string_compare
  ; "caml_string_get", caml_string_get
  ; "caml_string_set", caml_string_set
  ; "caml_sys_open", caml_sys_open
  ; "caml_sys_get_argv", caml_sys_get_argv
  ; "caml_sys_get_config", caml_sys_get_config
  ; "caml_sys_const_big_endian", c1_false
  ; "caml_sys_const_word_size", c1_int 64L
  ; "caml_sys_const_ostype_unix", c1_true
  ; "caml_sys_const_ostype_win32", c1_false
  ; "caml_sys_const_ostype_cygwin", c1_false
  ; "caml_sys_getenv", caml_sys_getenv
  ; "caml_sys_file_exists", caml_sys_file_exists
  ; "caml_array_get_addr", C2 caml_array_get_addr
  ; "caml_array_get_float", C2 caml_array_get_float
  ; "caml_array_get", caml_array_get
  ; "caml_array_set_addr", C3 caml_array_set_addr
  ; "caml_array_set_float", C3 caml_array_set_float
  ; "caml_array_set", caml_array_set
  ; "caml_array_unsafe_get_addr", C2 caml_array_get_addr
  ; "caml_array_unsafe_get_float", C2 caml_array_get_float
  ; "caml_array_unsafe_get", caml_array_get
  ; "caml_array_unsafe_set_addr", C3 caml_array_set_addr
  ; "caml_array_unsafe_set_float", C3 caml_array_set_float
  ; "caml_array_unsafe_set", caml_array_set
  ; "caml_array_sub", caml_array_sub
  ; "caml_array_blit", caml_array_blit
  ; "caml_make_vect", caml_make_vect
  ; "caml_obj_block", caml_obj_block
  ; "caml_obj_dup", caml_obj_dup
  ; "caml_obj_tag", caml_obj_tag
  ; "caml_obj_set_tag", caml_obj_set_tag
  ; "caml_compare", C2 caml_compare
  ; "caml_equal", C2 caml_equal
  ; "caml_notequal", C2 caml_notequal
  ; "caml_lessthan", C2 caml_lessthan
  ; "caml_lessequal", C2 caml_lessequal
  ; "caml_greaterthan", C2 caml_greaterthan
  ; "caml_greaterequal", C2 caml_greaterequal
  ; "caml_int_compare", C2 caml_compare
  ; "caml_hash", caml_hash
  ; "caml_weak_create", caml_weak_create
  ; "caml_ensure_stack_capacity", c1_unit
  ; "caml_dynlink_get_current_libs", caml_dynlink_get_current_libs
  ; "caml_format_int", caml_format_int
  ; "caml_install_signal_handler", c2_int 0L
  ; "caml_lex_engine", caml_lex_engine
  ; "caml_new_lex_engine", caml_new_lex_engine
  ; "caml_parse_engine", caml_parse_engine
  ]
  @ Nativeint_c_calls.c_calls
  @ Int64_c_calls.c_calls
  @ Int32_c_calls.c_calls
;;

let run bc prim st =
  let arg ofs = st.memory.{(Int64.to_int_exn st.sp / 8) + ofs} in
  match List.Assoc.find ~equal:String.equal c_calls bc.Load.prim.(prim) with
  | Some (C1 f) -> f st st.accu
  | Some (C2 f) -> f st st.accu (arg 1)
  | Some (C3 f) -> f st st.accu (arg 1) (arg 2)
  | Some (C4 f) -> f st st.accu (arg 1) (arg 2) (arg 3)
  | Some (C5 f) -> f st st.accu (arg 1) (arg 2) (arg 3) (arg 4)
  | Some CN -> failwith "C_CALLN not yet implemented"
  | None -> failwith ("C primitive " ^ bc.Load.prim.(prim) ^ " not found")
;;
