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

type std_exn = 
  | OUT_OF_MEMORY_EXN              (* "Out_of_memory" *)
  | SYS_ERROR_EXN                  (* "Sys_error" *)
  | FAILURE_EXN                    (* "Failure" *)
  | INVALID_EXN                    (* "Invalid_argument" *)
  | END_OF_FILE_EXN                (* "End_of_file" *)
  | ZERO_DIVIDE_EXN                (* "Division_by_zero" *)
  | NOT_FOUND_EXN                  (* "Not_found" *)
  | MATCH_FAILURE_EXN              (* "Match_failure" *)
  | STACK_OVERFLOW_EXN             (* "Stack_overflow" *)
  | SYS_BLOCKED_IO                 (* "Sys_blocked_io" *)
  | ASSERT_FAILURE_EXN             (* "Assert_failure" *)
  | UNDEFINED_RECURSIVE_MODULE_EXN (* "Undefined_recursive_module" *)
  deriving(Enum)

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

(* alplocate a block in the c-heap. *)
let alloc_block ~st ~size ~colour ~tag = 
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
  if size = 1 then a.(0) (* must be a scalar value *)
  else begin
    bump size;
    for i=0 to size - 1 do
      st.memory.{i+p} <- a.(i)
    done;
    Int64.of_int ((p+1)*8)
  end

let header st v = st.memory.{((to_int v) / 8)-1} 
let field st v i = st.memory.{((to_int v) / 8)+i} 
let set_field st v i d = st.memory.{((to_int v) / 8)+i} <- d
let modify = set_field

let c1_unit = C1 (fun _ _ -> `ok val_unit)
let c2_unit = C2 (fun _ _ _ -> `ok val_unit)
let c3_unit = C3 (fun _ _ _ _ -> `ok val_unit)
let c4_unit = C4 (fun _ _ _ _ _ -> `ok val_unit)
let c5_unit = C5 (fun _ _ _ _ _ _ -> `ok val_unit)

let c1_id = C1 (fun _ a -> `ok a)

let c1_int i = C1 (fun _ _ -> `ok (val_int i))
let c1_true = C1 (fun _ _ -> `ok val_true)
let c1_false = C1 (fun _ _ -> `ok val_false)

let caml_copy_string st s = 
  let size = size (header st s) in
  let p = alloc_block ~st ~size ~colour:white ~tag:string_tag in
  for i=0 to (to_int size)-1 do
    set_field st p i (field st s i)
  done;
  p

let caml_copy_string' st s = 
  let s = Obj.repr s in
  let size = Obj.size s in
  let p = alloc_block ~st ~size:(of_int size) ~colour:white ~tag:string_tag in
  for i=0 to size-1 do
    set_field st p i (Repr.int64_of_obj (Obj.field s i))
  done;
  p

(* exceptions (fail.c) *)
let caml_raise _ v = `exn v

let caml_raise_constant = caml_raise

let caml_raise_with_arg st tag arg = 
  let p = alloc_block ~st ~size:2L ~colour:white ~tag:0L in
  set_field st p 0 tag;
  set_field st p 1 arg;
  caml_raise st p

let caml_raise_with_string st tag s = 
  let s' = caml_copy_string st s in
  caml_raise_with_arg st tag s'

let caml_raise_with_string' st tag s = 
  let s' = caml_copy_string' st s in
  caml_raise_with_arg st tag s'

let exn_field st x = field st st.global_data (Enum.from_enum<std_exn> x)

let caml_failwith st s = caml_raise_with_string st (exn_field st FAILURE_EXN) s

let caml_invalid_argument st s = caml_raise_with_string st (exn_field st INVALID_EXN) s
let caml_invalid_argument' st s = caml_raise_with_string' st (exn_field st INVALID_EXN) s

let caml_array_bound_error st = caml_invalid_argument' st "index out of bounds"

let caml_raise_out_of_memory st = caml_raise_constant st (exn_field st OUT_OF_MEMORY_EXN)

let caml_raise_stack_overflow st = caml_raise_constant st (exn_field st STACK_OVERFLOW_EXN)

let caml_raise_sys_error st s = caml_raise_with_arg st (exn_field st SYS_ERROR_EXN) s
let caml_raise_sys_error' st s = caml_raise_with_string' st (exn_field st SYS_ERROR_EXN) s

let caml_raise_end_of_file st = caml_raise_constant st (exn_field st END_OF_FILE_EXN)

let caml_raise_zero_divide st = caml_raise_constant st (exn_field st ZERO_DIVIDE_EXN)

let caml_raise_not_found st = caml_raise_constant st (exn_field st NOT_FOUND_EXN)

let caml_raise_sys_blocked_io st = caml_raise_constant st (exn_field st SYS_BLOCKED_IO)

let caml_is_special_exception st exn = 
  exn = exn_field st MATCH_FAILURE_EXN ||
  exn = exn_field st ASSERT_FAILURE_EXN ||
  exn = exn_field st UNDEFINED_RECURSIVE_MODULE_EXN 

let aofs x = sll x 3L

let caml_alloc st size tag = 
  if size = 0L then st.atom_table +: (aofs tag)
  else
    let p = alloc_block ~st ~size ~colour:white ~tag in
    if (tag <+ no_scan_tag) = 1L then begin
      for i=0 to to_int size - 1 do
        set_field st p i val_unit
      done
    end;
    p

let caml_alloc_dummy = 
  C1 (fun st size -> `ok (caml_alloc st (int_val size) 0L))

let caml_update_dummy = 
  C2 (fun st dummy newval ->
    let hdummy = header st dummy in
    let hnewval = header st newval in
    assert (size hdummy = size hnewval);
    assert (tag newval < no_scan_tag || tag newval = double_array_tag);

    set_field st dummy (-1) (make_header (size hdummy) (colour hdummy) (tag hnewval));
    for i=0 to to_int (size hdummy) - 1 do
      modify st dummy i (field st newval i)
    done;
    `ok val_unit)

let caml_set_oo_id = 
  let c = ref 1L in
  C1 (fun st ptr ->
    set_field st ptr 1 !c;
    c := add !c 2L;
    `ok ptr)

let caml_get_section_table = 
  C1 (fun st _ -> caml_raise_not_found st)

let caml_int64_float_of_bits = 
  C1 (fun st v ->
    let p = alloc_block ~st ~size:1L ~colour:black ~tag:double_tag in
    set_field st p 0 (field st v 1);
    `ok p)

type chan_io = [ `i of in_channel | `o of out_channel ]
let file_descrs = ref []
let add_descr fd chan ptr = file_descrs := (fd,(chan,ptr))::!file_descrs
let remove_descr fd = file_descrs := List.filter (fun (fd',_) -> fd'<>fd) !file_descrs

let descr_block st fd = 
  let p = alloc_block ~st ~size:2L ~colour:white ~tag:custom_tag in (* int * int, fd is second field *)
  set_field st p 0 1L;
  set_field st p 1 fd;
  p

let get_descr x = (* XXX assumption; file_descr <=> int *) 
  (Obj.magic (Int64.to_int (int_val x)) : Unix.file_descr)

let find_chan st p = 
  try List.assoc (field st p 1) !file_descrs 
  with _ -> failwith ("in channel not found (" ^ Int64.to_string (field st p 1) ^ ")")

let unlink_chan st p = 
  let f = find_chan st p in
  remove_descr (field st p 1);
  f

let find_chan_in st p = match find_chan st p with `i f,_ -> f | _ -> raise Not_found
let find_chan_out st p = match find_chan st p with `o f,_ -> f | _ -> raise Not_found

let caml_ml_open_descriptor_in = 
  C1 (fun st fd -> 
    let fd' = get_descr fd in
    let p = descr_block st fd in
    add_descr fd (`i (Unix.in_channel_of_descr fd')) p;
    `ok p)

let caml_ml_open_descriptor_out = 
  C1 (fun st fd -> 
    let fd' = get_descr fd in
    let p = descr_block st fd in
    add_descr fd (`o (Unix.out_channel_of_descr fd')) p;
    `ok p)

let caml_ml_close_channel = 
  C1 (fun st fd ->
    (match unlink_chan st fd with (* raise exn? *)
    | `i f,_ -> close_in f
    | `o f,_ -> close_out f);
    `ok val_unit)

let mk_list st l = List.fold_right
  (fun p l -> 
    let x = alloc_block ~st ~size:2L ~tag:0L ~colour:white in
    set_field st x 0 p;
    set_field st x 1 l;
    x)
  l (val_int 0L) 

let caml_ml_out_channels_list = 
  C1 (fun st _ -> 
    let l = List.filter (function (_,(`o _,_)) -> true | _ -> false) !file_descrs in
    let l = List.map (fun (_,(_,p)) -> p) l in
    `ok (mk_list st l))

let caml_ml_output_char = 
  C2 (fun st chan c ->
    output_char (find_chan_out st chan) (Char.chr (to_int (int_val c)));
    `ok val_unit)

let caml_ml_flush = 
  C1 (fun st chan ->
    let chan = find_chan_out st chan in
    flush chan;
    `ok val_unit)

let string_length st v = 
    assert (string_tag = tag (header st v));
    let size = to_int @@ size (header st v) in
    let pad = to_int @@ shift_right_logical (field st v (size-1)) 56 in
    of_int @@ ((size * 8) - pad - 1)

let caml_ml_string_length =
  C1 (fun st v -> `ok (val_int @@ string_length st v))

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

let caml_ml_seek_in =
  C2 (fun st chan pos -> 
    try seek_in (find_chan_in st chan) (to_int (int_val pos)); `ok val_unit
    with _ -> caml_raise_sys_error' st "caml_ml_seek_in")

let caml_ml_seek_out =
  C2 (fun st chan pos -> 
    try seek_out (find_chan_out st chan) (to_int (int_val pos)); `ok val_unit
    with _ -> caml_raise_sys_error' st "caml_ml_seek_out")

let set_byte' st s ofs b = 
  let w = field st s (ofs / 8) in
  let sft = of_int ((ofs land 7) * 8)  in
  let mask = ~: (sll 255L sft) in
  let b = sll (b &: 255L) sft in
  set_field st s  (ofs / 8) ((w &: mask) |: b)

let set_byte st s ofs b = set_byte' st s ofs (Int64.of_int (Char.code b))

let get_byte st s ofs = 
  let w = field st s (ofs / 8) in
  let sft = of_int ((ofs land 7) * 8)  in
  (srl w sft) &: 255L 

let caml_string_get = 
  C2 (fun st s i ->
    let i = int_val i in
    if i < 0L || i >= string_length st s then caml_array_bound_error st
    else `ok (val_int (get_byte st s (to_int i))))

let caml_string_set = 
  C3 (fun st s i b -> 
    let i = int_val i in
    if i < 0L || i >= string_length st s then caml_array_bound_error st
    else begin 
      set_byte' st s (to_int i) b; 
      `ok val_unit
    end)

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

let caml_ml_input_int = 
  C1 (fun st ic ->
    let chan = find_chan_in st ic in
    let i = input_binary_int chan in
    `ok (val_int (of_int i)))

let caml_input_value = 
  (*let cnt = ref 0 in*)
  C1 (fun st ic ->
    let chan = find_chan_in st ic in
    let p = alloc_block_from st (input_value chan) in
    (*let f = open_out ("value.txt" ^ string_of_int !cnt) in
    let () = Trace.root f st.memory p in
    let () = close_out f; incr cnt in*)
    `ok p)

let caml_ml_channel_size = 
  C1 (fun st chan ->
    let x i = `ok (val_int (of_int i)) in
    try x (in_channel_length (find_chan_in st chan))
    with _ -> 
      try x (out_channel_length (find_chan_out st chan))
      with _ -> caml_raise_sys_error' st "caml_ml_channel_size")

external is_printable: char -> bool = "caml_is_printable"

let caml_is_printable = 
  C1 (fun st c -> 
    `ok (if is_printable (Char.chr (to_int (int_val c))) then val_true else val_false))

let caml_sys_file_exists = 
  C1 (fun st name ->
    `ok (if Sys.file_exists (get_obj st name : string) then val_true else val_false))

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

let caml_fill_string = 
  C4 (fun st s ofs len v -> 
    let ofs = to_int (int_val ofs) in
    let len = to_int (int_val len) in
    for i=0 to len-1 do
      set_byte st s (ofs+i) (Char.chr (to_int (int_val v)))
    done;
    `ok val_unit)

let string_compare st s1 s2 = 
  let s1 = (get_obj st s1 : string) in
  let s2 = (get_obj st s2 : string) in
  Pervasives.compare s1 s2

let caml_string_equal = 
  C2 (fun st s1 s2 -> 
    `ok (if string_compare st s1 s2 = 0 then val_true else val_false))

let caml_string_notequal =
  C2 (fun st s1 s2 -> 
  `ok (if string_compare st s1 s2 <> 0 then val_true else val_false))

let caml_string_compare =
  C2 (fun st s1 s2 -> `ok (val_int (of_int (string_compare st s1 s2))))

let argv = ref ("hardcamlzinc", [|"hardcamlzinc"|])

let caml_sys_get_argv = 
  C1 (fun st _ ->
    `ok (alloc_block_from st !argv))

let caml_sys_get_config = 
  C1 (fun st _ -> `ok (alloc_block_from st ("Unix", 64, false)))

let caml_sys_getenv = 
  C1 (fun st v ->
    let v = get_string st v in
    try `ok (alloc_block_from st (Sys.getenv v))
    with Not_found -> caml_raise_not_found st)

let caml_make_vect =
  C2 (fun st len init ->
    let len = int_val len in
    if len = 0L then `ok st.atom_table
    else
      let tag = 
        if is_block init = 1L && tag (header st init) = double_tag then double_array_tag 
        else 0L
      in
      let p = alloc_block ~st ~size:len ~tag ~colour:white in
      for i=0 to (to_int len)-1 do
        set_field st p i init
      done;
      `ok p)

let caml_array_get_addr = 
  (fun st a idx ->
    let idx = int_val idx in
    let size = size (header st a) in
    if idx >= size || idx < 0L then caml_array_bound_error st
    else `ok (field st a (to_int idx)))

let caml_array_get_float =
  (fun st a idx ->
    let idx = int_val idx in
    let size = size (header st a) in
    if idx >= size || idx < 0L then caml_array_bound_error st
    else
      let p = alloc_block ~st ~size:1L ~colour:white ~tag:double_tag in
      set_field st p 0 (field st a (to_int idx));
      `ok p)

let caml_array_get = 
  C2 (fun st a idx ->
    if tag (header st a) = double_array_tag then caml_array_get_float st a idx
    else caml_array_get_addr st a idx)

let caml_array_set_addr = 
  (fun st a idx v ->
    let idx = int_val idx in
    let size = size (header st a) in
    if idx >= size || idx < 0L then caml_array_bound_error st
    else begin 
      modify st a (to_int idx) v;
      `ok val_unit
    end)

let caml_array_set_float =
  (fun st a idx v ->
    let idx = int_val idx in
    let size = size (header st a) in
    if idx >= size || idx < 0L then caml_array_bound_error st
    else begin
      set_field st a (to_int idx) (field st v 0);
      `ok val_unit
    end)

let caml_array_set = 
  C3 (fun st a idx v ->
    if tag (header st a) = double_array_tag then caml_array_set_float st a idx v
    else caml_array_set_addr st a idx v)

let caml_array_sub = 
  C3 (fun st a ofs len ->
    let ofs = int_val ofs in
    let len = int_val len in
    let size = size (header st a) in
    if ofs < 0L || len < 0L || ofs +: len > size then 
      caml_invalid_argument' st "Array.sub"
    else if len = 0L then `ok st.atom_table
    else begin
      let p = alloc_block ~st ~size:len ~tag:0L ~colour:white in
      for i=0 to to_int len - 1 do
        set_field st p i (field st a (to_int ofs + i))
      done;
      `ok p
    end)

let caml_array_blit =
  C5 (fun st src sofs dst dofs len ->
    let sofs = to_int (int_val sofs) in
    let dofs = to_int (int_val dofs) in
    let len = to_int (int_val len) in
    (if dofs < sofs then
      for i=0 to len-1 do
        set_field st dst (i+dofs) (field st src (i+sofs))
      done
    else
      for i=len-1 downto 0 do
        set_field st dst (i+dofs) (field st src (i+sofs))
      done);
    `ok val_unit
  )

let caml_obj_block =
  C2 (fun st tag size -> 
    let size = int_val size in
    let tag = int_val tag in
    if size=0L then `ok (st.atom_table +: aofs tag)
    else
      let p = alloc_block ~st ~size ~tag ~colour:white in
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
      let p = alloc_block ~st ~size ~tag ~colour:white in
      for i=0 to (to_int size)-1 do
        set_field st p i (field st obj i)
      done;
      `ok p)

let caml_obj_tag = 
  C1 (fun st obj ->
    if is_int obj = 1L then `ok (val_int 1000L)
    else if obj &: 7L <> 0L then `ok (val_int 1002L)
    (* else if obj < st.heap then `ok (val_int 1001L) *)
    else `ok (val_int (tag (header st obj))))

let caml_compare_val st a b = (* need better implementation *)
  let (a : Obj.t), (b : Obj.t) = get_obj st a, get_obj st b in
  Pervasives.compare a b

let caml_hash = 
  C4 (fun st _ _ _ a -> 
    let (a : Obj.t) = get_obj st a in
    let x = Hashtbl.hash a in
    `ok (val_int (of_int x)))

(* capture and return possible exceptions *)
let caml_compare st a b = `ok (val_int (of_int (caml_compare_val st a b)))
let caml_equal st a b = `ok (val_int (of_int (if caml_compare_val st a b = 0 then 1 else 0)))
let caml_notequal st a b = `ok (val_int (of_int (if caml_compare_val st a b <> 0 then 1 else 0)))
let caml_lessthan st a b = `ok (val_int (of_int (if caml_compare_val st a b < 0 then 1 else 0)))
let caml_lessequal st a b = `ok (val_int (of_int (if caml_compare_val st a b <= 0 then 1 else 0)))
let caml_greaterthan st a b = `ok (val_int (of_int (if caml_compare_val st a b > 0 then 1 else 0)))
let caml_greaterequal st a b = `ok (val_int (of_int (if caml_compare_val st a b >= 0 then 1 else 0)))

let caml_weak_list_head = ref 0L
let caml_weak_none = 0L (* XXX ??? *)

let caml_weak_create = 
  C1 (fun st len ->
    let size = (int_val len) +: 1L in
    if size <= 0L (*|| size > max_wosize*) then caml_invalid_argument' st "Weak.create"
    else
      let p = alloc_block ~st ~size ~colour:white ~tag:abstract_tag in
      set_field st p 0 !caml_weak_list_head;
      caml_weak_list_head := p;
      for i=1 to to_int size - 1 do
        set_field st p i caml_weak_none
      done;
      `ok p)

let caml_dynlink_get_current_libs =
  C1 (fun st _ -> `ok st.atom_table)

external format_int : string -> int -> string = "caml_format_int"

let caml_format_int =
  C2 (fun st fmt arg ->
    let s = format_int (get_obj st fmt : string) (get_obj st arg : int) in
    `ok (alloc_block_from st s))

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
      let p = alloc_block ~st ~size:2L ~tag:custom_tag ~colour:white in
      let c = f (I.to_t a) (I.to_t b) in
      set_field st p 0 0L; (* XXX should point to custom allocation block... *)
      set_field st p 1 (Repr.int64_of_obj Obj.(field (repr c) 1));
      `ok p)

  let op1 f =
    C1 (fun st a ->
      let p = alloc_block ~st ~size:2L ~tag:custom_tag ~colour:white in
      let c = f (I.to_t a) in
      set_field st p 0 0L; (* XXX should point to custom allocation block... *)
      set_field st p 1 (Repr.int64_of_obj Obj.(field (repr c) 1));
      `ok p)

  let sftop f =
    C2 (fun st a b ->
      let p = alloc_block ~st ~size:2L ~tag:custom_tag ~colour:white in
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
  "caml_alloc_dummy", caml_alloc_dummy;
  "caml_update_dummy", caml_update_dummy;
  "caml_register_named_value", c2_unit;
  "caml_set_oo_id", caml_set_oo_id;
  "caml_get_section_table", caml_get_section_table;
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
  "caml_ml_input_int", caml_ml_input_int;
  "caml_input_value", caml_input_value;
  "caml_ml_channel_size", caml_ml_channel_size;
  "caml_is_printable", caml_is_printable;
  "caml_ml_seek_in", caml_ml_seek_in;
  "caml_ml_seek_out", caml_ml_seek_out;
  "caml_create_string", caml_create_string;
  "caml_blit_string", caml_blit_string;
  "caml_fill_string", caml_fill_string;
  "caml_string_equal", caml_string_equal;
  "caml_string_notequal", caml_string_notequal;
  "caml_string_compare", caml_string_compare;
  "caml_string_get", caml_string_get;
  "caml_string_set", caml_string_set;
  "caml_sys_open", caml_sys_open;
  "caml_sys_get_argv", caml_sys_get_argv;
  "caml_sys_get_config", caml_sys_get_config;
  "caml_sys_const_big_endian", c1_false;
  "caml_sys_const_word_size", c1_int 64L;
  "caml_sys_const_ostype_unix", c1_true;
  "caml_sys_const_ostype_win32", c1_false;
  "caml_sys_const_ostype_cygwin", c1_false;
  "caml_sys_getenv", caml_sys_getenv;
  "caml_sys_file_exists", caml_sys_file_exists;
  "caml_array_get_addr", C2 caml_array_get_addr;
  "caml_array_get_float", C2 caml_array_get_float;
  "caml_array_get", caml_array_get;
  "caml_array_set_addr", C3 caml_array_set_addr;
  "caml_array_set_float", C3 caml_array_set_float;
  "caml_array_set", caml_array_set;
  "caml_array_unsafe_get_addr", C2 caml_array_get_addr;
  "caml_array_unsafe_get_float", C2 caml_array_get_float;
  "caml_array_unsafe_get", caml_array_get;
  "caml_array_unsafe_set_addr", C3 caml_array_set_addr;
  "caml_array_unsafe_set_float", C3 caml_array_set_float;
  "caml_array_unsafe_set", caml_array_set;
  "caml_array_sub", caml_array_sub;
  "caml_array_blit", caml_array_blit;
  "caml_make_vect", caml_make_vect;
  "caml_obj_block", caml_obj_block;
  "caml_obj_dup", caml_obj_dup;
  "caml_obj_tag", caml_obj_tag;
  "caml_compare", C2 caml_compare;
  "caml_equal", C2 caml_equal;
  "caml_notequal", C2 caml_notequal;
  "caml_lessthan", C2 caml_lessthan;
  "caml_lessequal", C2 caml_lessequal;
  "caml_greaterthan", C2 caml_greaterthan;
  "caml_greaterequal", C2 caml_greaterequal;
  "caml_int_compare", C2 caml_compare;
  "caml_hash", caml_hash;
  "caml_weak_create", caml_weak_create;
  "caml_ensure_stack_capacity", c1_unit;
  "caml_dynlink_get_current_libs", caml_dynlink_get_current_libs;
  "caml_format_int", caml_format_int;
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

