open Machine

let c_heap_size_bytes = 2 * 1024 * 1024 (* 2Mb c-heap *)

let init_memory bc memory_size_words =
  let open Int64 in
  let open Load in
  (* convert exe to 64 bit *)
  let code_size = (Array.length bc.code + 1) / 2 in
  let code_address = 0 in
  let exe i =
    let a, b =
      ( of_int32 bc.code.(i * 2)
      , try of_int32 bc.code.((i * 2) + 1) with
        | _ -> 0L )
    in
    let a, b = logand a 0xFFFFFFFFL, logand b 0xFFFFFFFFL in
    logor a (shift_left b 32)
  in
  (* atoms table *)
  let atom i = of_int i in
  let atoms_address = code_size in
  (* globals *)
  let globals_address = atoms_address + 256 in
  let globals = get_global_data64 bc globals_address in
  (* init data *)
  let c_heap_address = globals_address + Array.length globals in
  let heap_address = c_heap_address + (c_heap_size_bytes / 8) in
  let memory = Bigarray.(Array1.create int64 c_layout memory_size_words) in
  let () =
    for i = 0 to memory_size_words - 1 do
      memory.{i}
        <- (if i < code_size
           then exe i
           else if i < globals_address
           then atom (i - code_size)
           else if i < c_heap_address
           then globals.(i - globals_address)
           else 0L)
    done
  in
  let stack_address = memory_size_words in
  (* init the c-heap *)
  let () = C_runtime.init (c_heap_address * 8) c_heap_size_bytes in
  ( { code_address = code_address * 8
    ; code_size = Array.length bc.code * 4
    ; atoms_address = atoms_address * 8
    ; globals_address = globals_address * 8
    ; c_heap_address = c_heap_address * 8
    ; c_heap_size = c_heap_size_bytes
    ; heap_address = heap_address * 8
    ; stack_address = stack_address * 8
    }
  , memory )
;;

let init_state mm memory bytecode =
  Machine.
    { pc = Int64.of_int mm.code_address
    ; sp = Int64.of_int mm.stack_address
    ; accu = 1L
    ; env = Int64.of_int (mm.atoms_address + 8)
    ; extra_args = 0L
    ; trapsp = Int64.of_int mm.stack_address
    ; global_data = Int64.of_int (mm.globals_address + 8)
    ; atom_table = Int64.of_int (mm.atoms_address + 8)
    ; alloc_base = Int64.of_int mm.heap_address
    ; stack_high = Int64.of_int mm.stack_address
    ; memory
    ; exe = bytecode
    ; mapping = mm
    }
;;

module Interp = struct
  open Machine
  module S = Interp.State_eval

  module M =
    Interp.Monad
      (struct
        let trace = false
      end)
      (S)

  module O = Interp.Opcodes (M)

  let init ~prog ~argv ~memsize_kb =
    let bytecode = Load.bytecode_exe prog in
    C_runtime.argv := prog, argv;
    let mapping, memory = init_memory bytecode (memsize_kb * (1024 / 8)) in
    let state = init_state mapping memory bytecode in
    state
  ;;

  let do_c_call st nargs prim =
    let open Ops.Int64 in
    let open Int64 in
    assert (nargs <> 0L);
    (* XXX C_CALLN TODO *)
    let prim = to_int prim in
    let setup_for_c_call st =
      let st = { st with sp = st.sp -: 8L } in
      st.memory.{to_int st.sp / 8} <- st.env;
      st
    in
    let restore_after_c_call st v =
      { st with
        env = st.memory.{to_int st.sp / 8}
      ; sp = st.sp +: (nargs *: 8L)
      ; accu = v
      }
    in
    let do_exception st v =
      let _, st = M.step { st with accu = v } O.(dispatch RAISE) in
      Some st
    in
    let st = setup_for_c_call st in
    match C_runtime.run st.exe prim st with
    | Ok v -> Some (restore_after_c_call st v)
    | Error v -> do_exception st v
  ;;

  let get_instr memory pc =
    let instr = memory.{pc / 2} in
    S.(sra (if pc mod 2 = 0 then sll instr 32L else instr) 32L)
  ;;

  let step ?(trace = 0) st =
    (* fetch instruction *)
    let pc = Int64.to_int st.pc / 4 in
    let instr = get_instr st.memory pc in
    let instr = Opcode.of_int @@ Int64.to_int instr in
    let () =
      if trace > 0 then Trace.instr st;
      if trace > 1 then Trace.machine st
    in
    (* execute instruction *)
    let st = { st with pc = S.(st.pc +: 4L) } in
    let result, st = M.step st O.(dispatch instr) in
    match result with
    | `step -> Some st
    | `stop -> None
    | `c_call (nargs, prim) -> do_c_call st nargs prim
  ;;

  let interactive ~prog ~argv ~memsize_kb =
    let state = ref (init ~prog ~argv ~memsize_kb) in
    let running = ref true in
    let ninstrs = ref 0 in
    object (this)
      method step =
        if !running
        then (
          incr ninstrs;
          match step !state with
          | Some st -> state := st
          | None -> running := false)

      method steps n =
        let rec f m =
          if n <= 0 || m >= n
          then ()
          else (
            this#step;
            f (m + 1))
        in
        f 0

      method stepto n = this#steps (n - this#ninstrs)

      method stepd =
        this#trace#instr;
        this#trace#machine;
        this#step;
        this#ninstrs

      method stepsd n =
        if n > 0
        then (
          this#steps (n - 1);
          this#trace#instr;
          this#trace#machine;
          this#step;
          this#ninstrs)
        else this#ninstrs

      method steptod n =
        if n > this#ninstrs
        then (
          this#stepto (n - 1);
          this#trace#instr;
          this#trace#machine;
          this#step;
          this#ninstrs)
        else this#ninstrs

      method state = !state

      method running = !running

      method ninstrs = !ninstrs

      method trace =
        object
          method machine = Trace.machine !state

          method instr = Trace.instr !state

          method value v =
            Trace.value !state v;
            Printf.printf "\n"

          method root v = Trace.root !state v
        end
    end
  ;;
end
