open Printf
open Hardcaml
module Waveform = Hardcaml_waveterm.Waveform
module Z = Cyclesim.With_interface (Zinc.I) (Zinc.O)
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
      ( of_int32 bc.code.(i * 2),
        try of_int32 bc.code.((i * 2) + 1) with _ -> 0L )
    in
    let a, b = (logand a 0xFFFFFFFFL, logand b 0xFFFFFFFFL) in
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
      memory.{i} <-
        ( if i < code_size then exe i
        else if i < globals_address then atom (i - code_size)
        else if i < c_heap_address then globals.(i - globals_address)
        else 0L )
    done
  in
  let stack_address = memory_size_words in
  (* init the c-heap *)
  let () = C_runtime.init (c_heap_address * 8) c_heap_size_bytes in
  ( {
      code_address = code_address * 8;
      code_size = Array.length bc.code * 4;
      atoms_address = atoms_address * 8;
      globals_address = globals_address * 8;
      c_heap_address = c_heap_address * 8;
      c_heap_size = c_heap_size_bytes;
      heap_address = heap_address * 8;
      stack_address = stack_address * 8;
    },
    memory )

let init_state mm memory bytecode =
  Machine.
    {
      pc = Int64.of_int mm.code_address;
      sp = Int64.of_int mm.stack_address;
      accu = 1L;
      env = Int64.of_int (mm.atoms_address + 8);
      extra_args = 0L;
      trapsp = Int64.of_int mm.stack_address;
      global_data = Int64.of_int (mm.globals_address + 8);
      atom_table = Int64.of_int (mm.atoms_address + 8);
      alloc_base = Int64.of_int mm.heap_address;
      stack_high = Int64.of_int mm.stack_address;
      memory;
      exe = bytecode;
      mapping = mm;
    }

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
    C_runtime.argv := (prog, argv);
    let mapping, memory = init_memory bytecode (memsize_kb * (1024 / 8)) in
    let state = init_state mapping memory bytecode in
    state

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
      {
        st with
        env = st.memory.{to_int st.sp / 8};
        sp = st.sp +: (nargs *: 8L);
        accu = v;
      }
    in
    let do_exception st v =
      let _, st = M.step { st with accu = v } O.(dispatch RAISE) in
      Some st
    in
    let st = setup_for_c_call st in
    match C_runtime.run st.exe prim st with
    | `ok v -> Some (restore_after_c_call st v)
    | `exn v -> do_exception st v

  let get_instr memory pc =
    let instr = memory.{pc / 2} in
    S.(sra (if pc mod 2 = 0 then sll instr 32L else instr) 32L)

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

  let interactive ~prog ~argv ~memsize_kb =
    let state = ref (init ~prog ~argv ~memsize_kb) in
    let running = ref true in
    let ninstrs = ref 0 in
    object (this)
      method step =
        if !running then (
          incr ninstrs;
          match step !state with
          | Some st -> state := st
          | None -> running := false )

      method steps n =
        let rec f m =
          if n <= 0 || m >= n then ()
          else (
            this#step;
            f (m + 1) )
        in
        f 0

      method stepto n = this#steps (n - this#ninstrs)

      method stepd =
        this#trace#instr;
        this#trace#machine;
        this#step;
        this#ninstrs

      method stepsd n =
        if n > 0 then (
          this#steps (n - 1);
          this#trace#instr;
          this#trace#machine;
          this#step;
          this#ninstrs )
        else this#ninstrs

      method steptod n =
        if n > this#ninstrs then (
          this#stepto (n - 1);
          this#trace#instr;
          this#trace#machine;
          this#step;
          this#ninstrs )
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
end

(* hardware testbench - to be rewritten *)
module Testbench = struct
  type cfg = {
    waves : bool;
    instr_trace : bool;
    state_trace : bool;
    mem_trace : bool;
  }

  let make cfg exe =
    let mem_size_words = 1024 * 1024 in

    let _show_instr =
      let ins =
        Base.List.map Opcode.all ~f:Opcode.to_string |> Base.Array.of_list
      in

      fun x ->
        let i = Bits.to_int x in
        try ins.(i) with _ -> ""
    in

    (* let wave_cfg =
     *   let f = function
     *     | n, b -> if b = 1 then (n, Waveterm_waves.B) else (n, Waveterm_waves.H)
     *   in
     *   Some
     *     ( [
     *         f ("clock", 1);
     *         f ("clear", 1);
     *         f ("enable", 1);
     *         f ("start", 1);
     *         f ("error", 1);
     *         ("state", Waveterm_waves.I Zinc.state_str);
     *         ("pc", Waveterm_waves.U);
     *         ("instruction", Waveterm_waves.F show_instr);
     *       ]
     *     @ Zinc.I.(to_list @@ map t ~f)
     *     @ Zinc.O.(to_list @@ map t ~f)
     *     @ List.map
     *         (fun s -> (s, Waveterm_waves.U))
     *         [ "nfuncs"; "nvars"; "count"; "count_next" ] )
     * in *)
    let sim = Z.create Zinc.zinc in
    let waves, sim =
      if cfg.waves then
        let waves, sim = Waveform.create sim in
        (Some waves, sim)
      else (None, sim)
    in

    let open Zinc.Memory.I in
    let open Zinc.Memory.O in
    let open Zinc.I in
    let open Zinc.O in
    let i =
      Zinc.I.map (Cyclesim.inputs sim) ~f:(fun i d ->
          i := Bits.consti ~width:(Bits.width !i) d)
    in
    let i64 =
      Zinc.I.map (Cyclesim.inputs sim) ~f:(fun i d ->
          i := Bits.consti64 ~width:(Bits.width !i) d)
    in
    let o = Zinc.O.map (Cyclesim.outputs sim) ~f:(fun o () -> Bits.to_int !o) in
    let n =
      Zinc.O.map (Cyclesim.outputs ~clock_edge:After sim) ~f:(fun o () ->
          Bits.to_int !o)
    in
    let n64 =
      Zinc.O.map (Cyclesim.outputs ~clock_edge:After sim) ~f:(fun o () ->
          Bits.to_int64 !o)
    in
    let o64 =
      Zinc.O.map (Cyclesim.outputs sim) ~f:(fun o () ->
          Bits.to_int !o |> Int64.of_int)
    in

    let mapping, memory = init_memory exe mem_size_words in

    let trace () =
      Trace.machine
        {
          Machine.empty with
          memory;
          mapping;
          env = o64.env ();
          sp = o64.sp ();
          accu = o64.sp ();
        }
    in

    Cyclesim.reset sim;
    i.bytecode_start_address mapping.code_address;
    i.atom_table_address (mapping.atoms_address + 8);
    i.globals_start_address (mapping.globals_address + 8);
    i.heap_start_address mapping.heap_address;
    i.stack_start_address mapping.stack_address;
    i.start 1;

    let log_mem_access cycle rw addr data sp =
      if cfg.mem_trace then
        let offs, typ =
          if addr < mapping.atoms_address then ((addr - 0) * 2, "BYTE")
          else if addr < mapping.globals_address then
            (addr - mapping.atoms_address, "ATOM")
          else if addr < mapping.heap_address then
            (addr - mapping.globals_address, "GLBL")
          else if addr >= sp - 8 then (mapping.stack_address - addr - 1, "STCK")
          else (addr - mapping.heap_address, "HEAP")
        in
        printf "[%-8i] %s %s @[%.8x | %.8x] = %.16Lx [sp=%i]\n" cycle
          (if rw = 0 then "R" else "W")
          typ addr offs data sp
    in

    let run () =
      let cycle = ref 0 in
      let stop = ref false in
      let instr_no = ref 1 in
      while (not !stop) && o.error () <> 1 do
        (* instruction trace *)
        if o.state () = 2 then (
          try
            if cfg.state_trace then printf "\n##%i\n" !instr_no;
            incr instr_no;
            if cfg.instr_trace then
              printf "%6i  %s\n%!"
                ((o.pc () / 4) - 1)
                (o.instruction () |> Opcode.of_int |> Opcode.to_string);
            if cfg.state_trace then trace ()
          with _ ->
            stop := true;
            printf "      INVALID\n%!" );
        Cyclesim.cycle sim;
        (* memory accesses *)
        i.memory_i.memory_ready 0;
        if n.memory_o.memory_request () <> 0 then (
          let addr = n.memory_o.memory_address () in
          let rw = n.memory_o.memory_read_write () in
          let sp = o.sp () in
          ( if rw = 0 then (
            (* read *)
            let data = memory.{addr lsr 3} in
            log_mem_access !cycle rw addr data sp;
            i64.memory_i.memory_data_in data )
          else
            (* write *)
            let data = n64.memory_o.memory_data_out () in
            log_mem_access !cycle rw addr data sp;
            memory.{addr lsr 3} <- data );
          i.memory_i.memory_ready 1 );
        (* c-calls *)
        i.c_call_ready 0;
        if n.c_call_request () = 1 then (
          let prim = n.c_call_prim () in
          if cfg.instr_trace then
            Printf.printf "c_call_request: [%i]%s\n" prim exe.Load.prim.(prim);
          let value =
            match
              C_runtime.run exe prim
                {
                  Machine.empty with
                  Machine.env = o64.env ();
                  accu = o64.accu ();
                  sp = o64.sp ();
                  memory;
                }
            with
            | `ok v -> v
            | `exn _ -> failwith "c-call exn not implemented"
          in
          i64.c_call_result value;
          i.c_call_ready 1 );

        i.start 0;
        incr cycle
      done
    in
    let () =
      try run () with
      | Failure x -> printf "\n\nEXN: %s\n\n%!" x
      | _ -> printf "\n\nEXN %s\n\n%!" (Printexc.get_backtrace ())
    in

    match waves with
    | None -> ()
    | Some waves -> Hardcaml_waveterm_interactive.run waves
end
