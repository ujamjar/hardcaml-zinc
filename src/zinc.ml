(* the hardcaml implementation of the zinc machine *)
open HardCaml
open Signal.Comb

let dbits = 64

(* gc colours *)
let white = consti 2 Instr.white
let gray = consti 2 Instr.gray
let blue = consti 2 Instr.blue
let black = consti 2 Instr.black

(* tags *)
let lazy_tag = consti 8 Instr.lazy_tag
let closure_tag = consti 8 Instr.closure_tag
let object_tag = consti 8 Instr.object_tag
let infix_tag = consti 8 Instr.infix_tag
let forward_tag = consti 8 Instr.forward_tag
let no_scan_tag = consti 8 Instr.no_scan_tag
let abstract_tag = consti 8 Instr.abstract_tag
let string_tag = consti 8 Instr.string_tag
let double_tag = consti 8 Instr.double_tag
let double_array_tag = consti 8 Instr.double_array_tag
let custom_tag = consti 8 Instr.custom_tag

let hdr_size_bits = dbits - 10
let make_header size col tag = 
  assert (width tag = 8);
  assert (width col = 2);
  uresize size hdr_size_bits @: col @: tag

module Rspec = struct
  let reg_spec = Signal.Seq.r_sync
  let ram_spec = Signal.Seq.r_none
end

module Seq = Signal.Make_seq(Rspec)
module Sm = State.Seq(Rspec)

module Memory = struct
  module I = interface
    (* memory data input *)
    memory_data_in[dbits]
    (* memory data in ready *)
    memory_ready[1]
  end
  module O = interface
    (* memory access request *)
    memory_request[1]
    (* memory read/write *)
    memory_read_write[1]
    (* memory address *)
    memory_address[dbits]
    (* memory data out *)
    memory_data_out[dbits]
  end
end

(* arbitrate between the bytecode, stack and memory access interfaces *)
let memory_if 
  ~e
  ~stack_o ~bc_o ~mem_o 
  ~ext_i
  =
  let open Memory.I in
  let open Memory.O in

  let (--) s n = s -- ("memory_if_" ^ n) in

  (* statemachine *)
  let is, sm, next = Seq.statemachine ~e [ `request; `ready ] in

  (* arbitrate access to the memory interface *)
  let m = [stack_o; bc_o; mem_o] in
  let req = concat (List.rev (List.map (fun x -> x.memory_request#q) m)) in
  let gnt = 
    let e = e &: (is `request) -- "arbiter_en" in
    Arbiter.arbiter ~prefix:HardCamlExamples.Prefix.sklansky ~e ~req 
  in

  let gntb = onehot_to_binary gnt in
  let sel f = mux gntb (List.map f m) in

  let ext_o = 
    {
      memory_data_out = sel (fun x -> x.memory_data_out#q);
      memory_read_write = sel (fun x -> x.memory_read_write#q);
      memory_address = sel (fun x -> x.memory_address#q);
      memory_request = sel (fun x -> x.memory_request#q);
    }
  in

  Signal.Guarded.(compile [
    sm [
      `request, [
        g_when (req <>:. 0) [ next `ready; ]
      ];
      `ready, [
        g_when ext_i.memory_ready [ next `request; ]
      ];
    ];
  ]);

  let stack_i, bc_i, mem_i = 
    let mk i = 
      { 
        memory_data_in = ext_i.memory_data_in; 
        memory_ready = gnt.[i:i] &: ext_i.memory_ready 
      }
    in
    mk 0, mk 1, mk 2
  in
  ext_o, stack_i, bc_i, mem_i

module Decode = interface
  acc[1] acc_op
  push[1] pushacc[1] push_op
  pop[1]
  assign[1]
  envacc[1] envacc_op
  apply[1] apply_op
  closurerec[1]
  atom[1] atom_op
  pushatom[1] pushatom_op
  makeblock[1] makeblock_op
  getfield[1] getfield_op
  setfield[1] setfield_op
  branch[1] branch_op
  ccall[1] ccall_op
  const[1] const_op
  pushconst[1] pushconst_op
  alu[1] alu_op
  comp[1] comp_op
  bcomp[1] bcomp_op
  ucomp[1] ucomp_op
  bucomp[1] bucomp_op
  stop[1]
end

module I = interface
  start[1]
  bytecode_start_address[dbits]
  atom_table_address[dbits]
  globals_start_address[dbits]
  heap_start_address[dbits]
  stack_start_address[dbits]
  (memory_i : Memory.I)
end

module O = interface
  state
  pc[dbits]
  sp[dbits]
  accu[dbits]
  instruction[8]
  error[1]
  (memory_o : Memory.O)
  (decode : Decode)
end

type decinstr = { lte : t; gte : t; eq : t; range : t; }

let decode instr = 
  let open Instr in
  let constins i = consti 8 (Enum_opcodes.from_enum i) in
  let min_ins, max_ins = Bounded.min_bound<opcodes>, Bounded.max_bound<opcodes> in
  let ins = Enum.enum_from_to<opcodes> min_ins max_ins in
  let sel = 
    let sel = 
      Array.of_list @@ 
        List.map (fun i ->
          let i = constins i in
          { (* with some care perhaps we can use 1 subtractor for all this??? *)
            lte = instr <=: i;
            gte = instr >=: i;
            eq = instr ==: i;
            range = instr -: i;
          }) ins
    in
    (fun i -> sel.(Enum.from_enum<opcodes> i))
  in
  
  let acc = (sel ACC).lte in
  let acc_op = (sel ACC0).range.[3:0] in
  
  let push = (sel PUSH).eq |: (sel PUSHACC0).eq in
  let pushacc = (sel PUSH).gte &: (sel PUSHACC).lte in
  let push_op = (sel PUSHACC0).range.[3:0] in

  let pop = (sel POP).eq in
  let assign = (sel ASSIGN).eq in
  
  let envacc = (sel ENVACC1).gte &: (sel ENVACC).lte in
  let envacc_op = (sel ENVACC1).range.[2:0] in

  (* push_retaddr *)

  let apply = (sel APPLY).gte &: (sel APPLY3).lte in
  let apply_op = (sel APPLY).range.[1:0] in

  (* return, reset, grab, closure, closurerec *)
  let closurerec = (sel CLOSUREREC).eq in

  (* offsetclosurem2, offsetclosure0, offsetclosure2, offsetclosure *)

  (* pushoffsetclosurem2, pushoffsetclosure0, pushoffsetclosure2, pushoffsetclosure *)

  (* getglobal, pushgetglobal, getglobalfield, pushgetglobalfield, setglobal *)

  let atom = (sel ATOM0).gte &: (sel ATOM).lte in
  let atom_op = (sel ATOM0).range.[0:0] in

  let pushatom = (sel PUSHATOM0).gte &: (sel PUSHATOM).lte in
  let pushatom_op = (sel PUSHATOM0).range.[0:0] in

  let makeblock = (sel MAKEBLOCK).gte &: (sel MAKEFLOATBLOCK).lte in
  let makeblock_op = (sel MAKEBLOCK).range.[2:0] in

  let getfield = (sel GETFIELD).gte &: (sel GETFLOATFIELD).lte in
  let getfield_op = (sel GETFIELD).range.[2:0] in

  let setfield = (sel SETFIELD).gte &: (sel SETFLOATFIELD).lte in
  let setfield_op = (sel SETFIELD).range.[2:0] in

  (* vectlength, getvectitem, setvectitem *)
  
  (* getstringchar, setstringchar *)

  let branch = (sel BRANCH).gte &: (sel SWITCH).lte in (* include switch? *)
  let branch_op = (sel BRANCH).range.[1:0] in

  (* boolnot *)

  (* pushtrap, poptrap, raise *)
  
  (* check_signals *)
  
  let ccall = (sel C_CALL1).gte &: (sel C_CALLN).lte in
  let ccall_op = (sel C_CALL1).range.[2:0] in
  
  let const = (sel CONST0).gte &: (sel CONSTINT).lte in
  let const_op = (sel CONST0).range.[2:0] in
  
  let pushconst = (sel PUSHCONST0).gte &: (sel PUSHCONSTINT).lte in
  let pushconst_op = (sel PUSHCONST0).range.[2:0] in
  
  let alu = (sel NEGINT).gte &: (sel ASRINT).lte in
  let alu_op = (sel NEGINT).range.[3:0] in
  
  let comp = (sel EQ).gte &: (sel GEINT).lte in
  let comp_op = (sel EQ).range.[2:0] in
  
  (* offsetint, offsetref, isint, getmethod *)
  
  let bcomp = (sel BEQ).gte &: (sel BGEINT).lte in
  let bcomp_op = (sel BGEINT).range.[2:0] in
  
  let ucomp = (sel ULTINT).gte &: (sel UGEINT).lte in
  let ucomp_op = (sel ULTINT).range.[0:0] in
  
  let bucomp = (sel BULTINT).gte &: (sel BUGEINT).lte in
  let bucomp_op = (sel BULTINT).range.[0:0] in
  
  (* getpubmet, getdynmet *)
  
  (* stop, event, break, reraise, raise_notrace *)
  let stop = (sel STOP).eq in

  Decode.{ 
    acc; acc_op; 
    push; pushacc; push_op;
    pop; assign;
    envacc; envacc_op;
    apply; apply_op;
    closurerec;
    atom; atom_op;
    pushatom; pushatom_op;
    makeblock; makeblock_op;
    getfield; getfield_op;
    setfield; setfield_op;
    branch; branch_op;
    ccall; ccall_op;
    const; const_op;
    pushconst; pushconst_op;
    alu; alu_op;
    comp; comp_op;
    bcomp; bcomp_op;
    ucomp; ucomp_op;
    bucomp; bucomp_op;
    stop;
  }

let alu_int op a b = 
  let a, b, z = msbs a, msbs b, zero (dbits-1) in
  mux op [
    z; (* negate - 1 arg *)
    a +: b;
    a -: b;
    (a *+ b).[dbits-2:0];
    z; (* div - need iterative implementation *)
    z; (* mod - need iterative implementation *)
    a &: b;
    a |: b;
    a ^: b;
    log_shift sll a b.[5:0]; (* XXX 64/32 *)
    log_shift srl a b.[5:0];
    log_shift sra a b.[5:0];
  ] @: vdd

let comp_int op a b = 
  let a, b = msbs a, msbs b in
  zero (dbits-2) @: mux op [
    a ==: b;
    a <>: b;
    a <+ b;
    a <=+ b;
    a >+ b;
    a >=+ b;
  ] @: vdd

let ucomp_int op a b = 
  let a, b = msbs a, msbs b in
  zero (dbits-2) @: mux op [
    a <: b;
    a >: b;
  ] @: vdd

type states = [ 
  `init | `fetch | `decode | 
  `acc_set | `acc_offset | `pushacc | `pop | `constint |
  `branch | `alu | `comp | `ucomp | `bcomp | `bucomp | `bcomp_setpc |
  `closure_nfuncs | `closure_nvars | `closure_alloc | 
  `closure_var_start | `closure_var_read | `closure_var_write |
  `closure_func_start | `closure_func_hdr | `closure_func_pc | 
  `closure_func_wpc | `closure_func_stack | 
  `makeblock | `makeblock_alloc | `makeblock_accu | `makeblock_read | `makeblock_write |
  `invalid_instruction 
  ] deriving(Show, Enum, Bounded)

let state_range, state_str = 
  let min,max = Bounded.min_bound<states>, Bounded.max_bound<states> in
  let stater = Enum.enum_from_to<states> min max in
  let states = List.map Show.show<states> stater in
  stater, states

let zinc i = 
  let open Memory.O in
  let open Memory.I in
  let open Decode in
  let open I in
  let open O in
  let open Signal.Guarded in

  let e = vdd in

  let ures x = uresize x dbits in
  let sres x = sresize x dbits in
  let val_int x = uresize x (dbits-1) @: vdd in
  let val_unit = val_int (zero dbits) in

  (* memory interface *)
  let stack_o = Memory.O.(map (fun (n,b) -> Seq.g_reg ~e b) t) in
  let mem_o = Memory.O.(map (fun (n,b) -> Seq.g_reg ~e b) t) in
  let bc_o = Memory.O.(map (fun (n,b) -> Seq.g_reg ~e b) t) in
  let memory_o, stack_i, bc_i, mem_i = 
    memory_if ~e ~stack_o ~bc_o ~mem_o ~ext_i:i.memory_i 
  in

  (* for 64 bit memory we need to extract the 32 bit bytecode value *)
  let bc_sel = Seq.g_reg ~e 1 in
  let bc_i = 
    { bc_i with 
        memory_data_in = 
          sres @@ mux2 bc_sel#q
            bc_i.memory_data_in.[63:32] bc_i.memory_data_in.[31:0] }
  in

  let pc = Seq.g_reg ~e dbits in (* code pointer *)
  let pc_next = pc#q +:. 1 in
  let sp = Seq.g_reg ~e dbits in (* stack pointer *)
  let accu = Seq.g_reg ~e dbits in (* accumulator *)
  let env = Seq.g_reg ~e dbits in (* heap-allocation environment *)
  let error = Seq.g_reg ~e 1 in
  let state = Sm.statemachine ~e state_range in
  let temp = Array.init 4 (fun i -> Seq.g_reg ~e dbits) in 
  let clear_temps = g_proc (Array.to_list @@ Array.map (fun t -> t $==. 0) temp) in
  let alloc_base = Seq.g_reg ~e dbits in
  let alloc_pointer = Seq.g_reg ~e dbits in

  let open State in
  let instruction = bc_i.memory_data_in.[7:0] in
  let decode = decode instruction in
  (*let return = state.reg () in*)

  (* functions for accessing memory *)
  let access_memif mem_i mem_o = 
    let read addr nstate = 
      g_proc [
        mem_o.memory_address $== sll addr 3; (* XXX 64/32 *)
        mem_o.memory_request $==. 1;
        mem_o.memory_read_write $==. 0;
        state.next nstate;
      ]
    in
    let write addr data nstate = 
      g_proc [
        mem_o.memory_address $== sll addr 3; (* XXX 64/32 *)
        mem_o.memory_data_out $== data;
        mem_o.memory_request $==. 1;
        mem_o.memory_read_write $==. 1;
        state.next nstate;
      ]
    in
    let ready = 
      let ready = ~: (mem_o.memory_request#q) |: mem_i.memory_ready in
      (fun f -> 
        g_when (ready) [
          mem_o.memory_request $==. 0;
          g_proc (f mem_i.memory_data_in);
        ]) 
    in
    read, write, ready
  in

  (* reading bytecode *)
  let read_bytecode, when_bytecode_ready = 
    let read, _, ready = access_memif bc_i bc_o in
    let pc32 = srl pc#q 1 in
    let pcsel = lsb pc#q in
    let read nstate = 
      g_proc [
        bc_sel $== pcsel; (* XXX 64/32 *)
        read pc32 nstate;
        pc $== pc_next;
      ]
    in
    read, ready
  in

  (* read and write general memory (ie the heaps, possible need to split into
     major and minor heaps *)
  let read_mem, write_mem, when_mem_ready = access_memif mem_i mem_o in

  let alloc_block col tag words rstate = 
    g_proc [
      write_mem alloc_base#q (make_header words col tag) rstate;
      alloc_base $== (alloc_base#q +: words +:. 1);
      alloc_pointer $== alloc_base#q +:. 1; (* 1 past the header *)
    ];
  in

  (* read and write the stack *)
  let read_stack, write_stack, when_stack_ready = access_memif stack_i stack_o in
  let push_stack = 
    let sp_next = sp#q -:. 1 in
    (fun data nstate -> g_proc [
      write_stack sp_next data nstate;
      sp $== sp_next;
    ])
  in
  let pop_stack =
    let sp_next = sp#q +:. 1 in
    (fun nstate -> g_proc [
      read_stack sp#q nstate;
      sp $== sp_next;
    ])
  in
  let push_stack_accu = push_stack accu#q in

  let count = temp.(0) in
  let count_next = (count#q +:. 1) -- "count_next" in

  let alloc_pointer_next = alloc_pointer#q +:. 1 in

  let closure_nfuncs = temp.(1) in
  let closure_nvars = temp.(2) in
  let closure_nfuncs_offs = (sll (closure_nfuncs#q -- "nfuncs") 1) -:. 1 in
  let closure_blksize = closure_nfuncs_offs +: (closure_nvars#q -- "nvars")  in

  let makeblock_wosize = temp.(1) in

  (* XXX stack argument should maybe be registered??? *)
  let alu_int = alu_int decode.alu_op accu#q stack_i.memory_data_in in
  (* separate comp and bcomp as they take args in different orders.
   * I think we can optimise these functions so that these seperate
   * blocks are more efficient anyway *)
  let bcomp_op, bucomp_op = temp.(0)#q.[2:0], temp.(0)#q.[0:0] in
  let bcomp_int = (comp_int bcomp_op bc_i.memory_data_in accu#q).[1:1] in
  let bucomp_int = (ucomp_int bucomp_op bc_i.memory_data_in accu#q).[1:1] in
  let comp_int = comp_int decode.comp_op accu#q stack_i.memory_data_in in
  let ucomp_int = ucomp_int decode.ucomp_op accu#q stack_i.memory_data_in in

  compile [

    mem_o.memory_read_write $==. 0;
    mem_o.memory_data_out $==. 0;
    mem_o.memory_request $==. 0;
    mem_o.memory_address $==. 0;

    (* not used *)
    bc_o.memory_read_write $==. 0; 
    bc_o.memory_data_out $==. 0;

    state.machine [
      `init, [
        accu $== val_unit; 
        sp $== i.stack_start_address;
        pc $== i.bytecode_start_address;
        alloc_base $== i.heap_start_address;
        env $==. 0; (* not sure *)
        g_when i.start [
          state.next `fetch;
        ]
      ];

      (* fetch instruction *)
      `fetch, [
        read_bytecode `decode;
        (* clear temporaries *)
        clear_temps;
      ];

      (* decode instruction *)
      `decode, [
        when_bytecode_ready (fun _ -> [
          (* branch instruction *)
          g_if decode.acc [
            g_if (msb decode.acc_op) 
              [ read_bytecode `acc_offset; ] 
              [ read_stack (sp#q +: (ures decode.acc_op)) `acc_set; ];
          ]
          (* push/acc/const *)
          @@ g_elif (decode.pushacc |: decode.pushconst) [ push_stack_accu `pushacc; ]
          @@ g_elif decode.const [ 
            g_if (msb decode.const_op) [
              read_bytecode `constint
            ] [
              accu $== val_int decode.const_op; 
              state.next `fetch;
            ]
          ]
          (* branch *)
          @@ g_elif decode.branch [ read_bytecode `branch; ]
          (* pop *)
          @@ g_elif decode.pop [ read_bytecode `pop ]
          (* alu *)
          @@ g_elif decode.alu [ 
            g_if (decode.alu_op ==:. 0) [
              (* only requires 1 operand *)
              accu $== val_int (negate (msbs accu#q));
              state.next `fetch;
            ] [
              (* 2 operands, through alu ... *)
              pop_stack `alu;
              (* ... unless div or mod which need an iterative implementation *)
              g_when (decode.alu_op ==:. 4) [ state.next `invalid_instruction ]; (* XXX todo *)
              g_when (decode.alu_op ==:. 5) [ state.next `invalid_instruction ];
            ]
          ]
          (* signed comparision *)
          @@ g_elif decode.comp [ pop_stack `comp; ]
          @@ g_elif decode.ucomp [ pop_stack `ucomp; ]
          @@ g_elif decode.bcomp [ temp.(0) $== ures decode.bcomp_op; read_bytecode `bcomp; ]
          @@ g_elif decode.bucomp [ temp.(0) $== ures decode.bucomp_op; read_bytecode `bucomp; ]
          (* closure(rec) *)
          @@ g_elif decode.closurerec [ read_bytecode `closure_nfuncs; ] 
          (* makeblock *)
          @@ g_elif decode.makeblock [ 
            g_if (decode.makeblock_op ==:. 0) [
              read_bytecode `makeblock
            ] @@ g_elif (decode.makeblock_op ==:. 4) [
              state.next `invalid_instruction (* MAKEFLOATBLOCK *)
            ] [
              makeblock_wosize $== ures decode.makeblock_op; (* size *)
              read_bytecode `makeblock_alloc
            ]
          ]
          @@ g_elif decode.stop [ state.next `invalid_instruction ]
          (* not implemented or invalid *)
          [ state.next `invalid_instruction; ];
        ])
      ];

      `acc_offset, [
        when_bytecode_ready (fun offset -> [
          read_stack (sp#q +: offset) `acc_set
        ]);
      ];

      `acc_set, [
        when_stack_ready (fun data -> [
          accu $== data;
          state.next `fetch;
        ])
      ];

      `pushacc, [
        when_stack_ready (fun _ -> [
          g_if decode.pushconst [
            g_if (msb decode.pushconst_op) [ 
              read_bytecode `constint; 
            ] [ 
              accu $== val_int decode.pushconst_op;
              state.next `fetch;
            ]
          ] @@ g_elif decode.push [
            state.next `fetch;
          ] [
            g_if (msb decode.push_op) [
              read_bytecode `acc_offset;
            ] [
              read_stack (sp#q +: (ures decode.push_op)) `acc_set;
            ]
          ]
        ]);
      ];

      `pop, [
        when_bytecode_ready (fun offset -> [
          sp $== sp#q +: offset;
          state.next `fetch;
        ]);
      ];

      `constint, [
        when_bytecode_ready (fun data -> [
          accu $== val_int data;
          state.next `fetch;
        ])
      ];

      (* perform branch *)
      `branch, [
        when_bytecode_ready (fun data -> [
          pc $== pc#q +: data -:. 1;
          state.next `fetch;
        ]);
      ];

      (* alu *)
      `alu, [
        when_stack_ready (fun _ -> [
          accu $== alu_int;
          state.next `fetch;
        ])
      ];

      (* comparison *)
      `comp, [
        when_stack_ready (fun _ -> [
          accu $== comp_int;
          state.next `fetch;
        ])
      ];

      `ucomp, [
        when_stack_ready (fun _ -> [
          accu $== ucomp_int;
          state.next `fetch;
        ])
      ];

      (* branch with comparison *)
      `bcomp, [
        when_bytecode_ready (fun _ -> [
          g_if bcomp_int [
            read_bytecode `bcomp_setpc;
          ] [
            pc $== pc_next; (* skip branch address *)
            state.next `fetch;
          ]
        ]);
      ];

      `bucomp, [
        when_bytecode_ready (fun _ -> [
          g_if bucomp_int [
            read_bytecode `bcomp_setpc;
          ] [
            pc $== pc_next;
            state.next `fetch;
          ]
        ]);
      ];

      `bcomp_setpc, [
        when_bytecode_ready (fun data -> [
          pc $== pc#q +: data -:. 1;
          state.next `fetch;
        ])
      ];

      (* makeblock *)
      `makeblock, [
        when_bytecode_ready (fun wosize -> [
          makeblock_wosize $== wosize;
          read_bytecode `makeblock_alloc
        ]);
      ];

      `makeblock_alloc, [
        when_bytecode_ready (fun tag -> [
          alloc_block black tag.[7:0] makeblock_wosize#q `makeblock_accu;
        ]);
      ];
      
      `makeblock_accu, [
        when_mem_ready (fun _ -> [
          count $== count_next;
          alloc_pointer $== alloc_pointer_next;
          write_mem alloc_pointer#q accu#q `makeblock_read;
        ]);
      ];

      `makeblock_read, [
        when_mem_ready (fun _ -> [
          g_if (count#q ==: makeblock_wosize#q) [
            state.next `fetch;
          ] [
            pop_stack `makeblock_write;
          ]
        ]);
      ];

      `makeblock_write, [
        when_stack_ready (fun data -> [
          count $== count_next;
          alloc_pointer $== alloc_pointer_next;
          write_mem alloc_pointer#q
            (mux2 (count#q ==:. 0) accu#q data) `makeblock_read;
        ])
      ];

      (* closure/rec *)

      (* nfuncs = *pc++
       * nvars = *pc++
       * allocate a block with (nfuncs*2-1+nvars) words [closure_tag]
       * if nvars > 0 then push accu to stack
       * for nvars
       *   read @ pc
       *   write pc+pc[0] to block
       *   write accu to stack
       * push (pc+pc[0]) to stack
       * write accu to stack
       * for nfuncs-1
       *   read @ pc
       *   write header to block
       *   write *p = pc+pc[i] to block
       *   write p to stack
       *)
      `closure_nfuncs, [
        when_bytecode_ready (fun data -> [
          closure_nfuncs $== data;
          read_bytecode `closure_nvars;
        ])
      ];
      `closure_nvars, [
        when_bytecode_ready (fun data -> [
          closure_nvars $== data;
          g_if (data >:. 0) 
            [ push_stack_accu `closure_alloc ] 
            [ state.next `closure_alloc ]
        ])
      ];
      `closure_alloc, [
        when_stack_ready (fun _ -> [
          alloc_block black closure_tag closure_blksize `closure_var_start;
        ])
      ];
      `closure_var_start, [
        accu $== alloc_pointer#q;
        when_mem_ready (fun _ -> [
          count $==. 0;
          g_if (closure_nvars#q <>:. 0) [
            state.next `closure_var_read;
          ] [
            state.next `closure_func_start
          ]
        ])
      ];
      `closure_var_read, [
        when_mem_ready (fun _ -> [
          let addr = count#q -- "closure_var_idx" in
          read_stack addr `closure_var_write;
        ]);
      ];
      `closure_var_write, [
        when_stack_ready (fun data -> 
          let addr = count#q +: alloc_pointer#q +: closure_nfuncs_offs in
          [
            write_mem addr data `closure_var_read;
            count $== count_next;
            g_when (count_next ==: closure_nvars#q) [
              state.next `closure_func_start;
            ]
          ]);
      ];
      (* setup 1st pass *)
      `closure_func_start, [
          count $==. 0;
          temp.(3) $== pc#q; (* store base pc *)
          when_mem_ready (fun _ -> [ read_bytecode `closure_func_wpc; ]);
      ];
      (* write header *)
      `closure_func_hdr, begin 
        let data = make_header (sll count#q 1) white infix_tag in
        [
          when_stack_ready (fun _ -> [
            alloc_pointer $== alloc_pointer_next;
            g_if (count#q ==: closure_nfuncs#q) [
              state.next `fetch;
            ] [
              write_mem alloc_pointer#q data `closure_func_pc;
            ];
          ])
        ]
      end;
      (* read bytecode *)
      `closure_func_pc, [
        when_mem_ready (fun _ -> [
          read_bytecode `closure_func_wpc;
        ]);
      ];
      (* write pc+pc[x] *)
      `closure_func_wpc, [
        when_bytecode_ready (fun data -> [ 
          alloc_pointer $== alloc_pointer_next;
          write_mem alloc_pointer#q (temp.(3)#q +: data) `closure_func_stack; 
        ])
      ];
      (* store to stack *)
      `closure_func_stack, begin
        let data = 
          mux2 (count#q ==:. 0) 
            accu#q (alloc_pointer#q +: (sll count#q 1) +:. 1)  (* XXX ??? *)
        in
        [
          when_mem_ready (fun _ -> [ 
            count $== count_next;
            push_stack data `closure_func_hdr; 
          ])
        ]
      end;

      (* invalid, or more likely not implemented yet *)
      `invalid_instruction, [
        error $==. 1;
      ];
    ];
  ];

  { 
    state=state.cur;
    pc=pc#q; 
    sp=sp#q;
    accu=accu#q;
    instruction = mux2 (state.is `decode &: bc_i.memory_ready) instruction (consti 8 255);
    error=error#q;
    memory_o; decode; 
  }


