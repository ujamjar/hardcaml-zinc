(* hardcaml implementation of the zinc machine *)

(* instructions todo;

  RETURN, RESTART, OFFSETCLOSURE, PUSHOFFSETCLOSURE

  PUSHTRAP, POPTRAP, RAISE, CHECK_SIGNALS

  GETMETHOD, GETPUBMET, GETDYNMET

  EVENT, BREAK, RERAISE, RAISE_NOTRACE

  DIVINT, MODINT

  C_CALLN (part of decode.c_call)
  
  SWITCH (part of decode.branch)

  STOP (as part of callbacks)

  APPLY (decode.apply - APPLY1..3 ok)

  APPTERM (decode.appterm - APPTERM1..3 ok)

  GRAB (extra_args < required)

  MAKEFLOATBLOCK

*)

open Import
open Signal

let dbits = 64

let bpw = dbits / 8

let asft = Int.ceil_log2 bpw

let reg_spec = Reg_spec.create () ~clock ~clear

module Memory = struct
  module I = struct
    type 'a t = {
      (* memory data input *)
      memory_data_in : 'a; [@bits dbits]
      (* memory data in ready *)
      memory_ready : 'a;
    }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = {
      (* memory access request *)
      memory_request : 'a;
      (* memory read/write *)
      memory_read_write : 'a;
      (* memory address *)
      memory_address : 'a; [@bits dbits]
      (* memory data out *)
      memory_data_out : 'a; [@bits dbits]
    }
    [@@deriving sexp_of, hardcaml]
  end
end

(* arbitrate between the bytecode, stack and memory access interfaces *)
let memory_if ~e ~stack_o ~bc_o ~mem_o ~ext_i =
  let open Memory.I in
  let open Memory.O in
  let open Always in
  let ( -- ) s n = s -- ("memory_if_" ^ n) in

  (* statemachine *)
  let module States = struct
    type t = Request | Ready [@@deriving enumerate, compare, sexp_of]
  end in
  let { State_machine.is; switch = sm; set_next = next; _ } =
    State_machine.create (module States) ~enable:e reg_spec
  in

  (* arbitrate access to the memory interface *)
  let m = [ stack_o; bc_o; mem_o ] in
  let req =
    concat_lsb
      (List.map m ~f:(fun (x : Always.Variable.t Memory.O.t) ->
           x.memory_request.value))
  in
  let gnt =
    let e = e &: is Request -- "arbiter_en" in
    let prefix f l =
      (* Parallel prefix networks are more efficient *)
      List.fold l ~init:[] ~f:(fun acc x ->
          match acc with [] -> [ x ] | h :: t -> f x h :: h :: t)
      |> List.rev
    in
    Arbiter.arbiter ~prefix ~enable:e ~req
  in

  let gntb = onehot_to_binary gnt in
  let sel f = mux gntb (List.map m ~f) in

  let ext_o =
    {
      memory_data_out = sel (fun x -> x.memory_data_out.value);
      memory_read_write = sel (fun x -> x.memory_read_write.value);
      memory_address = sel (fun x -> x.memory_address.value);
      memory_request = sel (fun x -> x.memory_request.value);
    }
  in

  Always.(
    compile
      [
        sm
          [
            (Request, [ when_ (req <>:. 0) [ next Ready ] ]);
            (Ready, [ when_ ext_i.memory_ready [ next Request ] ]);
          ];
      ]);

  let stack_i, bc_i, mem_i =
    let mk i =
      {
        memory_data_in = ext_i.memory_data_in;
        memory_ready = bit gnt i &: ext_i.memory_ready;
      }
    in
    (mk 0, mk 1, mk 2)
  in
  (ext_o, stack_i, bc_i, mem_i)

module Decode = struct
  type 'a t = {
    acc : 'a;
    acc_op : 'a;
    push : 'a;
    pushacc : 'a;
    push_op : 'a;
    pop : 'a;
    assign : 'a;
    envacc : 'a;
    envacc_op : 'a;
    pushenvacc : 'a;
    pushenvacc_op : 'a;
    push_retaddr : 'a;
    apply : 'a;
    apply_op : 'a;
    appterm : 'a;
    appterm_op : 'a;
    closure : 'a;
    closurerec : 'a;
    return : 'a;
    restart : 'a;
    grab : 'a;
    offsetclosure : 'a;
    offsetclosure_op : 'a;
    pushoffsetclosure : 'a;
    pushoffsetclosure_op : 'a;
    pushgetglobal : 'a;
    getglobal : 'a;
    pushgetglobalfield : 'a;
    getglobalfield : 'a;
    setglobal : 'a;
    atom : 'a;
    atom_op : 'a;
    pushatom : 'a;
    pushatom_op : 'a;
    makeblock : 'a;
    makeblock_op : 'a;
    getfield : 'a;
    getfield_op : 'a;
    setfield : 'a;
    setfield_op : 'a;
    vectlength : 'a;
    getvectitem : 'a;
    setvectitem : 'a;
    getstringchar : 'a;
    setstringchar : 'a;
    branch : 'a;
    branch_op : 'a;
    boolnot : 'a;
    pushtrap : 'a;
    poptrap : 'a;
    raise_ : 'a;
    check_signals : 'a;
    c_call : 'a;
    c_call_op : 'a;
    const : 'a;
    const_op : 'a;
    pushconst : 'a;
    pushconst_op : 'a;
    alu : 'a;
    alu_op : 'a;
    comp : 'a;
    comp_op : 'a;
    offsetint : 'a;
    offsetref : 'a;
    isint : 'a;
    getmethod : 'a;
    bcomp : 'a;
    bcomp_op : 'a;
    ucomp : 'a;
    ucomp_op : 'a;
    bucomp : 'a;
    bucomp_op : 'a;
    getpubmet : 'a;
    getdynmet : 'a;
    stop : 'a;
    event : 'a;
    break : 'a;
    reraise : 'a;
    raise_notrace : 'a;
  }
  [@@deriving sexp_of, hardcaml]
end

module I = struct
  type 'a t = {
    start : 'a;
    bytecode_start_address : 'a; [@bits dbits]
    atom_table_address : 'a; [@bits dbits]
    globals_start_address : 'a; [@bits dbits]
    heap_start_address : 'a; [@bits dbits]
    stack_start_address : 'a; [@bits dbits]
    memory_i : 'a Memory.I.t;
    c_call_ready : 'a;
    c_call_result : 'a; [@bits dbits]
  }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = {
    state : 'a;
    pc : 'a; [@bits dbits]
    sp : 'a; [@bits dbits]
    accu : 'a; [@bits dbits]
    env : 'a; [@bits dbits]
    extra_args : 'a; [@bits dbits]
    instruction : 'a; [@bits 8]
    error : 'a;
    memory_o : 'a Memory.O.t;
    decode : 'a Decode.t;
    c_call_request : 'a;
    c_call_prim : 'a; [@bits dbits]
  }
  [@@deriving sexp_of, hardcaml]
end

type decinstr = { lte : t; gte : t; eq : t; range : t }

let decode instr =
  let constins i = consti ~width:8 (Opcode.to_int i) in
  let ins = Opcode.all in
  let sel =
    let sel =
      Array.of_list
      @@ List.map ins ~f:(fun i ->
             let i = constins i in
             {
               (* with some care perhaps we can use 1 subtractor for all this??? *)
               lte = instr <=: i;
               gte = instr >=: i;
               eq = instr ==: i;
               range = instr -: i;
             })
    in
    fun i -> sel.(Opcode.to_int i)
  in

  let acc = (sel ACC).lte in
  let acc_op = (sel ACC0).range.:[(3, 0)] in

  let push = (sel PUSH).eq |: (sel PUSHACC0).eq in
  let pushacc = (sel PUSH).gte &: (sel PUSHACC).lte in
  let push_op = (sel PUSHACC0).range.:[(3, 0)] in

  let pop = (sel POP).eq in
  let assign = (sel ASSIGN).eq in

  let envacc = (sel ENVACC1).gte &: (sel ENVACC).lte in
  let envacc_op = (sel ENVACC1).range.:[(2, 0)] in

  let pushenvacc = (sel PUSHENVACC1).gte &: (sel PUSHENVACC).lte in
  let pushenvacc_op = (sel PUSHENVACC1).range.:[(2, 0)] in

  let push_retaddr = (sel PUSH_RETADDR).eq in

  let apply = (sel APPLY).gte &: (sel APPLY3).lte in
  let apply_op = (sel APPLY).range.:[(1, 0)] in

  let appterm = (sel APPTERM).gte &: (sel APPTERM3).lte in
  let appterm_op = (sel APPTERM).range.:[(1, 0)] in

  let closure = (sel CLOSURE).eq in
  let closurerec = (sel CLOSUREREC).eq in
  let return = (sel RETURN).eq in
  let restart = (sel RESTART).eq in
  let grab = (sel GRAB).eq in

  let pushgetglobal = (sel PUSHGETGLOBAL).eq in
  let getglobal = (sel GETGLOBAL).eq in
  let pushgetglobalfield = (sel PUSHGETGLOBALFIELD).eq in
  let getglobalfield = (sel GETGLOBALFIELD).eq in
  let setglobal = (sel SETGLOBAL).eq in

  let offsetclosure = (sel OFFSETCLOSUREM2).gte &: (sel OFFSETCLOSURE).lte in
  let offsetclosure_op = (sel OFFSETCLOSUREM2).range.:[(1, 0)] in

  let pushoffsetclosure =
    (sel PUSHOFFSETCLOSUREM2).gte &: (sel PUSHOFFSETCLOSURE).lte
  in
  let pushoffsetclosure_op = (sel PUSHOFFSETCLOSUREM2).range.:[(1, 0)] in

  let atom = (sel ATOM0).gte &: (sel ATOM).lte in
  let atom_op = (sel ATOM0).range.:[(0, 0)] in

  let pushatom = (sel PUSHATOM0).gte &: (sel PUSHATOM).lte in
  let pushatom_op = (sel PUSHATOM0).range.:[(0, 0)] in

  let makeblock = (sel MAKEBLOCK).gte &: (sel MAKEFLOATBLOCK).lte in
  let makeblock_op = (sel MAKEBLOCK).range.:[(2, 0)] in

  let getfield = (sel GETFIELD).gte &: (sel GETFLOATFIELD).lte in
  let getfield_op = (sel GETFIELD).range.:[(2, 0)] in

  let setfield = (sel SETFIELD).gte &: (sel SETFLOATFIELD).lte in
  let setfield_op = (sel SETFIELD).range.:[(2, 0)] in

  let vectlength = (sel VECTLENGTH).eq in
  let getvectitem = (sel GETVECTITEM).eq in
  let setvectitem = (sel SETVECTITEM).eq in

  let getstringchar = (sel GETSTRINGCHAR).eq in
  let setstringchar = (sel SETSTRINGCHAR).eq in

  let branch = (sel BRANCH).gte &: (sel SWITCH).lte in
  (* include switch? *)
  let branch_op = (sel BRANCH).range.:[(1, 0)] in

  let boolnot = (sel BOOLNOT).eq in

  let pushtrap = (sel PUSHTRAP).eq in
  let poptrap = (sel POPTRAP).eq in
  let raise_ = (sel RAISE).eq in

  let check_signals = (sel CHECK_SIGNALS).eq in

  let c_call = (sel C_CALL1).gte &: (sel C_CALLN).lte in
  let c_call_op = (sel C_CALL1).range.:[(2, 0)] in

  let const = (sel CONST0).gte &: (sel CONSTINT).lte in
  let const_op = (sel CONST0).range.:[(2, 0)] in

  let pushconst = (sel PUSHCONST0).gte &: (sel PUSHCONSTINT).lte in
  let pushconst_op = (sel PUSHCONST0).range.:[(2, 0)] in

  let alu = (sel NEGINT).gte &: (sel ASRINT).lte in
  let alu_op = (sel NEGINT).range.:[(3, 0)] in

  let comp = (sel EQ).gte &: (sel GEINT).lte in
  let comp_op = (sel EQ).range.:[(2, 0)] in

  let offsetint = (sel OFFSETINT).eq in
  let offsetref = (sel OFFSETREF).eq in
  let isint = (sel ISINT).eq in
  let getmethod = (sel GETMETHOD).eq in

  let bcomp = (sel BEQ).gte &: (sel BGEINT).lte in
  let bcomp_op = (sel BGEINT).range.:[(2, 0)] in

  let ucomp = (sel ULTINT).gte &: (sel UGEINT).lte in
  let ucomp_op = (sel ULTINT).range.:[(0, 0)] in

  let bucomp = (sel BULTINT).gte &: (sel BUGEINT).lte in
  let bucomp_op = (sel BULTINT).range.:[(0, 0)] in

  let getpubmet = (sel GETPUBMET).eq in
  let getdynmet = (sel GETDYNMET).eq in

  let stop = (sel STOP).eq in
  let event = (sel EVENT).eq in
  let break = (sel BREAK).eq in
  let reraise = (sel RERAISE).eq in
  let raise_notrace = (sel RAISE_NOTRACE).eq in

  Decode.
    {
      acc;
      acc_op;
      push;
      pushacc;
      push_op;
      pop;
      assign;
      envacc;
      envacc_op;
      pushenvacc;
      pushenvacc_op;
      push_retaddr;
      apply;
      apply_op;
      appterm;
      appterm_op;
      closure;
      closurerec;
      return;
      restart;
      grab;
      offsetclosure;
      offsetclosure_op;
      pushoffsetclosure;
      pushoffsetclosure_op;
      pushgetglobal;
      getglobal;
      pushgetglobalfield;
      getglobalfield;
      setglobal;
      atom;
      atom_op;
      pushatom;
      pushatom_op;
      makeblock;
      makeblock_op;
      getfield;
      getfield_op;
      setfield;
      setfield_op;
      vectlength;
      getvectitem;
      setvectitem;
      getstringchar;
      setstringchar;
      branch;
      branch_op;
      boolnot;
      pushtrap;
      poptrap;
      raise_;
      check_signals;
      c_call;
      c_call_op;
      const;
      const_op;
      pushconst;
      pushconst_op;
      alu;
      alu_op;
      comp;
      comp_op;
      offsetref;
      isint;
      getmethod;
      offsetint;
      bcomp;
      bcomp_op;
      ucomp;
      ucomp_op;
      bucomp;
      bucomp_op;
      getpubmet;
      getdynmet;
      stop;
      event;
      break;
      reraise;
      raise_notrace;
    }

let alu_int op a b =
  let a, b, z = (msbs a, msbs b, zero (dbits - 1)) in
  mux op
    [
      z;
      (* negate - 1 arg *)
      a +: b;
      a -: b;
      (a *+ b).:[(dbits - 2, 0)];
      z;
      (* div - need iterative implementation *)
      z;
      (* mod - need iterative implementation *)
      a &: b;
      a |: b;
      a ^: b;
      log_shift sll a b.:[(5, 0)];
      (* XXX 64/32 *)
      log_shift srl a b.:[(5, 0)];
      log_shift sra a b.:[(5, 0)];
    ]
  @: vdd

let comp_int op a b =
  let a, b = (msbs a, msbs b) in
  zero (dbits - 2)
  @: mux op [ a ==: b; a <>: b; a <+ b; a <=+ b; a >+ b; a >=+ b ]
  @: vdd

let ucomp_int op a b =
  let a, b = (msbs a, msbs b) in
  zero (dbits - 2) @: mux op [ a <: b; a >: b ] @: vdd

module States = struct
  type t =
    [ `init
    | `fetch
    | `decode
    | `acc_set
    | `acc_offset
    | `pushacc
    | `envacc
    | `pop
    | `constint
    | `branch
    | `c_call0
    | `c_call1
    | `c_call2
    | `c_call3
    | `alu
    | `comp
    | `ucomp
    | `bcomp
    | `bucomp
    | `bcomp_setpc
    | `closure_nfuncs
    | `closure_nvars
    | `closure_alloc
    | `closure_var_start
    | `closure_var_read
    | `closure_var_write
    | `closure_func_start
    | `closure_func_hdr
    | `closure_func_pc
    | `closure_func_wpc
    | `closure_func_stack
    | `closure_accu_pc0
    | `closure_accu_pc1
    | `closure_accu_pc2
    | `setglobal
    | `getglobal_data
    | `getglobal
    | `getglobalfield_data
    | `getglobalfield
    | `makeblock
    | `makeblock_alloc
    | `makeblock_accu
    | `makeblock_read
    | `makeblock_write
    | `offsetint
    | `offsetref0
    | `offsetref1
    | `offsetref2
    | `atom
    | `apply_pop_stack
    | `apply_push_stack_env
    | `apply_push_stack_pc
    | `apply_push_stack_args
    | `apply_eargs
    | `apply
    | `appterm0
    | `appterm1
    | `appterm2
    | `appterm3
    | `appterm4
    | `grab
    | `push_retaddr0
    | `push_retaddr1
    | `push_retaddr2
    | `push_retaddr3
    | `vectlength
    | `getvectitem0
    | `getvectitem1
    | `setvectitem0
    | `setvectitem1
    | `setvectitem2
    | `getstringchar0
    | `getstringchar1
    | `setstringchar0
    | `setstringchar1
    | `setstringchar2
    | `setstringchar3
    | `not_implemented
    | `invalid_instruction ]
  [@@deriving sexp_of, compare, enumerate]
end

let state_str =
  List.map States.all ~f:(fun s -> Sexp.to_string_hum (States.sexp_of_t s))

module M = Mlvalues.Make (struct
  include Signal

  let ( /: ) _ _ = failwith "/:"

  let ( %: ) _ _ = failwith "%:"

  let const = consti ~width:dbits

  let zero = const 0

  let one = const 1

  let log_shift f a b =
    if Signal.is_const b then f a (to_int b) else log_shift f a b

  let sll a b = log_shift sll a b.:[(5, 0)]

  let srl a b = log_shift srl a b.:[(5, 0)]

  let sra a b = log_shift sra a b.:[(5, 0)]
end)

let zinc i =
  let open Memory.O in
  let open Memory.I in
  let open Decode in
  let open I in
  let open O in
  let open Always in
  let e = vdd in

  let ures x = uresize x dbits in
  let sres x = sresize x dbits in
  let val_int x = uresize x (dbits - 1) @: vdd in
  let val_unit = val_int (zero dbits) in
  let int_val x = sra x 1 in

  (* memory interface *)
  let stack_o =
    Memory.O.(map t ~f:(fun (_, b) -> Variable.reg ~enable:e reg_spec ~width:b))
  in
  let mem_o =
    Memory.O.(map t ~f:(fun (_, b) -> Variable.reg ~enable:e reg_spec ~width:b))
  in
  let bc_o =
    Memory.O.(map t ~f:(fun (_, b) -> Variable.reg ~enable:e reg_spec ~width:b))
  in
  let memory_o, stack_i, bc_i, mem_i =
    memory_if ~e ~stack_o ~bc_o ~mem_o ~ext_i:i.memory_i
  in

  (* for 64 bit memory we need to extract the 32 bit bytecode value *)
  let bc_sel = Variable.reg reg_spec ~enable:e ~width:1 in
  let bc_i =
    {
      bc_i with
      memory_data_in =
        sres
        @@ mux2 bc_sel.value
             bc_i.memory_data_in.:[(63, 32)]
             bc_i.memory_data_in.:[(31, 0)];
    }
  in

  let pc = Variable.reg reg_spec ~enable:e ~width:dbits in
  (* code pointer *)
  let pc_next = pc.value +:. 4 in
  let sp = Variable.reg reg_spec ~enable:e ~width:dbits in
  (* stack pointer *)
  let accu = Variable.reg reg_spec ~enable:e ~width:dbits in
  (* accumulator *)
  let env = Variable.reg reg_spec ~enable:e ~width:dbits in
  (* heap-allocation environment *)
  let extra_args = Variable.reg reg_spec ~enable:e ~width:dbits in
  (* number of extra args provided by caller *)
  let error = Variable.reg reg_spec ~enable:e ~width:1 in
  let state = State_machine.create (module States) reg_spec ~enable:e in

  let n_temps = 3 in
  let temp =
    Array.init n_temps ~f:(fun i ->
        let g = Variable.reg reg_spec ~enable:e ~width:dbits in
        ignore (g.value -- ("temp_" ^ Int.to_string i) : Signal.t);
        g)
  in
  let clear_temps =
    proc (Array.to_list @@ Array.map temp ~f:(fun t -> t <--. 0))
  in
  let shift_temp_up din =
    proc
      ( Array.to_list
      @@ Array.init n_temps ~f:(fun i ->
             temp.(i) <-- if i = 0 then din else temp.(i - 1).value) )
  in
  let shift_temp_down din =
    proc
      ( Array.to_list
      @@ Array.init n_temps ~f:(fun i ->
             temp.(i) <-- if i = n_temps - 1 then din else temp.(i + 1).value)
      )
  in

  let count =
    let r = Variable.reg reg_spec ~enable:e ~width:dbits in
    ignore (r.value -- "count" : Signal.t);
    r
  in
  let count_next = count.value +:. 1 -- "count_next" in

  let alloc_base = Variable.reg reg_spec ~enable:e ~width:dbits in
  let alloc_pointer = Variable.reg reg_spec ~enable:e ~width:dbits in

  let instruction = bc_i.memory_data_in.:[(7, 0)] in
  let decode = decode instruction in
  let decode' =
    let e = e &: state.is `decode in
    Decode.map decode ~f:(reg reg_spec ~enable:e)
  in

  let c_call_request = Variable.reg reg_spec ~enable:e ~width:1 in
  let c_call_prim = Variable.reg reg_spec ~enable:e ~width:dbits in

  (* functions for accessing memory *)
  let access_memif mem_i mem_o =
    let read addr nstate =
      proc
        [
          mem_o.memory_address <-- addr;
          mem_o.memory_request <--. 1;
          mem_o.memory_read_write <--. 0;
          state.set_next nstate;
        ]
    in
    let write addr data nstate =
      proc
        [
          mem_o.memory_address <-- addr;
          mem_o.memory_data_out <-- data;
          mem_o.memory_request <--. 1;
          mem_o.memory_read_write <--. 1;
          state.set_next nstate;
        ]
    in
    let ready =
      let ready = ~:(mem_o.memory_request.value) |: mem_i.memory_ready in
      fun f ->
        when_ ready
          [ mem_o.memory_request <--. 0; proc (f mem_i.memory_data_in) ]
    in
    (read, write, ready)
  in

  (* reading bytecode *)
  let read_bytecode, when_bytecode_ready =
    let read, _, ready = access_memif bc_i bc_o in
    let pcaddr = pc.value.:[(dbits - 1, asft)] @: zero asft in
    let pcsel = pc.value.:[(2, 2)] in
    let read nstate =
      proc
        [ bc_sel <-- pcsel; (* XXX 64/32 *) read pcaddr nstate; pc <-- pc_next ]
    in
    (read, ready)
  in

  (* read and write general memory (ie the heaps, possible need to split into
     major and minor heaps *)
  let read_mem, write_mem, when_mem_ready = access_memif mem_i mem_o in

  let alloc_block col tag words rstate =
    proc
      [
        write_mem alloc_base.value (M.make_header words col tag) rstate;
        alloc_base <-- alloc_base.value +: sll (words +:. 1) asft;
        alloc_pointer <-- alloc_base.value +:. bpw;
        (* 1 past the header *)
      ]
  in
  let alloc_pointer_next = alloc_pointer.value +:. bpw in

  (* read and write the stack *)
  let read_stack, write_stack, when_stack_ready =
    access_memif stack_i stack_o
  in
  let push_stack =
    let sp_next = sp.value -:. bpw in
    fun data nstate -> proc [ write_stack sp_next data nstate; sp <-- sp_next ]
  in
  let pop_stack =
    let sp_next = sp.value +:. bpw in
    fun nstate -> proc [ read_stack sp.value nstate; sp <-- sp_next ]
  in
  let push_stack_accu = push_stack accu.value in

  let closure_nfuncs = temp.(0) in
  let closure_nvars = temp.(1) in
  let closure_base_pc = temp.(2) in
  let closure_nfuncs_offs =
    mux2 decode'.closure (one dbits)
      (sll (closure_nfuncs.value -- "nfuncs") 1 -:. 1)
  in
  let closure_blksize = closure_nfuncs_offs +: closure_nvars.value in

  let makeblock_wosize = temp.(0) in
  let makeblock_accu_base = temp.(1) in

  (* XXX stack argument should maybe be registered??? *)
  let alu_int = alu_int decode'.alu_op accu.value stack_i.memory_data_in in
  (* separate comp and bcomp as they take args in different orders.
   * I think we can optimise these functions so that these seperate
   * blocks are more efficient anyway *)
  let bcomp_int =
    (comp_int decode'.bcomp_op bc_i.memory_data_in accu.value).:[(1, 1)]
  in
  let bucomp_int =
    (ucomp_int decode'.bucomp_op bc_i.memory_data_in accu.value).:[(1, 1)]
  in
  let comp_int = comp_int decode'.comp_op accu.value stack_i.memory_data_in in
  let ucomp_int =
    ucomp_int decode'.ucomp_op accu.value stack_i.memory_data_in
  in

  let atom_ptr tag = i.atom_table_address +: sll tag asft in
  let aofs v = sll (uresize v dbits) 3 in
  let bcofs v = sll (uresize v dbits) 2 in
  let hdrp v = v +: aofs (consti ~width:dbits (-1)) in

  let get_byte s d =
    let f n = d.:[((8 * n) + 7, 8 * n)] in
    mux s @@ Array.to_list @@ Array.init 8 ~f
  in

  let set_byte s d b =
    let f n = insert ~into:d b ~at_offset:(n * 8) in
    mux s @@ Array.to_list @@ Array.init 8 ~f
  in

  compile
    [
      mem_o.memory_read_write <--. 0;
      mem_o.memory_data_out <--. 0;
      mem_o.memory_request <--. 0;
      mem_o.memory_address <--. 0;
      (* not used *)
      bc_o.memory_read_write <--. 0;
      bc_o.memory_data_out <--. 0;
      state.switch
        [
          ( `init,
            [
              accu <-- val_unit;
              sp <-- i.stack_start_address;
              pc <-- i.bytecode_start_address;
              alloc_base <-- i.heap_start_address;
              env <-- atom_ptr (zero dbits);
              extra_args <--. 0;
              when_ i.start [ state.set_next `fetch ];
            ] );
          (* fetch instruction *)
          ( `fetch,
            [
              read_bytecode `decode;
              (* clear temporaries *)
              count <--. 0;
              clear_temps;
            ] );
          (* decode instruction *)
          ( `decode,
            [
              when_bytecode_ready (fun _ ->
                  [
                    if_
                      (reduce ~f:( |: )
                         [
                           (* try to catch stuff thats not implemented yet *)
                           decode.return;
                           decode.restart;
                           decode.offsetclosure;
                           decode.pushoffsetclosure;
                           decode.pushtrap;
                           decode.poptrap;
                           decode.raise_;
                           decode.check_signals;
                           decode.getmethod;
                           decode.getpubmet;
                           decode.getdynmet;
                           decode.event;
                           decode.break;
                           decode.reraise;
                           decode.raise_notrace;
                           decode.alu &: (decode.alu_op ==:. 4);
                           (* DIVINT *)
                           decode.alu &: (decode.alu_op ==:. 5);
                           (* MODINT *)
                           decode.c_call &: (decode.c_call_op ==:. 5);
                           (* C_CALLN *)
                           decode.branch &: (decode.branch_op ==:. 3);
                           (* SWITCH *)
                           decode.apply &: (decode.apply_op ==:. 0);
                           (* APPLY *)
                           decode.appterm &: (decode.appterm_op ==:. 0);
                           (* APPTERM - not tested *)
                           decode.makeblock &: (decode.makeblock_op ==:. 4);
                           (* MAKEFLOATBLOCK *)
                         ])
                      [ state.set_next `not_implemented ]
                    (* branch instruction *)
                    @@ elif decode.acc
                         [
                           if_ (msb decode.acc_op)
                             [ read_bytecode `acc_offset ]
                             [
                               read_stack
                                 (sp.value +: aofs decode.acc_op)
                                 `acc_set;
                             ];
                         ]
                    (* push/acc/const *)
                    @@ elif
                         ( decode.pushacc |: decode.pushconst
                         |: decode.pushgetglobal |: decode.pushgetglobalfield
                         |: decode.pushatom |: decode.pushenvacc )
                         [ push_stack_accu `pushacc ]
                    @@ elif decode.envacc
                         [
                           read_mem
                             (env.value +: aofs (decode.envacc_op +:. 1))
                             `envacc;
                         ]
                    @@ elif decode.const
                         [
                           if_ (msb decode.const_op)
                             [ read_bytecode `constint ]
                             [
                               accu <-- val_int decode.const_op;
                               state.set_next `fetch;
                             ];
                         ]
                    (* branch *)
                    @@ elif decode.branch [ read_bytecode `branch ]
                    (* pop *)
                    @@ elif decode.pop [ read_bytecode `pop ]
                    (* alu *)
                    @@ elif decode.alu
                         [
                           if_ (decode.alu_op ==:. 0)
                             [
                               (* only requires 1 operand *)
                               accu <-- val_int (negate (msbs accu.value));
                               state.set_next `fetch;
                             ]
                             [
                               (* 2 operands, through alu ... *)
                               pop_stack `alu;
                               (* ... unless div or mod which need an iterative implementation *)
                               when_ (decode.alu_op ==:. 4)
                                 [ state.set_next `invalid_instruction ];
                               (* XXX todo *)
                               when_ (decode.alu_op ==:. 5)
                                 [ state.set_next `invalid_instruction ];
                             ];
                         ]
                    (* signed comparision *)
                    @@ elif decode.comp [ pop_stack `comp ]
                    @@ elif decode.ucomp [ pop_stack `ucomp ]
                    @@ elif decode.bcomp [ read_bytecode `bcomp ]
                    @@ elif decode.bucomp [ read_bytecode `bucomp ]
                    (* offset *)
                    @@ elif decode.offsetint [ read_bytecode `offsetint ]
                    @@ elif decode.offsetref [ read_bytecode `offsetref0 ]
                    @@ elif decode.isint
                         [
                           accu <-- ures accu.value.:[(0, 0)];
                           state.set_next `fetch;
                         ]
                    (* closure(rec) *)
                    @@ elif decode.closure [ read_bytecode `closure_nvars ]
                    @@ elif decode.closurerec [ read_bytecode `closure_nfuncs ]
                    (* globals *)
                    @@ elif decode.setglobal [ read_bytecode `setglobal ]
                    @@ elif
                         (decode.getglobal |: decode.getglobalfield)
                         [ read_bytecode `getglobal_data ]
                    (* makeblock *)
                    @@ elif decode.makeblock
                         [
                           if_
                             (decode.makeblock_op ==:. 0)
                             [ read_bytecode `makeblock ]
                             [
                               makeblock_wosize <-- ures decode.makeblock_op;
                               (* size *)
                               read_bytecode `makeblock_alloc;
                             ];
                         ]
                    @@ elif decode.c_call [ push_stack env.value `c_call0 ]
                    @@ elif decode.atom
                         [
                           if_ decode.atom_op
                             [ read_bytecode `atom ]
                             [ state.set_next `atom ];
                         ]
                    @@ elif decode.apply
                         [
                           if_ decode.apply_op
                             [ state.set_next `apply_pop_stack ]
                             [ read_bytecode `apply_eargs ];
                         ]
                    @@ elif decode.appterm
                         [
                           if_ decode.appterm_op
                             [
                               temp.(0) <-- ures decode.appterm_op;
                               read_bytecode `appterm1;
                             ]
                             [ read_bytecode `appterm0 ];
                         ]
                    @@ elif decode.grab [ read_bytecode `grab ]
                    @@ elif decode.stop [ state.set_next `invalid_instruction ]
                    @@ elif decode.push_retaddr [ read_bytecode `push_retaddr0 ]
                    @@ elif decode.vectlength
                         [ read_mem (hdrp accu.value) `vectlength ]
                    @@ elif decode.getvectitem [ pop_stack `getvectitem0 ]
                    @@ elif decode.setvectitem [ pop_stack `setvectitem0 ]
                    @@ elif decode.getstringchar [ pop_stack `getstringchar0 ]
                    @@ elif decode.setstringchar [ pop_stack `setstringchar0 ]
                    @@ elif decode.boolnot
                         [
                           accu <-- val_int ~:(accu.value.:[(1, 1)]);
                           state.set_next `fetch;
                         ]
                         (* not implemented or invalid *)
                         [ state.set_next `invalid_instruction ];
                  ]);
            ] );
          ( `acc_offset,
            [
              when_bytecode_ready (fun offset ->
                  [ read_stack (sp.value +: aofs offset) `acc_set ]);
            ] );
          ( `acc_set,
            [
              when_stack_ready (fun data ->
                  [ accu <-- data; state.set_next `fetch ]);
            ] );
          ( `pushacc,
            [
              when_stack_ready (fun _ ->
                  [
                    if_ decode'.pushconst
                      [
                        if_ (msb decode'.pushconst_op)
                          [ read_bytecode `constint ]
                          [
                            accu <-- val_int decode'.pushconst_op;
                            state.set_next `fetch;
                          ];
                      ]
                    @@ elif decode'.push [ state.set_next `fetch ]
                    @@ elif decode'.pushenvacc
                         [
                           read_mem
                             (env.value +: aofs (decode'.pushenvacc_op +:. 1))
                             `envacc;
                         ]
                    @@ elif
                         (decode'.pushgetglobal |: decode'.pushgetglobalfield)
                         [ read_bytecode `getglobal_data ]
                    @@ elif decode'.pushatom
                         [
                           if_ decode'.pushatom_op
                             [ read_bytecode `atom ]
                             [ state.set_next `atom ];
                         ]
                         [
                           if_ (msb decode'.push_op)
                             [ read_bytecode `acc_offset ]
                             [
                               read_stack
                                 (sp.value +: aofs decode'.push_op)
                                 `acc_set;
                             ];
                         ];
                  ]);
            ] );
          ( `envacc,
            [
              when_mem_ready (fun data ->
                  [
                    accu <-- data;
                    state.set_next `fetch;
                    when_
                      ( decode'.envacc &: (decode'.envacc_op ==:. 4)
                      |: (decode'.pushenvacc &: (decode'.pushenvacc_op ==:. 4))
                      )
                      [
                        state.set_next `invalid_instruction;
                        (* not implemented yet! *)
                      ];
                  ]);
            ] );
          ( `pop,
            [
              when_bytecode_ready (fun offset ->
                  [ sp <-- sp.value +: aofs offset; state.set_next `fetch ]);
            ] );
          ( `constint,
            [
              when_bytecode_ready (fun data ->
                  [ accu <-- val_int data; state.set_next `fetch ]);
            ] );
          (* perform branch *)
          ( `branch,
            [
              when_bytecode_ready (fun data ->
                  [
                    pc <-- pc.value +: bcofs (data -:. 1); state.set_next `fetch;
                  ]);
            ] );
          (* alu *)
          ( `alu,
            [
              when_stack_ready (fun _ ->
                  [ accu <-- alu_int; state.set_next `fetch ]);
            ] );
          (* comparison *)
          ( `comp,
            [
              when_stack_ready (fun _ ->
                  [ accu <-- comp_int; state.set_next `fetch ]);
            ] );
          ( `ucomp,
            [
              when_stack_ready (fun _ ->
                  [ accu <-- ucomp_int; state.set_next `fetch ]);
            ] );
          (* branch with comparison *)
          ( `bcomp,
            [
              when_bytecode_ready (fun _ ->
                  [
                    if_ bcomp_int
                      [ read_bytecode `bcomp_setpc ]
                      [
                        pc <-- pc_next;
                        (* skip branch address *)
                        state.set_next `fetch;
                      ];
                  ]);
            ] );
          ( `bucomp,
            [
              when_bytecode_ready (fun _ ->
                  [
                    if_ bucomp_int
                      [ read_bytecode `bcomp_setpc ]
                      [ pc <-- pc_next; state.set_next `fetch ];
                  ]);
            ] );
          ( `bcomp_setpc,
            [
              when_bytecode_ready (fun data ->
                  [
                    pc <-- pc.value +: bcofs (data -:. 1); state.set_next `fetch;
                  ]);
            ] );
          (* makeblock *)
          ( `makeblock,
            [
              when_bytecode_ready (fun wosize ->
                  [
                    makeblock_wosize <-- wosize; read_bytecode `makeblock_alloc;
                  ]);
            ] );
          ( `makeblock_alloc,
            [
              when_bytecode_ready (fun tag ->
                  [
                    alloc_block M.black
                      (ures tag.:[(7, 0)])
                      makeblock_wosize.value `makeblock_accu;
                  ]);
            ] );
          ( `makeblock_accu,
            [
              when_mem_ready (fun _ ->
                  [
                    makeblock_accu_base <-- alloc_pointer.value;
                    count <-- count_next;
                    alloc_pointer <-- alloc_pointer_next;
                    write_mem alloc_pointer.value accu.value `makeblock_read;
                  ]);
            ] );
          ( `makeblock_read,
            [
              when_mem_ready (fun _ ->
                  [
                    if_
                      (count.value ==: makeblock_wosize.value)
                      [
                        accu <-- makeblock_accu_base.value;
                        state.set_next `fetch;
                      ]
                      [ pop_stack `makeblock_write ];
                  ]);
            ] );
          ( `makeblock_write,
            [
              when_stack_ready (fun data ->
                  [
                    count <-- count_next;
                    alloc_pointer <-- alloc_pointer_next;
                    write_mem alloc_pointer.value
                      (mux2 (count.value ==:. 0) accu.value data)
                      `makeblock_read;
                  ]);
            ] );
          (* closure/closurerec *)
          ( `closure_nfuncs,
            [
              when_bytecode_ready (fun data ->
                  [ closure_nfuncs <-- data; read_bytecode `closure_nvars ]);
            ] );
          ( `closure_nvars,
            [
              when_bytecode_ready (fun data ->
                  [
                    closure_nvars <-- data;
                    if_ (data >:. 0)
                      [ push_stack_accu `closure_alloc ]
                      [ state.set_next `closure_alloc ];
                  ]);
            ] );
          ( `closure_alloc,
            [
              when_stack_ready (fun _ ->
                  [
                    alloc_block M.black M.closure_tag closure_blksize
                      `closure_var_start;
                  ]);
            ] );
          ( `closure_var_start,
            [
              accu <-- alloc_pointer.value;
              when_mem_ready (fun _ ->
                  [
                    count <--. 0;
                    if_
                      (closure_nvars.value <>:. 0)
                      [ state.set_next `closure_var_read ]
                    @@ elif decode'.closure
                         [
                           (* select CLOSURE/CLOSUREREC *)
                           read_bytecode `closure_accu_pc1;
                         ]
                         [ state.set_next `closure_func_start ];
                  ]);
            ] );
          ( `closure_var_read,
            [ when_mem_ready (fun _ -> [ pop_stack `closure_var_write ]) ] );
          ( `closure_var_write,
            [
              when_stack_ready (fun data ->
                  let addr =
                    alloc_pointer.value
                    +: aofs (count.value +: closure_nfuncs_offs)
                  in
                  [
                    write_mem addr data `closure_var_read;
                    count <-- count_next;
                    when_
                      (count_next ==: closure_nvars.value)
                      [
                        if_ decode'.closure
                          [
                            (* select CLOSURE/CLOSUREREC *)
                            state.set_next `closure_accu_pc0;
                          ]
                          [
                            (* CLOSUREREC *) state.set_next `closure_func_start;
                          ];
                      ];
                  ]);
            ] );
          (* setup 1st pass *)
          ( `closure_func_start,
            [
              count <--. 0;
              closure_base_pc <-- pc.value;
              (* store base pc *)
              when_mem_ready (fun _ -> [ read_bytecode `closure_func_wpc ]);
            ] );
          (* write header *)
          ( `closure_func_hdr,
            let data = M.make_header (sll count.value 1) M.white M.infix_tag in
            [
              when_stack_ready (fun _ ->
                  [
                    alloc_pointer <-- alloc_pointer_next;
                    if_
                      (count.value ==: closure_nfuncs.value)
                      [ state.set_next `fetch ]
                      [ write_mem alloc_pointer.value data `closure_func_pc ];
                  ]);
            ] );
          (* read bytecode *)
          ( `closure_func_pc,
            [ when_mem_ready (fun _ -> [ read_bytecode `closure_func_wpc ]) ] );
          (* write pc+pc[x] *)
          ( `closure_func_wpc,
            [
              when_bytecode_ready (fun data ->
                  [
                    alloc_pointer <-- alloc_pointer_next;
                    write_mem alloc_pointer.value
                      (closure_base_pc.value +: bcofs data)
                      `closure_func_stack;
                  ]);
            ] );
          (* store to stack *)
          ( `closure_func_stack,
            let data =
              mux2 (count.value ==:. 0) accu.value
                (alloc_pointer.value +: (aofs (sll count.value 1) +:. 1))
              (* XXX ??? *)
            in
            [
              when_mem_ready (fun _ ->
                  [ count <-- count_next; push_stack data `closure_func_hdr ]);
            ] );
          ( `closure_accu_pc0,
            [ when_mem_ready (fun _ -> [ read_bytecode `closure_accu_pc1 ]) ] );
          ( `closure_accu_pc1,
            [
              when_bytecode_ready (fun ofs ->
                  [
                    write_mem accu.value
                      (pc.value +: bcofs (ofs -:. 1))
                      `closure_accu_pc2;
                  ]);
            ] );
          ( `closure_accu_pc2,
            [ when_mem_ready (fun _ -> [ state.set_next `fetch ]) ] );
          (* globals *)
          ( `setglobal,
            [
              when_bytecode_ready (fun ofs ->
                  [
                    write_mem
                      (i.globals_start_address +: aofs ofs)
                      accu.value `fetch;
                    accu <-- val_unit;
                  ]);
            ] );
          ( `getglobal_data,
            [
              when_bytecode_ready (fun ofs ->
                  [ read_mem (i.globals_start_address +: aofs ofs) `getglobal ]);
            ] );
          ( `getglobal,
            [
              when_mem_ready (fun data ->
                  [
                    accu <-- data;
                    if_
                      (decode'.getglobalfield |: decode'.pushgetglobalfield)
                      [ read_bytecode `getglobalfield_data ]
                      [ state.set_next `fetch ];
                  ]);
            ] );
          ( `getglobalfield_data,
            [
              when_bytecode_ready (fun ofs ->
                  [ read_mem (accu.value +: aofs ofs) `getglobalfield ]);
            ] );
          ( `getglobalfield,
            [
              when_mem_ready (fun data ->
                  [ accu <-- data; state.set_next `fetch ]);
            ] );
          (* XXX TODO: C_CALLN *)
          (`c_call0, [ when_stack_ready (fun _ -> [ read_bytecode `c_call1 ]) ]);
          ( `c_call1,
            [
              when_bytecode_ready (fun prim ->
                  [
                    c_call_prim <-- prim;
                    if_ (decode'.c_call_op ==:. 5)
                      (* not implemented properly...stack=accu, multi args *)
                      [ state.set_next `invalid_instruction ]
                      (* XXX Somehow we need to do the C-call...through the c-testbench? *)
                      [ c_call_request <--. 1; state.set_next `c_call2 ];
                  ]);
            ] );
          ( `c_call2,
            [
              when_ i.c_call_ready
                [
                  c_call_request <--. 0;
                  accu <-- i.c_call_result;
                  (* result of c-call *)
                  pop_stack `c_call3;
                ];
            ] );
          ( `c_call3,
            [
              when_stack_ready (fun _ ->
                  [
                    sp <-- sp.value +: aofs decode'.c_call_op;
                    state.set_next `fetch;
                  ]);
            ] );
          ( `offsetint,
            [
              when_bytecode_ready (fun data ->
                  [ accu <-- accu.value +: sll data 1; state.set_next `fetch ]);
            ] );
          ( `offsetref0,
            [
              when_bytecode_ready (fun data ->
                  [ temp.(0) <-- data; read_mem accu.value `offsetref1 ]);
            ] );
          ( `offsetref1,
            [
              when_mem_ready (fun data ->
                  [
                    write_mem accu.value
                      (data +: sll temp.(0).value 1)
                      `offsetref2;
                  ]);
            ] );
          ( `offsetref2,
            [
              when_mem_ready (fun _ ->
                  [ accu <-- val_unit; state.set_next `fetch ]);
            ] );
          ( `atom,
            let get_tag ofs =
              mux2
                ( decode'.atom &: decode'.atom_op
                |: (decode'.pushatom &: decode'.pushatom_op) )
                ofs (zero dbits)
            in
            [
              when_bytecode_ready (fun ofs ->
                  [ accu <-- atom_ptr (get_tag ofs); state.set_next `fetch ]);
            ] );
          (* applyX XXX TODO APPLY *)
          ( `apply_pop_stack,
            [
              when_stack_ready (fun d ->
                  [
                    shift_temp_up d;
                    count <-- count_next;
                    if_
                      (count.value ==: ures decode'.apply_op)
                      [
                        count <--. 0;
                        push_stack (val_int extra_args.value)
                          `apply_push_stack_env;
                        extra_args <-- ures (decode'.apply_op -:. 1);
                        (* update extra_args *)
                      ]
                      [ pop_stack `apply_pop_stack ];
                  ]);
            ] );
          ( `apply_push_stack_env,
            [
              when_stack_ready (fun _ ->
                  [ push_stack env.value `apply_push_stack_pc ]);
            ] );
          ( `apply_push_stack_pc,
            [
              when_stack_ready (fun _ ->
                  [ push_stack pc.value `apply_push_stack_args ]);
            ] );
          ( `apply_push_stack_args,
            [
              when_stack_ready (fun _ ->
                  [
                    shift_temp_down (zero dbits);
                    count <-- count_next;
                    if_
                      (count.value ==: ures decode'.apply_op)
                      [ read_mem accu.value `apply ]
                      [ push_stack temp.(0).value `apply_push_stack_args ];
                  ]);
            ] );
          ( `apply_eargs,
            [
              when_bytecode_ready (fun earg ->
                  [
                    extra_args
                    <-- mux2 (decode'.apply_op ==:. 0) (earg -:. 1) (zero dbits);
                    pc <-- accu.value;
                    (* XXX Code_val *)
                    state.set_next `fetch;
                  ]);
            ] );
          ( `apply,
            [
              when_mem_ready (fun pc' ->
                  [ pc <-- pc'; env <-- accu.value; state.set_next `fetch ]);
            ] );
          (* APPTERM[x] *)
          ( `appterm0,
            [
              when_bytecode_ready (fun nargs ->
                  [ temp.(0) <-- nargs; read_bytecode `appterm1 ]);
            ] );
          ( `appterm1,
            [
              when_bytecode_ready (fun slotsize ->
                  [
                    temp.(1) <-- sp.value +: aofs (slotsize -: temp.(0).value);
                    count <-- temp.(0).value -:. 1;
                    state.set_next `appterm2;
                  ]);
            ] );
          ( `appterm2,
            [
              (* read stack *)
              when_stack_ready (fun _ ->
                  [
                    if_ (count.value ==:. -1)
                      [ read_mem accu.value `appterm4 ]
                      [ read_stack (sp.value +: aofs count.value) `appterm3 ];
                  ]);
            ] );
          ( `appterm3,
            [
              (* write stack *)
              when_stack_ready (fun d ->
                  [
                    write_stack (temp.(1).value +: aofs count.value) d `appterm2;
                    count <-- count.value -:. 1;
                  ]);
            ] );
          ( `appterm4,
            [
              when_mem_ready (fun pcn ->
                  [
                    sp <-- temp.(1).value;
                    pc <-- pcn;
                    env <-- accu.value;
                    extra_args <-- extra_args.value +: temp.(0).value -:. 1;
                    state.set_next `fetch;
                  ]);
            ] );
          ( `grab,
            [
              when_bytecode_ready (fun reqd ->
                  [
                    if_
                      (extra_args.value >=: reqd)
                      [
                        extra_args <-- extra_args.value -: reqd;
                        state.set_next `fetch;
                      ]
                      [ (* XXX TODO *) state.set_next `invalid_instruction ];
                  ]);
            ] );
          ( `push_retaddr0,
            [
              when_bytecode_ready (fun ofs ->
                  [
                    push_stack (pc.value +: aofs ofs) `push_retaddr1;
                    (* XXX ofs-1??? *)
                  ]);
            ] );
          ( `push_retaddr1,
            [
              when_stack_ready (fun _ ->
                  [ push_stack env.value `push_retaddr2 ]);
            ] );
          ( `push_retaddr2,
            [
              when_stack_ready (fun _ ->
                  [ push_stack (val_int extra_args.value) `push_retaddr3 ]);
            ] );
          ( `push_retaddr3,
            [ when_stack_ready (fun _ -> [ state.set_next `fetch ]) ] );
          ( `vectlength,
            [
              when_mem_ready (fun d ->
                  [ (* XXX; double??? *) accu <-- val_int (srl d 10) ]);
            ] );
          ( `getvectitem0,
            [
              when_stack_ready (fun ofs ->
                  [ read_mem (accu.value +: aofs ofs) `getvectitem1 ]);
            ] );
          ( `getvectitem1,
            [
              when_mem_ready (fun data ->
                  [ accu <-- data; state.set_next `fetch ]);
            ] );
          ( `setvectitem0,
            [
              when_stack_ready (fun d ->
                  [ temp.(0) <-- d; pop_stack `setvectitem1 ]);
            ] );
          ( `setvectitem1,
            [
              when_stack_ready (fun d ->
                  [
                    write_mem
                      (accu.value +: aofs (int_val temp.(0).value))
                      d `fetch;
                  ]);
            ] );
          ( `setvectitem2,
            [
              when_mem_ready (fun _ ->
                  [ accu <-- val_unit; state.set_next `fetch ]);
            ] );
          ( `getstringchar0,
            [
              when_stack_ready (fun d ->
                  [ temp.(0) <-- d; read_mem accu.value `getstringchar1 ]);
            ] );
          ( `getstringchar0,
            [
              when_stack_ready (fun d ->
                  [
                    accu <-- val_int @@ get_byte temp.(0).value.:[(3, 1)] d;
                    state.set_next `fetch;
                  ]);
            ] );
          ( `setstringchar0,
            [
              when_stack_ready (fun d ->
                  [ temp.(0) <-- d; pop_stack `setstringchar1 ]);
            ] );
          ( `setstringchar1,
            [
              when_stack_ready (fun d ->
                  [ temp.(1) <-- d; read_mem accu.value `setstringchar2 ]);
            ] );
          ( `setstringchar2,
            [
              when_mem_ready (fun d ->
                  [
                    write_mem accu.value
                      (set_byte
                         temp.(1).value.:[(3, 1)]
                         d
                         temp.(0).value.:[(8, 1)])
                      `setstringchar3;
                  ]);
            ] );
          ( `setstringchar3,
            [ when_mem_ready (fun _ -> [ state.set_next `fetch ]) ] );
          (* invalid, or more likely not implemented yet *)
          (`not_implemented, [ error <--. 1 ]);
          (`invalid_instruction, [ error <--. 1 ]);
        ];
    ];

  {
    state = state.current;
    pc = pc.value;
    sp = sp.value;
    accu = accu.value;
    env = env.value;
    extra_args = extra_args.value;
    instruction =
      mux2
        (state.is `decode &: bc_i.memory_ready)
        instruction (consti ~width:8 255);
    error = error.value;
    memory_o;
    decode = decode';
    c_call_request = c_call_request.value;
    c_call_prim = c_call_prim.value;
  }
