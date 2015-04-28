open Ops.Int64

type state = 
  {
    mutable accu : t;
    mutable sp : t;
    mutable pc : t;
    mutable env : t;
    mutable extra_args : t;
  }

include Mlvalues.Make(Ops.Int64)

let mofs addr = srl addr 3L
let aofs x = sll x 3L

let header memory ptr = memory.{ Int64.to_int (mofs ptr -: 1L) }
let field memory ptr field = memory.{ Int64.to_int (mofs ptr +: field) }
let set_field memory ptr field v = memory.{ Int64.to_int (mofs ptr +: field) } <- v

let header_size memory ptr = size (header memory ptr) 
let header_colour memory ptr = colour (header memory ptr) 
let header_tag memory ptr = tag (header memory ptr) 


let fetch memory pc = 
  let ins = memory.{ Int64.to_int (mofs pc) } in
  let sel = pc &: 4L in
  sra (if sel = 0L then sll ins 32L else ins) 32L

let read_stack memory sp ofs = memory.{ Int64.to_int ((mofs sp) +: ofs) }
let write_stack memory sp ofs v = memory.{ Int64.to_int ((mofs sp) +: ofs) } <- v

let read_mem memory ptr = memory.{ Int64.to_int (mofs ptr) }
let write_mem memory ptr v = memory.{ Int64.to_int (mofs ptr) } <- v

let rec iter f t g = 
  if f = t then ()
  else
    let () = g f in
    if (f -: t) < 0L then iter (f +: 1L) t g
    else iter (f -: 1L) t g

let step exe memory state = 
  let open Instr in

  (* fetch instruction *)
  let incr_pc () = state.pc <- state.pc +: 4L in
  let instr = fetch memory state.pc in
  let () = incr_pc () in

  (* memory ops *)
  let header = header memory in
  let field = field memory in
  let set_field = set_field memory in

  (* stack ops *)
  let incr_sp () = state.sp <- state.sp +: 8L in
  let decr_sp () = state.sp <- state.sp -: 8L in
  let read_stack ofs = read_stack memory state.sp ofs in
  let write_stack ofs v = write_stack memory state.sp ofs v in
  let push_stack v = 
    decr_sp ();
    write_stack 0L v
  in
  let pop_stack () = 
    let v = read_stack 0L in
    incr_sp ();
    v
  in

  let pop_arg () =
    let v = fetch memory state.pc in
    incr_pc ();
    v
  in
  let read_arg () = fetch memory state.pc in

  let read_mem = read_mem memory in
  let write_mem = write_mem memory in

  let modify ptr fld v = write_mem (ptr +: (aofs fld)) v in

  let alloc _ _ = 0L in (* XXX *)
  let global_data = 0L in (* XXX *)
  let atom _ = 0L in (* XXX *)

  let c_call n = 
    let prim = pop_arg () in
    push_stack state.env;
    state.accu <- 
      C_runtime.run exe (Int64.to_int prim) 
        C_runtime.({ env=state.env; 
                     accu=state.accu;
                     sp=(Int64.to_int state.sp);
                     memory=memory });
    state.env <- pop_stack ();
    state.sp <- state.sp +: (aofs n)
  in
  
  let int_comp f = 
    state.accu <- val_int (if f state.accu (pop_stack()) then 1L else 0L) 
  in
  let uint_comp f =
    let msk = 0x8000_0000_0000_0000L in (* not sure... *)
    state.accu <- val_int (if f (state.accu ^: msk) (pop_stack() ^: msk) then 1L else 0L) 
  in
  let int_bcomp f = 
    if f (pop_arg()) (int_val state.accu) then
      state.pc <- state.pc +: (aofs (read_arg()))
    else 
      incr_pc ()
  in
  let uint_bcomp f = 
    let msk = 0x8000_0000_0000_0000L in (* not sure... *)
    if f (pop_arg() ^: msk) ((int_val state.accu) ^: msk)  then
      state.pc <- state.pc +: (aofs (read_arg()))
    else 
      incr_pc ()
  in

  match Enum.to_enum<opcodes> (Int64.to_int instr) with
  | ACC0 -> state.accu <- read_stack 0L
  | ACC1 -> state.accu <- read_stack 1L
  | ACC2 -> state.accu <- read_stack 2L
  | ACC3 -> state.accu <- read_stack 3L
  | ACC4 -> state.accu <- read_stack 4L
  | ACC5 -> state.accu <- read_stack 5L
  | ACC6 -> state.accu <- read_stack 6L
  | ACC7 -> state.accu <- read_stack 7L
  | ACC -> state.accu <- read_stack (pop_arg()) 
  | PUSH | PUSHACC0 -> push_stack state.accu
  | PUSHACC1 -> push_stack state.accu; state.accu <- read_stack 1L
  | PUSHACC2 -> push_stack state.accu; state.accu <- read_stack 2L
  | PUSHACC3 -> push_stack state.accu; state.accu <- read_stack 3L
  | PUSHACC4 -> push_stack state.accu; state.accu <- read_stack 4L
  | PUSHACC5 -> push_stack state.accu; state.accu <- read_stack 5L
  | PUSHACC6 -> push_stack state.accu; state.accu <- read_stack 6L
  | PUSHACC7 -> push_stack state.accu; state.accu <- read_stack 7L
  | PUSHACC -> push_stack state.accu; state.accu <- read_stack (pop_arg())
  | POP -> state.sp <- state.sp +: (aofs (pop_arg()))
  | ASSIGN -> write_stack (pop_arg()) state.accu
  | ENVACC1 -> state.accu <- field state.env 1L
  | ENVACC2 -> state.accu <- field state.env 2L
  | ENVACC3 -> state.accu <- field state.env 3L
  | ENVACC4 -> state.accu <- field state.env 4L
  | ENVACC -> state.accu <- field state.env (pop_arg())
  | PUSHENVACC1 -> push_stack state.accu; state.accu <- field state.env 1L
  | PUSHENVACC2 -> push_stack state.accu; state.accu <- field state.env 2L
  | PUSHENVACC3 -> push_stack state.accu; state.accu <- field state.env 3L
  | PUSHENVACC4 -> push_stack state.accu; state.accu <- field state.env 4L
  | PUSHENVACC -> push_stack state.accu; state.accu <- field state.env (pop_arg())
  | PUSH_RETADDR -> 
    push_stack (val_int state.extra_args);
    push_stack state.env;
    push_stack (state.pc +: (aofs (pop_arg())))
  | APPLY ->
    state.extra_args <- (pop_arg()) -: 1L;
    state.pc <- read_mem state.accu;
    state.env <- state.accu
  | APPLY1 ->
    let arg1 = pop_stack () in
    push_stack (val_int state.extra_args);
    push_stack state.env;
    push_stack state.pc;
    push_stack arg1;
    state.pc <- read_mem state.accu;
    state.env <- state.accu;
    state.extra_args <- 0L
  | APPLY2 ->
    let arg1 = pop_stack () in
    let arg2 = pop_stack () in
    push_stack (val_int state.extra_args);
    push_stack state.env;
    push_stack state.pc;
    push_stack arg2;
    push_stack arg1;
    state.pc <- read_mem state.accu;
    state.env <- state.accu;
    state.extra_args <- 1L
  | APPLY3 ->
    let arg1 = pop_stack () in
    let arg2 = pop_stack () in
    let arg3 = pop_stack () in
    push_stack (val_int state.extra_args);
    push_stack state.env;
    push_stack state.pc;
    push_stack arg3;
    push_stack arg2;
    push_stack arg1;
    state.pc <- read_mem state.accu;
    state.env <- state.accu;
    state.extra_args <- 2L
  | APPTERM ->
    let nargs = pop_arg () in
    let slotsize = pop_arg () in
    let newsp = state.sp +: (aofs (slotsize -: nargs)) in
    iter (nargs -: 1L) 0L 
      (fun i -> write_mem newsp (read_stack i));
    state.sp <- newsp;
    state.pc <- read_mem state.accu;
    state.env <- state.accu;
    state.extra_args <- state.extra_args +: nargs -: 1L
  | APPTERM1 ->
    let arg1 = pop_stack () in
    state.sp <- state.sp +: (pop_arg());
    push_stack arg1;
    state.pc <- read_mem state.accu;
    state.env <- state.accu;
  | APPTERM2 ->
    let arg1 = pop_stack () in
    let arg2 = pop_stack () in
    state.sp <- state.sp +: (pop_arg());
    push_stack arg2;
    push_stack arg1;
    state.pc <- read_mem state.accu;
    state.env <- state.accu;
    state.extra_args <- state.extra_args +: 1L
  | APPTERM3 ->
    let arg1 = pop_stack () in
    let arg2 = pop_stack () in
    let arg3 = pop_stack () in
    state.sp <- state.sp +: (pop_arg());
    push_stack arg3;
    push_stack arg2;
    push_stack arg1;
    state.pc <- read_mem state.accu;
    state.env <- state.accu;
    state.extra_args <- state.extra_args +: 2L
  | RETURN ->
    state.sp <- state.sp +: (aofs (pop_arg()));
    if state.extra_args > 0L then begin
      state.extra_args <- state.extra_args -: 1L;
      state.pc <- read_mem state.accu;
      state.env <- state.accu;
    end else begin
      state.pc <- pop_stack ();
      state.env <- pop_stack ();
      state.extra_args <- int_val (pop_stack ())
    end
  | RESTART ->
    let num_args = header_size memory state.env -: 2L in
    iter (num_args -: 1L) 0L 
      (fun i -> push_stack (field state.env (i +: 2L)));
    state.env <- field state.env 1L;
    state.extra_args <- state.extra_args +: num_args
  | GRAB ->
    let required = pop_arg () in
    if (state.extra_args >= required) then 
      state.extra_args <- state.extra_args -: required
    else begin
      let num_args = 1L +: state.extra_args in
      state.accu <- alloc (num_args +: 2L) closure_tag;
      set_field state.accu 1L state.env;
      iter 0L (num_args -: 1L) 
        (fun i -> set_field state.accu (i +: 2L) (pop_stack ()));
      write_mem state.accu (state.pc -: (aofs 3L));
      state.pc <- pop_stack ();
      state.env <- pop_stack ();
      state.extra_args <- int_val (pop_stack ())
    end
  | CLOSURE ->
    let nvars = pop_arg () in
    let () = if nvars > 0L then push_stack state.accu in
    state.accu <- alloc (nvars +: 1L) closure_tag;
    iter 0L (nvars -: 1L) 
      (fun i -> set_field state.accu (i +: 1L) (pop_stack()));
    state.accu <- state.pc +: (aofs (pop_arg()))
  | CLOSUREREC ->
    let nfuncs = pop_arg () in
    let nvars = pop_arg () in
    let blksize = (nfuncs *: 2L) -: 1L +: nvars in
    let () = if nvars > 0L then push_stack state.accu in
    state.accu <- alloc blksize closure_tag;
    iter 0L (nvars -: 1L) 
      (fun i -> set_field state.accu ((nfuncs *: 2L) -: 1L +: i) (pop_stack()));
    set_field state.accu 0L (state.pc +: (aofs (pop_arg())));
    push_stack state.accu;
    iter 1L nfuncs 
      (fun i -> 
        set_field state.accu (1L +: (i *: 2L)) (make_header (i *: 2L) white infix_tag);
        set_field state.accu (2L +: (i *: 2L)) (state.pc +: (aofs (pop_arg())));
        push_stack (state.accu +: (aofs (1L +: (i *: 2L)))));
  | PUSHOFFSETCLOSURE -> push_stack state.accu; state.accu <- state.env +: (aofs (pop_arg()))
  | OFFSETCLOSURE -> state.accu <- state.env +: (aofs (pop_arg()))
  | PUSHOFFSETCLOSUREM2 -> push_stack state.accu; state.accu <- state.env -: (aofs 2L)
  | OFFSETCLOSUREM2 -> state.accu <- state.env -: (aofs 2L)
  | PUSHOFFSETCLOSURE0 -> push_stack state.accu; state.accu <- state.env
  | OFFSETCLOSURE0 -> state.accu <- state.env
  | PUSHOFFSETCLOSURE2 -> push_stack state.accu; state.accu <- state.env +: (aofs 2L)
  | OFFSETCLOSURE2 -> state.accu <- state.env +: (aofs 2L)
  | PUSHGETGLOBAL -> push_stack state.accu; state.accu <- field global_data (pop_arg())
  | GETGLOBAL -> state.accu <- field global_data (pop_arg())
  | PUSHGETGLOBALFIELD -> 
    push_stack state.accu; 
    state.accu <- field global_data (pop_arg());
    state.accu <- field state.accu (pop_arg())
  | GETGLOBALFIELD -> 
    state.accu <- field global_data (pop_arg());
    state.accu <- field state.accu (pop_arg())
  | SETGLOBAL -> modify global_data (pop_arg()) state.accu; state.accu <- val_unit
  | PUSHATOM0 -> push_stack state.accu; state.accu <- atom 0L
  | ATOM0 -> state.accu <- atom 0L
  | PUSHATOM -> push_stack state.accu; state.accu <- atom (pop_arg())
  | ATOM -> state.accu <- atom (pop_arg())
  | MAKEBLOCK ->
    let wosize = pop_arg () in
    let tag = pop_arg () in
    let block = alloc wosize tag in
    set_field block 0L state.accu;
    iter 1L (wosize -: 1L) 
      (fun i -> set_field block i (pop_stack()));
    state.accu <- block
  | MAKEBLOCK1 ->
    let tag = pop_arg () in
    let block = alloc 1L tag in
    set_field block 0L state.accu;
    state.accu <- block
  | MAKEBLOCK2 ->
    let tag = pop_arg () in
    let block = alloc 2L tag in
    set_field block 0L state.accu;
    set_field block 1L (pop_stack());
    state.accu <- block
  | MAKEBLOCK3 ->
    let tag = pop_arg () in
    let block = alloc 3L tag in
    set_field block 0L state.accu;
    set_field block 1L (pop_stack());
    set_field block 2L (pop_stack());
    state.accu <- block
  | MAKEFLOATBLOCK ->
    let size = pop_arg () in
    let block = alloc size double_array_tag in
    set_field block 0L state.accu;
    iter 1L (size -: 1L) 
      (fun i ->
        set_field block i (pop_stack()));
    state.accu <- block
  | GETFIELD0 -> state.accu <- field state.accu 0L
  | GETFIELD1 -> state.accu <- field state.accu 1L
  | GETFIELD2 -> state.accu <- field state.accu 2L
  | GETFIELD3 -> state.accu <- field state.accu 3L
  | GETFIELD -> state.accu <- field state.accu (pop_arg())
  | GETFLOATFIELD -> 
    let d = field state.accu (pop_arg()) in (* XXX; wont work for 32 bit *)
    state.accu <- alloc 1L double_tag;
    set_field state.accu 0L d
  | SETFIELD0 -> modify state.accu 0L (pop_stack()); state.accu <- val_unit
  | SETFIELD1 -> modify state.accu 1L (pop_stack()); state.accu <- val_unit
  | SETFIELD2 -> modify state.accu 2L (pop_stack()); state.accu <- val_unit
  | SETFIELD3 -> modify state.accu 3L (pop_stack()); state.accu <- val_unit
  | SETFIELD -> modify state.accu (pop_arg()) (pop_stack()); state.accu <- val_unit
  | SETFLOATFIELD -> modify state.accu (pop_arg()) (pop_stack()); state.accu <- val_unit
  | VECTLENGTH -> state.accu <- val_int (header_size memory state.accu)
  | GETVECTITEM -> state.accu <- field state.accu (pop_stack())
  | SETVECTITEM ->
    let fld = pop_stack () in
    let v = pop_stack () in
    modify state.accu fld v
  | GETSTRINGCHAR -> failwith "GETSTRINGCHAR"
  | SETSTRINGCHAR -> failwith "SETSTRINGCHAR"
  | BRANCH -> state.pc <- state.pc +: (aofs (read_arg()))
  | BRANCHIF -> if state.accu <> val_false then state.pc <- state.pc +: (aofs (read_arg()))
  | BRANCHIFNOT -> if state.accu = val_false then state.pc <- state.pc +: (aofs (read_arg()))
  | SWITCH -> failwith "SWITCH"
  | BOOLNOT -> state.accu <- val_false +: val_true -: state.accu
  | PUSHTRAP -> failwith "PUSHTRAP"
  | POPTRAP -> failwith "POPTRAP"
  | RAISE_NOTRACE -> failwith "RAISE_NOTRACE"
  | RERAISE -> failwith "RERAISE"
  | RAISE -> failwith "RAISE"
  | CHECK_SIGNALS -> failwith "CHECK_SIGNALS"
  | C_CALL1 -> c_call 0L
  | C_CALL2 -> c_call 1L
  | C_CALL3 -> c_call 2L
  | C_CALL4 -> c_call 3L
  | C_CALL5 -> c_call 4L
  | C_CALLN -> failwith "C_CALLN"
  | CONST0 -> state.accu <- val_int 0L
  | CONST1 -> state.accu <- val_int 1L
  | CONST2 -> state.accu <- val_int 2L
  | CONST3 -> state.accu <- val_int 3L
  | PUSHCONST0 -> push_stack state.accu; state.accu <- val_int 0L
  | PUSHCONST1 -> push_stack state.accu; state.accu <- val_int 1L
  | PUSHCONST2 -> push_stack state.accu; state.accu <- val_int 2L
  | PUSHCONST3 -> push_stack state.accu; state.accu <- val_int 3L
  | PUSHCONSTINT -> push_stack state.accu; state.accu <- val_int (pop_arg())
  | CONSTINT -> state.accu <- val_int (pop_arg())
  | NEGINT -> state.accu <- 2L -: state.accu
  | ADDINT -> state.accu <- state.accu +: (pop_stack()) -: 1L
  | SUBINT -> state.accu <- state.accu -: (pop_stack()) +: 1L
  | MULINT -> state.accu <- val_int (int_val state.accu *: int_val (pop_stack()))
  | DIVINT -> 
    let divisor = int_val (pop_stack ()) in
    if divisor = 0L then failwith "DIVINT by 0" (* XXX *)
    else
      state.accu <- val_int (int_val state.accu /: divisor)
  | MODINT -> 
    let divisor = int_val (pop_stack ()) in
    if divisor = 0L then failwith "MODINT by 0" (* XXX *)
    else
      state.accu <- val_int (int_val state.accu %: divisor)
  | ANDINT -> state.accu <- state.accu &: pop_stack()
  | ORINT -> state.accu <- state.accu |: pop_stack()
  | XORINT -> state.accu <- (state.accu ^: pop_stack()) |: 1L
  | LSLINT -> state.accu <- (sll (state.accu -: 1L) (pop_stack())) +: 1L
  | LSRINT -> state.accu <- (srl (state.accu -: 1L) (pop_stack())) |: 1L
  | ASRINT -> state.accu <- (sra (state.accu -: 1L) (pop_stack())) |: 1L
  | EQ -> int_comp (=)
  | NEQ -> int_comp (<>)
  | LTINT -> int_comp (<)
  | LEINT -> int_comp (<=)
  | GTINT -> int_comp (>)
  | GEINT -> int_comp (>=)
  | ULTINT -> uint_comp (<)
  | UGEINT -> uint_comp (>=)
  | BEQ -> int_comp (=)
  | BNEQ -> int_comp (<>)
  | BLTINT -> int_bcomp (<)
  | BLEINT -> int_bcomp (<=)
  | BGTINT -> int_bcomp (>)
  | BGEINT -> int_bcomp (>=)
  | BULTINT -> uint_bcomp (<)
  | BUGEINT -> uint_bcomp (>=)
  | OFFSETINT -> state.accu <- state.accu +: (sll (pop_arg()) 1L)
  | OFFSETREF -> set_field state.accu 0L (field state.accu 0L +: (sll (pop_arg()) 1L))
  | ISINT -> state.accu <- int_val (state.accu &: 1L)
  | GETMETHOD -> failwith "GETMETHOD"
  | GETPUBMET -> failwith "GETPUBMET"
  | GETDYNMET -> failwith "GETDYNMET"
  | STOP -> failwith "STOP"
  | EVENT -> failwith "EVENT"
  | BREAK -> failwith "BREAK"

