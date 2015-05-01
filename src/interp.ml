(*
 * TODO;
 *
 *  - not_implemented; 
 *      getmethod, getpubmet, getdynmet
 *      stop, event, break
 *
 *  - not fully implemented
 *      pushtrap, poptrap, raise_notrace, reraise, raise_
 *      check_signals
 *      c_call
 *
 *  - need to indicate 'stop' to caller
 *)

type machine_register = 
  (* zinc registers *)
  [ `accu | `env | `pc | `sp | `extra_args | `trapsp
  (* other state *)
  | `global_data | `atom_table | `alloc_base ]
type cache = [ `stack | `program | `mem ]

let string_of_mach_reg = function
  | `accu -> "accu"
  | `env -> "env"
  | `pc -> "pc"
  | `sp -> "sp"
  | `extra_args -> "extra_args"
  | `trapsp -> "trapsp"
  | `global_data -> "global_data"
  | `atom_table -> "atom_table"
  | `alloc_base -> "alloc_base"

let string_of_cache = function
  | `stack -> "stack"
  | `program -> "program"
  | `mem -> "mem"

module type State = sig

  type st
  include Ops.S 

  (* initial state *)

  val initial : unit -> st

  (* machine registers *)

  val get_reg : st -> machine_register -> t * st
  val set_reg : st -> machine_register -> t -> st

  (* memory access *)

  val get_mem : st -> cache -> t -> t * st
  val set_mem : st -> cache -> t -> t -> st

  (* c-calls *)

  val c_call : t -> st -> t * st

  (* control *)
  
  val cond : st -> t -> (st -> unit * st) -> (st -> unit * st) -> st
  val iter_up : st -> t -> t -> (t -> st -> unit * st) -> st
  val iter_dn : st -> t -> t -> (t -> st -> unit * st) -> st

  (* debugging *)

  val string_of_value : t -> string

end

module State_eval = struct

  type st = 
    {
      (* zinc registers *)
      accu : int64;
      env : int64;
      pc : int64;
      sp : int64;
      extra_args : int64;
      trapsp : int64;
      (* other state *)
      global_data : int64;
      atom_table : int64;
      alloc_base : int64;
      (* memory *)
      memory : (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t;
      (* executable *)
      exe : Load.bytecode_exe;
    }

  let initial () = 
    {
      accu = 1L;
      env = 0L;
      pc = 0L;
      sp = 0L;
      extra_args = 0L;
      trapsp = 0L;
      global_data = 0L;
      atom_table = 0L;
      alloc_base = 0L;
      memory = Bigarray.(Array1.create int64 c_layout 0);
      exe = Load.empty;
    }

  let get_reg st r = 
    match r with
    | `accu -> st.accu, st
    | `env -> st.env, st
    | `pc -> st.pc, st
    | `sp -> st.sp, st
    | `extra_args -> st.extra_args, st
    | `trapsp -> st.trapsp, st
    | `global_data -> st.global_data, st
    | `atom_table -> st.atom_table, st
    | `alloc_base -> st.alloc_base, st

  let set_reg st r v = 
    match r with
    | `accu -> { st with accu = v }
    | `env -> { st with env = v }
    | `pc -> { st with pc = v } 
    | `sp -> { st with sp = v }
    | `extra_args -> { st with extra_args = v }
    | `trapsp -> { st with trapsp = v }
    | `global_data -> failwith "cannot write to global data pointer"
    | `atom_table -> failwith "cannot write to atom table pointer"
    | `alloc_base -> { st with alloc_base = v }

  let get_mem st cache adr = 
    try
      st.memory.{Int64.to_int adr}, st
    with Invalid_argument x ->
      raise (Invalid_argument ("get_mem {" ^ Int64.to_string adr ^ "} " ^ x))

  let set_mem st cache adr v = 
    try
      st.memory.{Int64.to_int adr} <- v; st 
    with Invalid_argument x ->
      raise (Invalid_argument ("set_mem {" ^ Int64.to_string adr ^ "} " ^ x))

  let c_call prim st = 
    (C_runtime.(run 
      st.exe
      (Int64.to_int prim)
      {
        env=st.env;
        accu=st.accu;
        sp=Int64.to_int st.sp;
        memory=st.memory;
      }), st)

  let cond st c t f = 
    snd @@ if c <> 0L then t st else f st

  let iter_up st m n f = 
    let rec g st i = 
      if i >= n then st
      else
        let _, st = f i st in
        g st (Int64.add i 1L)
    in
    (g st m)

  let iter_dn st m n f = 
    let rec g st i = 
      if i < n then st
      else
        let _, st = f i st in
        g st (Int64.sub i 1L)
    in
    (g st m)

  include Ops.Int64

  let string_of_value = Int64.to_string

end

module State_poly = struct

  type cmd = 
    | Get_reg of int * machine_register 
    | Set_reg of machine_register * t
    | Get_mem of int * cache * t
    | Set_mem of cache * t * t
    | Cond of t * cmd list * cmd list
    | Iter of bool * int * t * t * cmd list
    | C_call of t 
  and t = 
    | Op of string * t * t
    | Val of int
    | Const of int
    | C_call_result 
  and st = 
    {
      id : int;
      cmd : cmd list;
    }
  
  let initial () = 
    {
      id = 0;
      cmd = [];
    }

  let get_reg st r = 
    let x = Get_reg(st.id,r) in 
    (Val st.id), { id=st.id+1; cmd=x::st.cmd }

  let set_reg st r v = { st with cmd = Set_reg(r, v) :: st.cmd }

  let get_mem st c adr = 
    let x = Get_mem(st.id, c, adr) in 
    (Val st.id), { id=st.id+1; cmd=x::st.cmd }

  let set_mem st c adr v = { st with cmd = Set_mem(c, adr, v) :: st.cmd }

  let c_call prim st = (C_call_result, { st with cmd = C_call(prim) :: st.cmd })

  let cond st c t f = 
    let (),st_t = t {id = st.id; cmd = []} in
    let (),st_f = f {id = st_t.id; cmd = []} in
    { id = st_f.id; cmd = Cond(c,st_t.cmd,st_f.cmd)::st.cmd }

  let iter dirn st m n f =
    let tmp_id = st.id in
    let _, st_body = f (Val tmp_id) { id = st.id+1; cmd = [] } in (* eval the body *)
    { st_body with cmd = Iter(dirn, tmp_id, m, n, st_body.cmd) :: st.cmd }

  let iter_up = iter true
  let iter_dn = iter false

  let const i = Const(i)
  let zero = const 0
  let one = const 1

  let ( +: ) a b = Op("+", a, b)
  let ( -: ) a b = Op("-", a, b)
  let ( *: ) a b = Op("*", a, b)
  let ( /: ) a b = Op("/", a, b)
  let ( %: ) a b = Op("%", a, b)
  let ( &: ) a b = Op("&", a, b)
  let ( |: ) a b = Op("|", a, b)
  let ( ^: ) a b = Op("^", a, b)
  let ( ~: ) a   = Op("~", a, a)

  let sll a b = Op("<<",  a, b)
  let srl a b = Op(">>",  a, b)
  let sra a b = Op(">>+", a, b)

  let (==:) a b = Op("==", a, b)
  let (<>:) a b = Op("<>", a, b)

  let (<+)  a b = Op("<+",  a, b)
  let (<=+) a b = Op("<=+", a, b)
  let (>+)  a b = Op(">+",  a, b)
  let (>=+) a b = Op(">=+", a, b)

  let (<:)  a b = Op("<",  a, b)
  let (<=:) a b = Op("<=", a, b)
  let (>:)  a b = Op(">",  a, b)
  let (>=:) a b = Op(">=", a, b)

  let rec string_of_value = function
    | Op(o,a,b) -> "(" ^ string_of_value a ^ o ^ string_of_value b ^ ")"
    | Val(i) -> "_" ^ string_of_int i
    | Const i -> string_of_int i
    | C_call_result -> "c_call_result"

  let rec print lev st = 
    let open Printf in
    let pad = String.make lev ' ' in
    List.iter (function
      | Get_reg(i,r) -> printf "%s_%i = %s;\n" pad i (string_of_mach_reg r)
      | Set_reg(r,v) -> printf "%s%s = %s;\n" pad (string_of_mach_reg r) (string_of_value v)
      | Get_mem(i,c,a) -> printf "%s_%i = %s[%s];\n" pad i (string_of_cache c) (string_of_value a)
      | Set_mem(c,a,v) -> printf "%s%s[%s] = %s;\n" pad
        (string_of_cache c) (string_of_value a) (string_of_value v)
      | Cond(c,t,f) -> 
        printf "%sif %s then\n" pad (string_of_value c); 
        print (lev+2) t;
        printf "%selse\n" pad;
        print (lev+2) f;
        printf "%send\n" pad
      | Iter(d, i, m, n, b) ->
        printf "%sfor _%i=[%s %s %s] do\n" pad i (string_of_value m) 
          (if d then "to" else "downto") (string_of_value n);
        print (lev+2) b;
        printf "%sdone\n" pad
      | C_call(prim) ->
        printf "%sc_call prim=%s\n" pad (string_of_value prim)
      )
      (List.rev st)

  let print st = print 0 st.cmd

end

module type Monad = sig
  module S : State

  type 'a t = S.st -> ('a * S.st)
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>) : 'a t -> 'b t -> 'b t

  val if_ : S.t -> unit t -> unit t -> unit t
  val for_up : S.t -> S.t -> (S.t -> unit t) -> unit t
  val for_dn : S.t -> S.t -> (S.t -> unit t) -> unit t

  val run : 'a t -> 'a * S.st
  val step : S.st -> 'a t -> S.st

  val trace : bool
  val debug : string -> unit t

  val write_reg : machine_register -> S.t -> unit t
  val read_reg : machine_register -> S.t t
  val modify_reg : machine_register -> (S.t -> S.t) -> unit t

  val read_mem : cache -> S.t -> S.t t
  val write_mem : cache -> S.t -> S.t -> unit t

  val c_call : S.t -> S.t t

  val read_bytecode : S.t -> S.t t

end 
  
module Monad(T : sig val trace : bool end)(S : State) = struct
  module S = S

  type 'a t = S.st -> ('a * S.st)

  let bind m f =
    fun s ->
      let x, s = m s in
      f x s

  let return a = fun s -> (a, s)

  let run m = m (S.initial ())

  let step st m = snd (m st)

  let (>>=) = bind
  let (>>) m f = bind m (fun _ -> f)

  let debug s st = ((output_string stdout s; flush stdout), st)

  let if_ c t f st = 
    ((), S.cond st c t f)

  let for_up m n f st = ((), S.iter_up st m n f)
  let for_dn m n f st = ((), S.iter_dn st m n f)

  let read_reg r st = S.get_reg st r

  let write_reg r v st = ((), S.set_reg st r v)

  let c3, c32 = S.const 3, S.const 32

  let read_mem cache adr st = 
    S.get_mem st cache (S.srl adr c3) 

  let write_mem cache adr value st = 
    ((), S.set_mem st cache (S.srl adr c3) value)

  let c_call prim st = S.c_call prim st

  (* move logic into state *)
  let read_bytecode adr st = 
    let open S in
    let ins,st = S.get_mem st `program (srl adr c3) in
    let sel = adr &: (const 4) in
    let ins = sra (if sel = zero then sll ins c32 else ins) c32 in
    (ins, st)

  (* XXX DEBUG XXX *)
  let trace = T.trace

  let read_reg arg = 
    if trace then
      read_reg arg >>= fun v -> 
      debug ("read_reg " ^ string_of_mach_reg arg ^ " " ^ S.string_of_value v ^ "\n") >> 
      return v
    else
      read_reg arg

  let write_reg arg v = 
    if trace then
      debug ("write_reg " ^ string_of_mach_reg arg ^ " " ^ 
                            S.string_of_value v ^"\n") >> write_reg arg v
    else
      write_reg arg v

  let read_mem cache adr = 
    if trace then
      read_mem cache adr >>= fun v ->
      debug ("read_mem " ^ string_of_cache cache ^ " " ^ S.string_of_value adr ^ 
                                                   " " ^ S.string_of_value v ^ "\n") >>
      return v
    else
      read_mem cache adr

  let write_mem cache adr v = 
    if trace then
      debug ("write_mem " ^ string_of_cache cache ^ " " ^ S.string_of_value adr ^ 
                                                    " " ^ S.string_of_value v ^ "\n") >>
      write_mem cache adr v
    else
      write_mem cache adr v

  let if_ c a b = 
    if trace then debug ("if_ " ^ S.string_of_value c ^ "\n") >> if_ c a b
    else if_ c a b

  let for_up m n f = 
    if trace then
      debug ("for_up " ^ S.string_of_value m ^ ".." ^ S.string_of_value n ^ "\n") >>
      for_up m n f
    else
      for_up m n f

  let for_dn m n f = 
    if trace then
      debug ("for_dn " ^ S.string_of_value m ^ ".." ^ S.string_of_value n ^ "\n") >>
      for_dn m n f
    else
      for_dn m n f

  let c_call prim = 
    if trace then
      debug ("c_call prim=%i\n" ^ S.string_of_value prim ^ "\n") >>
      c_call prim
    else
      c_call prim

  let read_bytecode adr = 
    if trace then
      read_bytecode adr >>= fun v ->
      debug ("read_bytecode " ^ S.string_of_value adr ^ " " ^ S.string_of_value v ^ "\n") >>
      return v
    else
      read_bytecode adr

  (* derived *)

  let modify_reg r f = 
    read_reg r >>= fun v -> write_reg r (f v)

end 

module Opcodes(M : Monad) = struct

  open M
  open S

  (******************************************************************)
  (* utils *)

  let incr r n = modify_reg r (fun v -> v +: n)
  let decr r n = modify_reg r (fun v -> v -: n)

  let incr_sp = incr `sp (const 8)
  let decr_sp = decr `sp (const 8)

  let incr_pc = incr `pc (const 4)
  let decr_pc = decr `pc (const 4)

  let c2, c3 = const 2, const 3

  let aofs x = sll x c3
  let pcofs x = sll x c2
  let mofs x = srl x c3

  let read_bytecode ofs = 
    read_reg `pc >>= fun pc ->
    read_bytecode (pc +: (sll ofs c2))

  let read_stack ofs = 
    read_reg `sp >>= fun sp -> 
    read_mem `stack (sp +: aofs ofs)

  let write_stack ofs value = 
    read_reg `sp >>= fun sp ->
    write_mem `stack (sp +: aofs ofs) value

  let push_stack value = 
    decr_sp >> write_stack zero value

  let pop_stack = 
    read_stack zero >>= fun arg ->
    incr_sp >>
    return arg

  let rec pop_stack_n n = 
    if n=0 then return []
    else 
      pop_stack >>= fun a -> 
      pop_stack_n (n-1) >>= 
      fun l -> return (a::l)

  let rec push_stack_n e = 
    match e with
    | [] -> return ()
    | h::t -> push_stack h >> push_stack_n t

  let pop_arg = 
    read_bytecode zero >>= fun arg ->
    incr_pc >>
    return arg

  let push_stack_accu = read_reg `accu >>= push_stack 

  let copy_modify_reg from to_ f = 
    read_reg from >>= fun x -> return (f x) >>= write_reg to_

  include Mlvalues.Make(M.S)

  let header ptr = read_mem `mem (ptr -: (aofs one))
  let field ptr fld = read_mem `mem (ptr +: (aofs fld))
  let set_field ptr fld v = write_mem `mem (ptr +: (aofs fld)) v

  let modify_field = set_field

  let alloc size tag = 
    read_reg `alloc_base >>= fun base ->
    write_reg `alloc_base (base +: aofs (size +: one)) >>
    write_mem `mem base (make_header size white tag) >>
    return (base +: aofs one)

  let check_stacks = return ()

  let not_implemented st = failwith "not implemented"

  let raise_error _ = return ()

  let something_to_do = zero

  (******************************************************************)
  (* XXX DEBUG XXX *)

  let alloc size tag = 
    if trace then debug ("alloc " ^ string_of_value size ^ "\n") >> alloc size tag
    else alloc size tag

  let pop_arg = 
    if trace then debug ("pop_arg\n") >> pop_arg 
    else pop_arg

  (******************************************************************)
  (* Basic stack operations *)

  (*
    Instruct(ACC0):
      accu = sp[0]; Next;
    ...
    Instruct(ACC7):
      accu = sp[7]; Next;

    Instruct(PUSH): Instruct(PUSHACC0):
      *--sp = accu; Next;
    Instruct(PUSHACC1):
      *--sp = accu; accu = sp[1]; Next;
    ...
    Instruct(PUSHACC7):
      *--sp = accu; accu = sp[7]; Next;

    Instruct(PUSHACC):
      *--sp = accu;
      /* Fallthrough */
    Instruct(ACC):
      accu = sp[*pc++];
      Next;
  *)

  let accn i = read_stack i >>= write_reg `accu 

  let acc = pop_arg >>= accn

  let push = push_stack_accu

  let pushaccn arg = 
    push_stack_accu >>
    read_stack arg >>= fun data ->
    write_reg `accu data

  let pushacc = pop_arg >>= pushaccn

  (*
    Instruct(POP):
      sp += *pc++;
      Next;
  *)
  let pop = pop_arg >>= fun ofs -> modify_reg `sp (fun sp -> sp +: (aofs ofs))

  (*
    Instruct(ASSIGN):
      sp[*pc++] = accu;
      accu = Val_unit;
      Next;
  *)
  let assign = 
    pop_arg >>= fun ofs -> 
    read_reg `accu >>= write_stack ofs >>
    write_reg `accu val_unit

  (************************************************************)
  (* Access in heap-allocated environment *)

  (*
    Instruct(ENVACC1):
      accu = Field(env, 1); Next;
    ...
    Instruct(ENVACC4):
      accu = Field(env, 4); Next;

    Instruct(PUSHENVACC1):
      *--sp = accu; accu = Field(env, 1); Next;
    ...
    Instruct(PUSHENVACC4):
      *--sp = accu; accu = Field(env, 4); Next;

    Instruct(PUSHENVACC):
      *--sp = accu;
      /* Fallthrough */
    Instruct(ENVACC):
      accu = Field(env, *pc++);
      Next;
  *)
  let envaccn i = 
    read_reg `env >>= fun ptr -> field ptr i >>= write_reg `accu

  let envacc = pop_arg >>= envaccn

  let pushenvaccn i = push_stack_accu >> envaccn i

  let pushenvacc = pop_arg >>= pushenvaccn
  
  (************************************************************)
  (* Function application *)

  (*
    Instruct(PUSH_RETADDR): {
      sp -= 3;
      sp[0] = (value) (pc + *pc);
      sp[1] = env;
      sp[2] = Val_long(extra_args);
      pc++;
      Next;
    }
  *)
  let push_retaddr = 
    read_reg `extra_args >>= fun eargs -> push_stack (val_int eargs) >>
    read_reg `env >>= push_stack >>
    read_reg `pc >>= fun pc -> pop_arg >>= fun ofs -> 
    push_stack (pc +: (pcofs ofs))

  (*
    Instruct(APPLY): {
      extra_args = *pc - 1;
      pc = Code_val(accu);
      env = accu;
      goto check_stacks;
    }
  *)
  let apply = 
    pop_arg >>= fun eargs -> write_reg `extra_args (eargs -: one) >>
    read_reg `accu >>= fun accu -> read_mem `mem accu >>= write_reg `pc >>
    write_reg `env accu

  (*
    .....
    Instruct(APPLY3): {
      value arg1 = sp[0];
      value arg2 = sp[1];
      value arg3 = sp[2];
      sp -= 3;
      sp[0] = arg1;
      sp[1] = arg2;
      sp[2] = arg3;
      sp[3] = (value)pc;
      sp[4] = env;
      sp[5] = Val_long(extra_args);
      pc = Code_val(accu);
      env = accu;
      extra_args = 2;
      goto check_stacks;
    }
  *)
  let applyn n = 
    pop_stack_n n >>= fun args ->
    read_reg `extra_args >>= fun earg -> push_stack (val_int earg) >>
    read_reg `env >>= push_stack >>
    read_reg `pc >>= push_stack >>
    push_stack_n (List.rev args) >>
    read_reg `accu >>= fun accu ->
    read_mem `mem accu >>= write_reg `pc >> 
    write_reg `env accu >>
    write_reg `extra_args (const (n-1))
  
  (*
    Instruct(APPTERM): {
      int nargs = *pc++;
      int slotsize = *pc;
      value * newsp;
      int i;
      /* Slide the nargs bottom words of the current frame to the top
         of the frame, and discard the remainder of the frame */
      newsp = sp + slotsize - nargs;
      for (i = nargs - 1; i >= 0; i--) newsp[i] = sp[i];
      sp = newsp;
      pc = Code_val(accu);
      env = accu;
      extra_args += nargs - 1;
      goto check_stacks;
    }
  *)
  let appterm = 
    pop_arg >>= fun nargs ->
    pop_arg >>= fun slotsize ->
    read_reg `sp >>= fun sp ->
    let newsp = slotsize -: nargs in
    for_dn (nargs -: one) zero
      (fun i -> read_stack i >>= write_stack (i +: newsp)) >>
    write_reg `sp (sp +: aofs newsp) >>
    read_reg `accu >>= fun accu -> read_mem `mem accu >>= write_reg `pc >>
    write_reg `env accu >>
    modify_reg `extra_args (fun eargs -> eargs +: (nargs -: one)) >>
    check_stacks
  
  (*
    ...
    Instruct(APPTERM3): {
      value arg1 = sp[0];
      value arg2 = sp[1];
      value arg3 = sp[2];
      sp = sp + *pc - 3;
      sp[0] = arg1;
      sp[1] = arg2;
      sp[2] = arg3;
      pc = Code_val(accu);
      env = accu;
      extra_args += 2;
      goto check_stacks;
    }
  *)
  let apptermn n = 
    pop_stack_n n >>= fun args ->
    pop_arg >>= fun ofs ->
    modify_reg `sp (fun sp -> sp +: aofs (ofs -: const n)) >>
    push_stack_n (List.rev args) >>
    read_reg `accu >>= fun accu -> read_mem `mem accu >>= write_reg `pc >>
    write_reg `env accu >>
    modify_reg `extra_args (fun earg -> earg +: const (n-1)) >>
    check_stacks

  (*
    Instruct(RETURN): {
      sp += *pc++;
      if (extra_args > 0) {
        extra_args--;
        pc = Code_val(accu);
        env = accu;
      } else {
        pc = (code_t)(sp[0]);
        env = sp[1];
        extra_args = Long_val(sp[2]);
        sp += 3;
      }
      Next;
    }
  *)
  let return_ =
    pop_arg >>= fun ofs -> modify_reg `sp (fun sp -> sp +: aofs ofs) >>
    read_reg `extra_args >>= fun earg ->
    if_ (earg >+ zero) begin
      modify_reg `extra_args (fun earg -> earg -: one) >>
      read_reg `accu >>= fun accu -> read_mem `mem accu >>= write_reg `pc >>
      write_reg `env accu
    end begin
      pop_stack >>= write_reg `pc >>
      pop_stack >>= write_reg `env >>
      pop_stack >>= fun eargs -> write_reg `extra_args (int_val earg)
    end

  (*
    Instruct(RESTART): {
      int num_args = Wosize_val(env) - 2;
      int i;
      sp -= num_args;
      for (i = 0; i < num_args; i++) sp[i] = Field(env, i + 2);
      env = Field(env, 1);
      extra_args += num_args;
      Next;
    }
  *)
  let restart =
    read_reg `env >>= header >>= fun hdr ->
    let num_args = size hdr in
    modify_reg `sp (fun sp -> sp -: num_args) >>
    read_reg `env >>= fun env ->
    for_dn (num_args -: one) zero 
      (fun i -> field env (i +: (const 2)) >>= push_stack) >>
    field env one >>= write_reg `env  >>
    modify_reg `extra_args (fun earg -> earg +: num_args)

  (*
    Instruct(GRAB): {
      int required = *pc++;
      if (extra_args >= required) {
        extra_args -= required;
      } else {
        mlsize_t num_args, i;
        num_args = 1 + extra_args; /* arg1 + extra args */
        Alloc_small(accu, num_args + 2, Closure_tag);
        Field(accu, 1) = env;
        for (i = 0; i < num_args; i++) Field(accu, i + 2) = sp[i];
        Code_val(accu) = pc - 3; /* Point to the preceding RESTART instr. */
        sp += num_args;
        pc = (code_t)(sp[0]);
        env = sp[1];
        extra_args = Long_val(sp[2]);
        sp += 3;
      }
      Next;
    }
  *)
  let grab = 
    pop_arg >>= fun required ->
    read_reg `extra_args >>= fun earg ->
    if_ (earg >=+ required) begin
      write_reg `extra_args (earg -: required)
    end begin
      let num_args = earg +: one in
      alloc (num_args +: (const 2)) closure_tag >>= fun ptr ->
      read_reg `env >>= set_field ptr one >>
      for_up zero num_args
        (fun i -> pop_stack >>= set_field ptr (i +: (const 2))) >>
      read_reg `pc >>= fun pc -> 
      read_reg `accu >>= fun accu ->
      write_mem `mem accu (pc -: (pcofs (const 3))) >>
      pop_stack >>= write_reg `env >>
      pop_stack >>= fun earg -> write_reg `extra_args (int_val earg)
    end
  

  (*
    Instruct(CLOSURE): {
      int nvars = *pc++;
      int i;
      if (nvars > 0) *--sp = accu;
      if (nvars < Max_young_wosize) {
        /* nvars + 1 <= Max_young_wosize, can allocate in minor heap */
        Alloc_small(accu, 1 + nvars, Closure_tag);
        for (i = 0; i < nvars; i++) Field(accu, i + 1) = sp[i];
      } else {
        /* PR#6385: must allocate in major heap */
        /* caml_alloc_shr and caml_initialize never trigger a GC,
           so no need to Setup_for_gc */
        accu = caml_alloc_shr(1 + nvars, Closure_tag);
        for (i = 0; i < nvars; i++) caml_initialize(&Field(accu, i + 1), sp[i]);
      }
      /* The code pointer is not in the heap, so no need to go through
         caml_initialize. */
      Code_val(accu) = pc + *pc;
      pc++;
      sp += nvars;
      Next;
    }
  *)
  let closure = 
    pop_arg >>= fun nvars ->
    (if_ (nvars >+ zero) (read_reg `accu >>= push_stack) (return ())) >>
    alloc (nvars +: one) closure_tag >>= fun ptr ->
    for_up zero nvars 
      (fun i -> pop_stack >>= set_field ptr (i +: one)) >>
    pop_arg >>= fun ofs -> 
    read_reg `pc >>= fun pc ->
    write_mem `mem ptr (pc +: pcofs (ofs -: one)) >> (* need -1 because we've popped from pc *)
    write_reg `accu ptr

  (*
    Instruct(CLOSUREREC): {
      int nfuncs = *pc++;
      int nvars = *pc++;
      mlsize_t blksize = nfuncs * 2 - 1 + nvars;
      int i;
      value * p;
      if (nvars > 0) *--sp = accu;
      if (blksize <= Max_young_wosize) {
        Alloc_small(accu, blksize, Closure_tag);
        p = &Field(accu, nfuncs * 2 - 1);
        for (i = 0; i < nvars; i++, p++) *p = sp[i];
      } else {
        /* PR#6385: must allocate in major heap */
        /* caml_alloc_shr and caml_initialize never trigger a GC,
           so no need to Setup_for_gc */
        accu = caml_alloc_shr(blksize, Closure_tag);
        p = &Field(accu, nfuncs * 2 - 1);
        for (i = 0; i < nvars; i++, p++) caml_initialize(p, sp[i]);
      }
      sp += nvars;
      /* The code pointers and infix headers are not in the heap,
         so no need to go through caml_initialize. */
      p = &Field(accu, 0);
      *p = (value) (pc + pc[0]);
      *--sp = accu;
      p++;
      for (i = 1; i < nfuncs; i++) {
        *p = Make_header(i * 2, Infix_tag, Caml_white);  /* color irrelevant. */
        p++;
        *p = (value) (pc + pc[i]);
        *--sp = (value) p;
        p++;
      }
      pc += nfuncs;
      Next;
    }
  *)
  let closurerec = 
    pop_arg >>= fun nfuncs ->
    pop_arg >>= fun nvars ->
    let blksize = (sll nfuncs one) -: one +: nvars in
    (if_ (nvars >+ zero) (read_reg `accu >>= push_stack) (return ())) >>
    alloc blksize closure_tag >>= fun ptr ->
    for_up zero nvars
      (fun i -> pop_stack >>= set_field ptr ((sll nfuncs one) -: one +: i)) >>
    read_reg `pc >>= fun pc ->
    pop_arg >>= fun ofs ->
    set_field ptr zero (pc +: (pcofs ofs)) >>
    push_stack ptr >>
    for_up one nfuncs
      (fun i -> 
        let i2 = sll i one in
        set_field ptr (one +: i2) (make_header i2 white infix_tag) >>
        pop_arg >>= fun ofs -> set_field ptr ((const 2) +: i2) (pc +: (pcofs ofs)) >>
        push_stack (ptr +: (aofs (one +: i2)))) >>
    write_reg `accu ptr
  
  (*
    Instruct(PUSHOFFSETCLOSURE):
      *--sp = accu; /* fallthrough */
    Instruct(OFFSETCLOSURE):
      accu = env + *pc++ * sizeof(value); Next;

    Instruct(PUSHOFFSETCLOSUREM2):
      *--sp = accu; /* fallthrough */
    Instruct(OFFSETCLOSUREM2):
      accu = env - 2 * sizeof(value); Next;
    Instruct(PUSHOFFSETCLOSURE0):
      *--sp = accu; /* fallthrough */
    Instruct(OFFSETCLOSURE0):
      accu = env; Next;
    Instruct(PUSHOFFSETCLOSURE2):
      *--sp = accu; /* fallthrough */
    Instruct(OFFSETCLOSURE2):
      accu = env + 2 * sizeof(value); Next;
  *)
  let offsetclosure' ofs = copy_modify_reg `env `accu (fun d -> d +: (aofs ofs))

  let pushoffsetclosure' ofs = push_stack_accu >> offsetclosure' ofs

  let pushoffsetclosure = pop_arg >>= pushoffsetclosure'
  
  let offsetclosure = pop_arg >>= offsetclosure'

  let pushoffsetclosurem2 = pushoffsetclosure' (const (-2))

  let offsetclosurem2 = offsetclosure' (const (-2))

  let pushoffsetclosure0 = pushoffsetclosure' zero

  let offsetclosure0 = offsetclosure' zero

  let pushoffsetclosure2 = pushoffsetclosure' (const 2)

  let offsetclosure2 = offsetclosure' (const 2)

  (************************************************************)
  (* Access to global variables *)

  (*
    Instruct(PUSHGETGLOBAL):
      *--sp = accu;
      /* Fallthrough */
    Instruct(GETGLOBAL):
      accu = Field(caml_global_data, *pc);
      pc++;
      Next;

    Instruct(PUSHGETGLOBALFIELD):
      *--sp = accu;
      /* Fallthrough */
    Instruct(GETGLOBALFIELD): {
      accu = Field(caml_global_data, *pc);
      pc++;
      accu = Field(accu, *pc);
      pc++;
      Next;
    }
  *)
  let getglobal = 
    read_reg `global_data >>= fun global_data -> 
    pop_arg >>= fun fld -> 
    field global_data fld >>= 
    write_reg `accu

  let pushgetglobal = push_stack_accu >> getglobal

  let getglobalfield = 
    read_reg `global_data >>= fun global_data -> 
    pop_arg >>= fun fld -> 
    field global_data fld >>= fun data ->
    pop_arg >>= field data >>= write_reg `accu

  let pushgetglobalfield = push_stack_accu >> getglobalfield

  (*
    Instruct(SETGLOBAL):
      caml_modify(&Field(caml_global_data, *pc), accu);
      accu = Val_unit;
      pc++;
      Next;
  *)
  let setglobal = 
    read_reg `global_data >>= fun global_data -> 
    pop_arg >>= fun fld -> 
    read_reg `accu >>= 
    modify_field global_data fld >>
    write_reg `accu val_unit

  (************************************************************)
  (* Allocation of blocks *)

  (*
    Instruct(PUSHATOM0):
      *--sp = accu;
      /* Fallthrough */
    Instruct(ATOM0):
      accu = Atom(0); Next;

    Instruct(PUSHATOM):
      *--sp = accu;
      /* Fallthrough */
    Instruct(ATOM):
      accu = Atom( *pc++); Next;
  *)
  let atom' ofs = 
    read_reg `atom_table >>= fun atom -> 
    write_reg `accu (atom +: aofs ofs)

  let atom0 = atom' zero

  let pushatom0 = push_stack_accu >> atom0

  let atom = pop_arg >>= atom'
  
  let pushatom = push_stack_accu >> atom

  (*
    Instruct(MAKEBLOCK): {
      mlsize_t wosize = *pc++;
      tag_t tag = *pc++;
      mlsize_t i;
      value block;
      if (wosize <= Max_young_wosize) {
        Alloc_small(block, wosize, tag);
        Field(block, 0) = accu;
        for (i = 1; i < wosize; i++) Field(block, i) = *sp++;
      } else {
        block = caml_alloc_shr(wosize, tag);
        caml_initialize(&Field(block, 0), accu);
        for (i = 1; i < wosize; i++) caml_initialize(&Field(block, i), *sp++);
      }
      accu = block;
      Next;
    }
    ....
    Instruct(MAKEBLOCK3): {
      tag_t tag = *pc++;
      value block;
      Alloc_small(block, 3, tag);
      Field(block, 0) = accu;
      Field(block, 1) = sp[0];
      Field(block, 2) = sp[1];
      sp += 2;
      accu = block;
      Next;
    }
  *)
  let makeblockn wosize = 
    pop_arg >>= fun tag ->
    alloc wosize tag >>= fun block ->
    read_reg `accu >>= set_field block zero >>
    for_up one wosize
      (fun i -> pop_stack >>= set_field block i) >>
    write_reg `accu block

  let makeblock = pop_arg >>= makeblockn

  (*
    Instruct(MAKEFLOATBLOCK): {
      mlsize_t size = *pc++;
      mlsize_t i;
      value block;
      if (size <= Max_young_wosize / Double_wosize) {
        Alloc_small(block, size * Double_wosize, Double_array_tag);
      } else {
        block = caml_alloc_shr(size * Double_wosize, Double_array_tag);
      }
      Store_double_field(block, 0, Double_val(accu));
      for (i = 1; i < size; i++){
        Store_double_field(block, i, Double_val( *sp));
        ++ sp;
      }
      accu = block;
      Next;
    }
  *)

  let makefloatblock = 
    pop_arg >>= fun size ->
    alloc size double_array_tag >>= fun block ->
    read_reg `accu >>= set_field block zero >>
    for_up one size
      (fun i -> pop_stack >>= set_field block i) >>
    write_reg `accu block


  (************************************************************)
  (* Access to components of blocks *)

  (*
    Instruct(GETFIELD0):
      accu = Field(accu, 0); Next;
    ...
    Instruct(GETFIELD3):
      accu = Field(accu, 3); Next;
    Instruct(GETFIELD):
      accu = Field(accu, *pc); pc++; Next;
  *)
  let getfieldn n = 
    read_reg `accu >>= fun accu ->
    field accu n >>= write_reg `accu

  let getfield = pop_arg >>= getfieldn

  (*
    Instruct(GETFLOATFIELD): {
      double d = Double_field(accu, *pc);
      Alloc_small(accu, Double_wosize, Double_tag);
      Store_double_val(accu, d);
      pc++;
      Next;
    }
  *)
  let getfloatfield = 
    read_reg `accu >>= fun accu ->
    pop_arg >>= fun fld ->
    field accu fld >>= fun d ->
    alloc one double_tag >>= fun ptr ->
    set_field ptr zero d >>
    write_reg `accu ptr

  (*
    Instruct(SETFIELD0):
      caml_modify(&Field(accu, 0), *sp++);
      accu = Val_unit;
      Next;
    ...
    Instruct(SETFIELD3):
      caml_modify(&Field(accu, 3), *sp++);
      accu = Val_unit;
      Next;
    Instruct(SETFIELD):
      caml_modify(&Field(accu, *pc), *sp++);
      accu = Val_unit;
      pc++;
      Next;
  *)
  let setfieldn n =
    read_reg `accu >>= fun accu ->
    pop_stack >>= fun d ->
    modify_field accu n d >>
    write_reg `accu val_unit

  let setfield = pop_arg >>= setfieldn

  (*
    Instruct(SETFLOATFIELD):
      Store_double_field(accu, *pc, Double_val( *sp));
      accu = Val_unit;
      sp++;
      pc++;
      Next;
  *)
  let setfloatfield = setfield

  (************************************************************)
  (* Array operations *)

  let vectlength = 
    read_reg `accu >>= header >>= fun hdr ->
    write_reg `accu (val_int (size hdr))

  let getvectitem = 
    read_reg `accu >>= fun accu -> 
    pop_stack >>= fun fld ->
    field accu (int_val fld) >>= write_reg `accu

  let setvectitem = 
    read_reg `accu >>= fun accu ->
    pop_stack >>= fun fld ->
    pop_stack >>= fun data ->
    modify_field accu (int_val fld) data >>
    write_reg `accu val_unit

  (************************************************************)
  (* String operations *)

  (*
    Instruct(GETSTRINGCHAR):
      accu = Val_int(Byte_u(accu, Long_val(sp[0])));
      sp += 1;
      Next;
  *)
  let getstringchar = 
    pop_stack >>= fun byte_ofs ->
    read_reg `accu >>= fun ptr ->
    let c7, c255 = const 7, const 255 in
    let byte_ofs = int_val byte_ofs in
    let ptr = ptr +: (byte_ofs &: (~: c7)) in
    let ofs = byte_ofs &: c7 in
    read_mem `mem ptr >>= fun str ->
    let byte = (srl str (sll ofs c3)) &: c255 in
    write_reg `accu (val_int byte)

  (*
    Instruct(SETSTRINGCHAR):
      Byte_u(accu, Long_val(sp[0])) = Int_val(sp[1]);
      sp += 2;
      accu = Val_unit;
      Next;
  *)
  let setstringchar = 
    pop_stack >>= fun byte_ofs ->
    pop_stack >>= fun byte_val ->
    read_reg `accu >>= fun ptr ->
    let c7, c255 = const 7, const 255 in
    let byte_ofs = int_val byte_ofs in
    let ptr = ptr +: (byte_ofs &: (~: c7)) in
    let ofs = byte_ofs &: c7 in
    read_mem `mem ptr >>= fun str ->
    let byte = (sll (int_val byte_val) (sll ofs c3)) in
    let mask = ~: (sll c255 (sll ofs c3)) in
    write_mem `mem ptr ((str &: mask) |: byte) >>
    write_reg `accu val_unit

  (************************************************************)
  (* Branches and conditional branches *)

  (*
    Instruct(BRANCH):
      pc += *pc;
      Next;
  *)
  let branch = read_bytecode zero >>= fun ofs -> modify_reg `pc (fun pc -> pc +: pcofs ofs)

  (*
    Instruct(BRANCHIF):
      if (accu != Val_false) pc += *pc; else pc++;
      Next;
  *)
  let branchif = 
    read_reg `accu >>= fun accu ->
    if_ (accu <>: val_false) branch incr_pc

  (*
    Instruct(BRANCHIFNOT):
      if (accu == Val_false) pc += *pc; else pc++;
      Next;
  *)
  let branchifnot = 
    read_reg `accu >>= fun accu ->
    if_ (accu ==: val_false) branch incr_pc

  (*
    Instruct(SWITCH): {
      uint32 sizes = *pc++;
      if (Is_block(accu)) {
        intnat index = Tag_val(accu);
        Assert ((uintnat) index < (sizes >> 16));
        pc += pc[(sizes & 0xFFFF) + index];
      } else {
        intnat index = Long_val(accu);
        Assert ((uintnat) index < (sizes & 0xFFFF)) ;
        pc += pc[index];
      }
      Next;
    }
  *)
  let switch = 
    read_reg `accu >>= fun accu ->
    if_ (is_block accu) begin
      pop_arg >>= fun sizes ->
      header accu >>= fun hdr ->
      let index = tag hdr in
      read_bytecode ((sizes &: const 0xFFFF) +: index) >>= fun ofs ->
      modify_reg `pc (fun pc -> pc +: pcofs ofs)
    end begin
      let index = int_val accu in
      incr_pc >>
      read_bytecode index >>= fun ofs ->
      modify_reg `pc (fun pc -> pc +: pcofs ofs)
    end

  (*
    Instruct(BOOLNOT):
      accu = Val_not(accu);
      Next;
  *)
  let boolnot = (* probably not the hardware implementation we'd choose... *)
    modify_reg `accu (fun accu -> val_false +: val_true -: accu) 

  (************************************************************)
  (* Exceptions *)

  (*
    Instruct(PUSHTRAP):
      sp -= 4;
      Trap_pc(sp) = pc + *pc;
      Trap_link(sp) = caml_trapsp;
      sp[2] = env;
      sp[3] = Val_long(extra_args);
      caml_trapsp = sp;
      pc++;
      Next;
  *)
  let pushtrap = 
    read_reg `extra_args >>= fun earg -> push_stack (val_int earg) >>
    read_reg `env >>= push_stack >>
    read_reg `trapsp >>= push_stack >>
    pop_arg >>= fun ofs -> read_reg `pc >>= fun pc -> push_stack (pc +: pcofs (ofs -: one)) >>
    copy_modify_reg `sp `trapsp (fun d -> d)

  (*
    Instruct(POPTRAP):
      if (caml_something_to_do) {
        /* We must check here so that if a signal is pending and its
           handler triggers an exception, the exception is trapped
           by the current try...with, not the enclosing one. */
        pc--; /* restart the POPTRAP after processing the signal */
        goto process_signal;
      }
      caml_trapsp = Trap_link(sp);
      sp += 4;
      Next;
  *)
  let poptrap = 
    if_ something_to_do
      (return ()) (* XXX *)
      (read_stack one >>= write_reg `trapsp >> incr `sp (aofs (const 4))) 

  let raise_notrace = not_implemented

  let reraise = not_implemented

  let raise_ = not_implemented

  (************************************************************)
  (* Stack checks *)

  (* ??? *)

  (************************************************************)
  (* Signal handling *)

  let check_signals = 
    if_ something_to_do (* XXX *)
      (return ()) (return ()) 

  (* XXX process signal *)

  (************************************************************)
  (* Calling C functions *)

  (*
    Instruct(C_CALL5):
      Setup_for_c_call;
      accu = Primitive( *pc)(accu, sp[1], sp[2], sp[3], sp[4]);
      Restore_after_c_call;
      sp += 4;
      pc++;
      Next;
  *)
  let c_call n = 
    pop_arg >>= fun prim ->
    read_reg `env >>= push_stack >>
    c_call prim >>= write_reg `accu >>
    pop_stack >>= write_reg `env >>
    modify_reg `sp (fun sp -> sp +: (aofs n))

  let c_calln = not_implemented

  (************************************************************)
  (* Integer constants *)

  (*
    Instruct(CONST0):
      accu = Val_int(0); Next;
    ...
    Instruct(CONST3):
      accu = Val_int(3); Next;

    Instruct(PUSHCONST0):
      *--sp = accu; accu = Val_int(0); Next;
    ...
    Instruct(PUSHCONST3):
      *--sp = accu; accu = Val_int(3); Next;
  *)
  let constn n = write_reg `accu (val_int n)
  
  let pushconstn n = push_stack_accu >> constn n

  (*
    Instruct(PUSHCONSTINT):
      *--sp = accu;
      /* Fallthrough */
    Instruct(CONSTINT):
      accu = Val_int( *pc);
      pc++;
      Next;
  *)
  let constint = pop_arg >>= constn

  let pushconstint = push_stack_accu >> constint

  (************************************************************)
  (* Integer arithmetic *)

  (*
    Instruct(NEGINT):
      accu = (value)(2 - (intnat)accu); Next;
    Instruct(ADDINT):
      accu = (value)((intnat) accu + (intnat) *sp++ - 1); Next;
    Instruct(SUBINT):
      accu = (value)((intnat) accu - (intnat) *sp++ + 1); Next;
    Instruct(MULINT):
      accu = Val_long(Long_val(accu) * Long_val( *sp++)); Next;
  *)
  let negint = modify_reg `accu (fun d -> (const 2) -: d)

  let op2int f = pop_stack >>= fun b -> modify_reg `accu (fun a -> f a b)

  let addint = op2int (fun a b -> a +: b -: one)

  let subint = op2int (fun a b -> a -: b +: one)

  let mulint = op2int (fun a b -> val_int (int_val a *: int_val b))

  (*
    Instruct(DIVINT): {
      intnat divisor = Long_val( *sp++);
      if (divisor == 0) { Setup_for_c_call; caml_raise_zero_divide(); }
      accu = Val_long(Long_val(accu) / divisor);
      Next;
    }
    Instruct(MODINT): {
      intnat divisor = Long_val( *sp++);
      if (divisor == 0) { Setup_for_c_call; caml_raise_zero_divide(); }
      accu = Val_long(Long_val(accu) % divisor);
      Next;
    }
  *)
  let op2div f = 
    pop_stack >>= fun divisor ->
    let divisor = int_val divisor in
    if_ (divisor ==: zero) 
      (raise_error "divide by zero")
      (modify_reg `accu (fun a -> val_int (f (int_val a) divisor)))
      
  let divint = op2div (fun a b -> a /: b)

  let modint = op2div (fun a b -> a %: b)
  
  (*
    Instruct(ANDINT):
      accu = (value)((intnat) accu & (intnat) *sp++); Next;
    Instruct(ORINT):
      accu = (value)((intnat) accu | (intnat) *sp++); Next;
    Instruct(XORINT):
      accu = (value)(((intnat) accu ^ (intnat) *sp++) | 1); Next;
    Instruct(LSLINT):
      accu = (value)((((intnat) accu - 1) << Long_val( *sp++)) + 1); Next;
    Instruct(LSRINT):
      accu = (value)((((uintnat) accu - 1) >> Long_val( *sp++)) | 1);
      Next;
    Instruct(ASRINT):
      accu = (value)((((intnat) accu - 1) >> Long_val( *sp++)) | 1); Next;
  *)
  let andint = op2int (&:)

  let orint = op2int (|:)

  let xorint = op2int (fun a b -> (a ^: b) |: one)

  let lslint = op2int (fun a b -> (sll (a -: one) (int_val b)) +: one)

  let lsrint = op2int (fun a b -> (srl (a -: one) (int_val b)) |: one)

  let asrint = op2int (fun a b -> (sra (a -: one) (int_val b)) |: one)

  (*
    #define Integer_comparison(typ,opname,tst) 
        Instruct(opname): 
          accu = Val_int((typ) accu tst (typ) *sp++); Next;
  *)
  let int_comp f =
    pop_stack >>= fun b -> modify_reg `accu (fun a -> val_int (f a b))

  let eq = int_comp (==:)

  let neq = int_comp (<>:)

  let ltint = int_comp (<+)
  
  let leint = int_comp (<=+)

  let gtint = int_comp (>+)

  let geint = int_comp (>=+)

  let ultint = int_comp (<:)

  let ugeint = int_comp (>=:)

  (*
    #define Integer_branch_comparison(typ,opname,tst,debug) 
        Instruct(opname): 
          if ( *pc++ tst (typ) Long_val(accu)) { 
            pc += *pc ; 
          } else { 
            pc++ ; 
          } ; Next;
  *)
  let int_bcomp f = 
    pop_arg >>= fun a -> read_reg `accu >>= fun b ->
    if_ (f a (int_val b))
      (read_bytecode zero >>= fun ofs -> modify_reg `pc (fun pc -> pc +: (pcofs ofs)))
      incr_pc

  let beq = int_bcomp (==:)

  let bneq = int_bcomp (<>:)

  let bltint = int_bcomp (<+)
  
  let bleint = int_bcomp (<=+)

  let bgtint = int_bcomp (>+)

  let bgeint = int_bcomp (>=+)

  let bultint = int_bcomp (<:)

  let bugeint = int_bcomp (>=:)

  (*
    Instruct(OFFSETINT):
      accu += * pc sll 1;
      pc++;
      Next
  *)
  let offsetint = 
    pop_arg >>= fun pc -> 
    modify_reg `accu (fun accu -> accu +: (sll pc one)) 

  (*
    Instruct(OFFSETREF):
      Field(accu, 0) += * pc sll 1;
      accu = Val_unit;
      pc++;
      Next
  *)
  let offsetref = 
    pop_arg >>= fun pc ->
    read_reg `accu >>= fun accu ->
    field accu zero >>= fun d ->
    set_field accu zero (d +: (sll pc one)) >>
    write_reg `accu val_unit
  
  (*
    Instruct(ISINT):
      accu = Val_long(accu & 1);
      Next;
  *)
  let isint = modify_reg `accu (fun accu -> val_int (accu &: one))

  (************************************************************)
  (* Object-oriented operations *)

  let getmethod = not_implemented

  let getpubmet = not_implemented

  let getdynmet = not_implemented

  (************************************************************)
  (* Debugging and machine control *)

  let stop = not_implemented

  let event = not_implemented

  let break = not_implemented

  (************************************************************)

  let dispatch = function
    | Instr.ACC0 -> accn (const 0)
    | Instr.ACC1 -> accn (const 1)
    | Instr.ACC2 -> accn (const 2)
    | Instr.ACC3 -> accn (const 3)
    | Instr.ACC4 -> accn (const 4)
    | Instr.ACC5 -> accn (const 5)
    | Instr.ACC6 -> accn (const 6)
    | Instr.ACC7 -> accn (const 7)
    | Instr.ACC -> acc
  
    | Instr.PUSH -> push
    | Instr.PUSHACC0 -> pushaccn (const 0)
    | Instr.PUSHACC1 -> pushaccn (const 1) 
    | Instr.PUSHACC2 -> pushaccn (const 2) 
    | Instr.PUSHACC3 -> pushaccn (const 3) 
    | Instr.PUSHACC4 -> pushaccn (const 4) 
    | Instr.PUSHACC5 -> pushaccn (const 5) 
    | Instr.PUSHACC6 -> pushaccn (const 6) 
    | Instr.PUSHACC7 -> pushaccn (const 7) 
    | Instr.PUSHACC -> pushacc
  
    | Instr.POP -> pop
    | Instr.ASSIGN -> assign

    | Instr.ENVACC1 -> envaccn (const 1)
    | Instr.ENVACC2 -> envaccn (const 2)
    | Instr.ENVACC3 -> envaccn (const 3)
    | Instr.ENVACC4 -> envaccn (const 4)
    | Instr.ENVACC -> envacc
    
    | Instr.PUSHENVACC1 -> pushenvaccn (const 1)
    | Instr.PUSHENVACC2 -> pushenvaccn (const 2)
    | Instr.PUSHENVACC3 -> pushenvaccn (const 3)
    | Instr.PUSHENVACC4 -> pushenvaccn (const 4)
    | Instr.PUSHENVACC -> pushenvacc
    
    | Instr.PUSH_RETADDR -> push_retaddr
    
    | Instr.APPLY -> apply
    | Instr.APPLY1 -> applyn 1
    | Instr.APPLY2 -> applyn 2 
    | Instr.APPLY3 -> applyn 3 
    
    | Instr.APPTERM -> appterm
    | Instr.APPTERM1 -> apptermn 1
    | Instr.APPTERM2 -> apptermn 2
    | Instr.APPTERM3 -> apptermn 3
    
    | Instr.RETURN -> return_
    | Instr.RESTART -> restart
    | Instr.GRAB -> grab
    | Instr.CLOSURE -> closure
    | Instr.CLOSUREREC -> closurerec
    
    | Instr.OFFSETCLOSUREM2 -> offsetclosurem2
    | Instr.OFFSETCLOSURE0 -> offsetclosure0
    | Instr.OFFSETCLOSURE2 -> offsetclosure2
    | Instr.OFFSETCLOSURE -> offsetclosure

    | Instr.PUSHOFFSETCLOSUREM2 -> pushoffsetclosurem2 
    | Instr.PUSHOFFSETCLOSURE0 -> pushoffsetclosure0
    | Instr.PUSHOFFSETCLOSURE2 -> pushoffsetclosure2
    | Instr.PUSHOFFSETCLOSURE -> pushoffsetclosure 
    
    | Instr.GETGLOBAL -> getglobal
    | Instr.PUSHGETGLOBAL -> pushgetglobal
    | Instr.GETGLOBALFIELD -> getglobalfield
    | Instr.PUSHGETGLOBALFIELD -> pushgetglobalfield
    | Instr.SETGLOBAL -> setglobal
    
    | Instr.ATOM0 -> atom0
    | Instr.ATOM -> atom
    
    | Instr.PUSHATOM0 -> pushatom0
    | Instr.PUSHATOM -> pushatom
    
    | Instr.MAKEBLOCK -> makeblock
    | Instr.MAKEBLOCK1 -> makeblockn (const 1)
    | Instr.MAKEBLOCK2 -> makeblockn (const 2)
    | Instr.MAKEBLOCK3 -> makeblockn (const 3)
    | Instr.MAKEFLOATBLOCK -> makefloatblock
    
    | Instr.GETFIELD0 -> getfieldn (const 0)
    | Instr.GETFIELD1 -> getfieldn (const 1)
    | Instr.GETFIELD2 -> getfieldn (const 2)
    | Instr.GETFIELD3 -> getfieldn (const 3)
    | Instr.GETFIELD -> getfield
    | Instr.GETFLOATFIELD -> getfloatfield

    | Instr.SETFIELD0 -> setfieldn (const 0)
    | Instr.SETFIELD1 -> setfieldn (const 1) 
    | Instr.SETFIELD2 -> setfieldn (const 2) 
    | Instr.SETFIELD3 -> setfieldn (const 3) 
    | Instr.SETFIELD -> setfield
    | Instr.SETFLOATFIELD -> setfloatfield 
    
    | Instr.VECTLENGTH -> vectlength
    | Instr.GETVECTITEM -> getvectitem
    | Instr.SETVECTITEM -> setvectitem
    
    | Instr.GETSTRINGCHAR -> getstringchar
    | Instr.SETSTRINGCHAR -> setstringchar
    
    | Instr.BRANCH -> branch
    | Instr.BRANCHIF -> branchif
    | Instr.BRANCHIFNOT -> branchifnot
    | Instr.SWITCH -> switch
    
    | Instr.BOOLNOT -> boolnot
    
    | Instr.PUSHTRAP -> pushtrap
    | Instr.POPTRAP -> poptrap
    | Instr.RAISE -> raise_
    
    | Instr.CHECK_SIGNALS -> check_signals
    
    | Instr.C_CALL1 -> c_call (const 0) (* argument is number of elements to pop from stack *)
    | Instr.C_CALL2 -> c_call (const 1) 
    | Instr.C_CALL3 -> c_call (const 2) 
    | Instr.C_CALL4 -> c_call (const 3) 
    | Instr.C_CALL5 -> c_call (const 4) 
    | Instr.C_CALLN -> c_calln
    
    | Instr.CONST0 -> constn (const 0)
    | Instr.CONST1 -> constn (const 1)
    | Instr.CONST2 -> constn (const 2)
    | Instr.CONST3 -> constn (const 3)
    | Instr.CONSTINT -> constint
    
    | Instr.PUSHCONST0 -> pushconstn (const 0)
    | Instr.PUSHCONST1 -> pushconstn (const 1) 
    | Instr.PUSHCONST2 -> pushconstn (const 2) 
    | Instr.PUSHCONST3 -> pushconstn (const 3) 
    | Instr.PUSHCONSTINT -> pushconstint
    
    | Instr.NEGINT -> negint
    | Instr.ADDINT -> addint
    | Instr.SUBINT -> subint
    | Instr.MULINT -> mulint
    | Instr.DIVINT -> divint
    | Instr.MODINT -> modint
    | Instr.ANDINT -> andint
    | Instr.ORINT -> orint
    | Instr.XORINT -> xorint
    | Instr.LSLINT -> lslint
    | Instr.LSRINT -> lsrint
    | Instr.ASRINT -> asrint
    
    | Instr.EQ -> eq
    | Instr.NEQ -> neq
    | Instr.LTINT -> ltint
    | Instr.LEINT -> leint
    | Instr.GTINT -> gtint
    | Instr.GEINT -> geint
    
    | Instr.OFFSETINT -> offsetint
    | Instr.OFFSETREF -> offsetref
    | Instr.ISINT -> isint
    | Instr.GETMETHOD -> getmethod
    
    | Instr.BEQ -> beq
    | Instr.BNEQ -> bneq
    | Instr.BLTINT -> bltint
    | Instr.BLEINT -> bleint
    | Instr.BGTINT -> bgtint
    | Instr.BGEINT -> bgeint
    
    | Instr.ULTINT -> ultint
    | Instr.UGEINT -> ugeint
    | Instr.BULTINT -> bultint
    | Instr.BUGEINT -> bugeint
    
    | Instr.GETPUBMET -> getpubmet
    | Instr.GETDYNMET -> getdynmet
    
    | Instr.STOP -> stop
    | Instr.EVENT -> event
    | Instr.BREAK -> break
    | Instr.RERAISE -> reraise
    | Instr.RAISE_NOTRACE-> raise_notrace
  
end

(*module type M_eval = Monad 
    with type S.t = int64
     and type S.st = State_eval.st
module M : M_eval = Monad(State_eval) 
module O = Opcodes(M)*)

module type M_poly = Monad 
    with type S.t = State_poly.t
     and type S.st = State_poly.st
module M' : M_poly = Monad(struct let trace=false end)(State_poly) 
module O' = Opcodes(M')

