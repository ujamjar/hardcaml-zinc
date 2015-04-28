(*
 * TODO;
 *
 *  - rethink the iter_up/dn functions.  when we see how the statemchine will work try
 *    to unify them.
 *
 *
 *)

type machine_register = [ `accu | `env | `pc | `sp | `extra_args ]
type cache = [ `stack | `program | `mem ]

module type State = sig

  type st
  include Ops.S 

  (* initial state *)

  val initial : unit -> st

  (* machine registers and memory accesses *)

  val get_reg : st -> machine_register -> t * st
  val set_reg : st -> machine_register -> t -> st
  val get_mem : st -> cache -> t -> t * st
  val set_mem : st -> cache -> t -> t -> st

  (* control *)
  
  val cond : st -> t -> (st -> unit * st) -> (st -> unit * st) -> st
  val iter_up : st -> t -> t -> (t -> st -> unit * st) -> st
  val iter_dn : st -> t -> t -> (t -> st -> unit * st) -> st

end

module State_eval = struct

  let mem_size = 16

  type st = 
    {
      accu : int64;
      env : int64;
      pc : int64;
      sp : int64;
      extra_args : int64;
      memory : int64 array;
    }

  let initial () = 
    {
      accu = 0L;
      env = 0L;
      pc = 0L;
      sp = Int64.of_int (8*mem_size);
      extra_args = 0L;
      memory = Array.init mem_size (fun i -> Int64.of_int i);
    }

  let get_reg st r = 
    match r with
    | `accu -> st.accu, st
    | `env -> st.env, st
    | `pc -> st.pc, st
    | `sp -> st.sp, st
    | `extra_args -> st.extra_args, st

  let set_reg st r v = 
    match r with
    | `accu -> { st with accu = v }
    | `env -> { st with env = v }
    | `pc -> { st with pc = v } 
    | `sp -> { st with sp = v }
    | `extra_args -> { st with extra_args = v }

  let get_mem st cache adr = 
    try
      st.memory.(Int64.to_int adr), st
    with Invalid_argument x ->
      raise (Invalid_argument ("get_mem [" ^ Int64.to_string adr ^ "] " ^ x))

  let set_mem st cache adr v = 
    try
      st.memory.(Int64.to_int adr) <- v; st 
    with Invalid_argument x ->
      raise (Invalid_argument ("set_mem [" ^ Int64.to_string adr ^ "] " ^ x))

  let cond st c t f = 
    snd @@ if c <> 0L then t st else f st

  let iter_up st m n f = 
    let rec g st i = 
      if i > n then st
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

end

module State_poly = struct

  type cmd = 
    | Get_reg of int * machine_register 
    | Set_reg of machine_register * t
    | Get_mem of int * cache * t
    | Set_mem of cache * t * t
    | Cond of t * cmd list * cmd list
    | Iter of bool * int * t * t * cmd list
  and t = 
    | Op of string * t * t
    | Sft of string * t * int
    | Val of int
    | Const of int
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

  let sll a b = Sft("<<",  a, b)
  let srl a b = Sft(">>",  a, b)
  let sra a b = Sft(">>+", a, b)

  let (==:) a b = Op("==", a, b)
  let (<>:) a b = Op("<>", a, b)
  let (<:)  a b = Op("<",  a, b)
  let (<=:) a b = Op("<=", a, b)
  let (>:)  a b = Op(">",  a, b)
  let (>=:) a b = Op(">=", a, b)

  let string_of_mach_reg = function
    | `accu -> "accu"
    | `env -> "env"
    | `pc -> "pc"
    | `sp -> "sp"
    | `extra_args -> "extra_args"

  let string_of_cache = function
    | `stack -> "stack"
    | `program -> "program"
    | `mem -> "mem"

  let rec string_of_value = function
    | Op(o,a,b) -> "(" ^ string_of_value a ^ o ^ string_of_value b ^ ")"
    | Sft(o,a,b) -> "(" ^ string_of_value a ^ o ^ string_of_int b ^ ")"
    | Val(i) -> "_" ^ string_of_int i
    | Const i -> string_of_int i

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

  val write_reg : machine_register -> S.t -> unit t
  val read_reg : machine_register -> S.t t
  val modify_reg : machine_register -> (S.t -> S.t) -> unit t

  val read_mem : cache -> S.t -> S.t t
  val write_mem : cache -> S.t -> S.t -> unit t

  val read_bytecode : S.t -> S.t t

end 
  
module Monad(S : State) = struct
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

  let if_ c t f st = 
    ((), S.cond st c t f)

  let for_up m n f st = ((), S.iter_up st m n f)
  let for_dn m n f st = ((), S.iter_dn st m n f)

  let read_reg r st = S.get_reg st r

  let write_reg r v st = ((), S.set_reg st r v)

  let modify_reg r f = 
    read_reg r >>= fun v -> write_reg r (f v)

  let read_mem cache adr st = 
    S.get_mem st cache (S.srl adr 3) 

  let write_mem cache adr value st = 
    ((), S.set_mem st cache (S.srl adr 3) value)

  (* move logic into state *)
  let read_bytecode adr st = 
    let open S in
    let ins,st = S.get_mem st `program (srl adr 3) in
    let sel = adr &: (const 4) in
    let ins = sra (if sel = (const 0) then sll ins 32 else ins) 32 in
    (ins, st)

end 

module Opcodes(M : Monad) = struct

  open M
  open S

  (******************************************************************)
  (* utils *)

  let incr r n = 
    read_reg r >>= fun v -> 
    write_reg r (v +: n)

  let decr r n = 
    read_reg r >>= fun v ->
    write_reg r (v -: n)

  let incr_sp = incr `sp (const 8)
  let decr_sp = decr `sp (const 8)

  let incr_pc = incr `pc (const 4)
  let decr_pc = decr `pc (const 4)

  let aofs x = sll x 3
  let mofs x = srl x 3

  let read_bytecode ofs = 
    read_reg `pc >>= fun pc ->
    read_bytecode (pc +: (sll ofs 2))

  let read_stack ofs = 
    read_reg `sp >>= fun sp -> 
    read_mem `stack (sp +: ofs)

  let write_stack ofs value = 
    read_reg `sp >>= fun sp ->
    write_mem `stack (sp +: ofs) value

  let push_stack value = 
    decr_sp >> write_stack (const 0) value

  let pop_stack = 
    read_stack (const 0) >>= fun arg ->
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
    read_bytecode (const 0) >>= fun arg ->
    incr_pc >>
    return arg

  let push_stack_accu = read_reg `accu >>= push_stack 

  include Mlvalues.Make(M.S)

  let header ptr = read_mem `mem (ptr -: (aofs one))
  let field ptr fld = read_mem `mem (ptr +: (aofs fld))
  let set_field ptr fld v = write_mem `mem (ptr +: (aofs fld)) v

  let alloc size tag = return zero

  let check_stacks = return ()

  (******************************************************************)
  (* instructions *)

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

  let acc i = read_stack i >>= write_reg `accu 

  let acc_arg = pop_arg >>= acc

  let push = push_stack_accu

  let pushacc arg = 
    push_stack_accu >>
    read_stack arg >>= fun data ->
    write_reg `accu data

  let pushacc_arg = pop_arg >>= pushacc

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
  let envacc i = 
    read_reg `env >>= fun ptr -> field ptr i >>= write_reg `accu

  let envacc_arg = pop_arg >>= envacc

  let pushenvacc i = push_stack_accu >> envacc i

  let pushenvacc_arg = pop_arg >>= pushenvacc
  
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
    push_stack (pc +: (aofs ofs))

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
  let apply_push_stack = 
    read_reg `extra_args >>= fun earg -> push_stack (val_int earg) >>
    read_reg `env >>= push_stack >>
    read_reg `pc >>= push_stack 

  let applyn n = 
    pop_stack_n n >>= fun args ->
    apply_push_stack >>
    push_stack_n args >>
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
    for_dn nargs zero
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
    modify_reg `sp (fun sp -> sp +: ofs -: const n) >>
    push_stack_n args >>
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
  let return =
    pop_arg >>= fun ofs -> modify_reg `sp (fun sp -> sp +: ofs) >>
    read_reg `extra_args >>= fun earg ->
    if_ (earg >: zero) begin
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
    if_ (earg >=: required) begin
      write_reg `extra_args (earg -: required)
    end begin
      let num_args = earg +: one in
      alloc (num_args +: (const 2)) closure_tag >>= fun ptr ->
      read_reg `env >>= set_field ptr one >>
      for_up zero num_args
        (fun i -> pop_stack >>= set_field ptr (i +: (const 2))) >>
      read_reg `pc >>= fun pc -> 
      read_reg `accu >>= fun accu ->
      write_mem `mem accu (pc -: (aofs (const 3))) >>
      pop_stack >>= write_reg `env >>
      pop_stack >>= fun earg -> write_reg `extra_args (int_val earg)
    end



  let constn i = write_reg `accu i
  let constint = pop_arg >>= constn

end

module type M_eval = Monad 
    with type S.t = int64
     and type S.st = State_eval.st
module M : M_eval = Monad(State_eval) 
module O = Opcodes(M)

module type M_poly = Monad 
    with type S.t = State_poly.t
     and type S.st = State_poly.st
module M' : M_poly = Monad(State_poly) 
module O' = Opcodes(M')

open M
open O

