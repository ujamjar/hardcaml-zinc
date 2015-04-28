
(*module type S = sig

  type reg
  type stat
  type exp

  val set : reg -> exp -> stat

end*)
(*
module M = struct

  type reg = Accu | Env | Pc | Sp | Temp of int
  and stat = 
    | Set of reg * exp
    | Write of reg * exp * exp
  and exp = 
    | Const of int64
    | Reg of reg
    | Read of reg * exp
    | Op of string * exp * exp

  (* registers *)

  let accu = Accu
  let pc = Pc
  let env = Env
  let sp = Sp
  let temp i = Temp i

  (* statements *)

  let set reg exp = [Set(reg, exp)]
  let write reg ofs v = [Write(reg, ofs, v)]

  (* expressions *)

  let const i = Const i
  let zero = const 0L
  let one = const 1L
  let two = const 2L
  let three = const 3L

  let read reg ofs = Read(reg, ofs)

  let get_accu () = Reg accu
  let get_sp () = Reg sp
  let get_env () = Reg env
  let get_pc () = Reg pc
  let get_temp i = Reg (temp i)

  let (+:) a b = Op("+", a, b)
  let (+:.) a b = a +: const b

  (* statement sequencing *)
  let (>>) s t = s @ t

end

module N = struct

  let mem_size = 1024
  let memory = Array.init mem_size (fun i -> 0L)

  let accu = ref 0L
  let pc = ref 0L
  let env = ref 0L
  let sp = ref 0L
  let temp = 
    let temp = Array.init 4 (fun i -> ref 0L) in
    (fun i -> temp.(i))

  let set reg exp = reg := exp
  let write reg ofs v = 
    let adr = Int64.add !reg ofs in
    memory.(Int64.to_int adr) <- v

  let const i = i
  let zero = 0L
  let one = 1L
  let two = 2L
  let three = 3L

  let read reg ofs =
    let adr = Int64.add !reg ofs in
    memory.(Int64.to_int adr) 

  let get_accu () = !accu
  let get_sp () = !sp
  let get_env () = !env
  let get_pc () = !pc
  let get_temp i = !(temp i)

  let (+:) a b = Int64.add a b
  let (+:.) a b = a +: b

  let (>>) s t = s; t

end

module X = struct

  open M

  let push_stack v = 
    write sp zero v >>
    set sp (get_sp() +:. 1L)

  let pop_arg i = 
    set (temp i) (read pc zero) >>
    set pc (get_pc() +:. 1L)

  let accX i = set accu (read sp (const i))

  let acc () = 
    pop_arg 0 >>
    push_stack (get_temp 0)

  let push () = push_stack (get_accu())

  let pushaccX i = 
    push_stack (get_accu()) >> 
    accX i

  let pushacc () = 
    push_stack (get_accu()) >>
    pop_arg 0 >>
    set accu (get_temp 0)

end
*)

(* hmmmm....arrrrgggg *)

type machine_register = [ `accu | `env | `pc | `sp | `extra_args ]
type cache = [ `stack | `program | `mem ]

module type State = sig

  type st
  type value 

  val initial : unit -> st

  (* machine registers and memory accesses *)

  val get_reg : st -> machine_register -> value * st
  val set_reg : st -> machine_register -> value -> st
  val get_mem : st -> cache -> value -> value * st
  val set_mem : st -> cache -> value -> value -> st

  (* expressions *)

  val const : int -> value
  val zero : value
  val one : value

  val (+:) : value -> value -> value
  val (-:) : value -> value -> value
  val ( *: ) : value -> value -> value

  val (&:) : value -> value -> value
  val (|:) : value -> value -> value
  val (^:) : value -> value -> value
  val (~:) : value -> value

  val sll : value -> int -> value
  val srl : value -> int -> value
  val sra : value -> int -> value

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

  type value = int64

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

  open Int64

  let const = of_int 
  let zero = 0L
  let one = 1L

  let ( +: ) a b = add a b
  let ( -: ) a b = sub a b
  let ( *: ) a b = mul a b

  let ( /: ) a b = div a b
  let ( %: ) a b = rem a b
  
  let ( &: ) a b = logand a b
  let ( |: ) a b = logor a b
  let ( ^: ) a b = logxor a b
  let ( ~: ) a   = lognot a

  let sll a b = shift_left a b
  let srl a b = shift_right_logical a b
  let sra a b = shift_right a b

end

module State_poly = struct

  type cmd = 
    | Get_reg of machine_register 
    | Set_reg of machine_register * value
    | Get_mem of cache * value
    | Set_mem of cache * value * value
  and value = 
    | Op of string * value * value
    | Sft of string * value * int
    | Val of cmd
    | Const of int
  and st = cmd list
  
  let initial () = []

  let get_reg st r = let x = Get_reg(r) in (Val x), x::st
  let set_reg st r v = Set_reg(r, v) :: st
  let get_mem st c adr = let x = Get_mem(c, adr) in (Val x), x::st
  let set_mem st c adr v = Set_mem(c, adr, v) :: st

  let const i = Const(i)
  let zero = const 0
  let one = const 1

  let ( +: ) a b = Op("+", a, b)
  let ( -: ) a b = Op("-", a, b)
  let ( *: ) a b = Op("*", a, b)
  let ( &: ) a b = Op("&", a, b)
  let ( |: ) a b = Op("|", a, b)
  let ( ^: ) a b = Op("^", a, b)
  let ( ~: ) a   = Op("~", a, a)

  let sll a b = Sft("<<",  a, b)
  let srl a b = Sft(">>",  a, b)
  let sra a b = Sft(">>+", a, b)

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
    | Val(Get_reg(r)) -> string_of_mach_reg r
    | Val(Get_mem(c,a)) -> string_of_cache c ^ "[" ^ string_of_value a ^ "]"
    | Val(_) -> failwith "Set_reg/Set_mem as value"
    | Const i -> string_of_int i

  let rec print st = 
    let open Printf in
    List.iter (function
      | Get_reg(r) -> printf "%s;\n" (string_of_mach_reg r)
      | Set_reg(r,v) -> printf "%s = %s;\n" (string_of_mach_reg r) (string_of_value v)
      | Get_mem(c,a) -> printf "%s = %s;\n" (string_of_cache c) (string_of_value a)
      | Set_mem(c,a,v) -> printf "%s[%s] = %s;\n" 
        (string_of_cache c) (string_of_value a) (string_of_value v))
    (List.rev st)

end

module type Monad = sig
  include State

  type 'a t = st -> ('a * st)
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>) : 'a t -> 'b t -> 'b t

  (*val if_ : value -> unit t -> unit t -> unit t*)

  val run : 'a t -> 'a * st
  val step : st -> 'a t -> st

  val write_reg : machine_register -> value -> unit t
  val read_reg : machine_register -> value t
  val modify_reg : machine_register -> (value -> value) -> unit t

  val read_mem : cache -> value -> value t
  val write_mem : cache -> value -> value -> unit t

  val read_bytecode : value -> value t

end 
  
module Monad(S : State) = struct
  include S

  type 'a t = st -> ('a * st)

  let bind m f =
    fun s ->
      let x, s = m s in
      f x s

  let return a = fun s -> (a, s)

  let run m = m (S.initial ())

  let step st m = snd (m st)

  let (>>=) = bind
  let (>>) m f = bind m (fun _ -> f)

  (*let if_ c t f st = .... this is not good...
    (if c <> zero then t st else f st);
    ((), st)*)

  let read_reg r st = S.get_reg st r

  let write_reg r v st = ((), S.set_reg st r v)

  let modify_reg r f = 
    read_reg r >>= fun v -> write_reg r (f v)

  let read_mem cache adr st = 
    S.get_mem st cache (srl adr 3) 

  let write_mem cache adr value st = 
    ((), S.set_mem st cache (srl adr 3) value)

  let read_bytecode adr st = 
    let ins,st = S.get_mem st `program (srl adr 3) in
    let sel = adr &: (const 4) in
    let ins = sra (if sel = (const 0) then sll ins 32 else ins) 32 in
    (ins, st)

end 

module Opcodes(M : Monad) = struct

  open M

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

  let val_int x = (sll x 1) |: one
  let int_val x = sra x 1

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

  let header ptr = read_mem `mem (ptr -: one)
  let field ptr ofs = read_mem `mem (ptr +: (aofs ofs))

  (* instructions *)

  let acc i = read_stack i >>= write_reg `accu 

  let acc_arg = pop_arg >>= acc

  let push = push_stack_accu

  let pushacc arg = 
    push_stack_accu >>
    read_stack arg >>= fun data ->
    write_reg `accu data

  let pushacc_arg = pop_arg >>= pushacc

  let pop = 
    pop_arg >>= fun ofs -> modify_reg `sp (fun sp -> sp +: (aofs ofs))

  let assign = 
    pop_arg >>= fun ofs -> 
    read_reg `accu >>= write_stack ofs 

  let envacc i = 
    read_reg `env >>= fun ptr -> field ptr i >>= write_reg `accu

  let envacc_arg = pop_arg >>= envacc

  let pushenvacc i = push_stack_accu >> envacc i

  let pushenvacc_arg = pop_arg >>= pushenvacc

  let push_retaddr = 
    read_reg `extra_args >>= fun eargs -> push_stack (val_int eargs) >>
    read_reg `env >>= push_stack >>
    read_reg `pc >>= fun pc -> pop_arg >>= fun ofs -> 
    push_stack (pc +: (aofs ofs))

  let apply = 
    pop_arg >>= fun eargs -> write_reg `extra_args (eargs -: one) >>
    read_reg `accu >>= fun accu -> read_mem `mem accu >>= write_reg `pc >>
    write_reg `env accu

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

  let const i = write_reg `accu i
  let constint = pop_arg >>= const 

end

module type M_eval = Monad 
    with type value = int64
     and type st = State_eval.st
module M : M_eval = Monad(State_eval) 
module O = Opcodes(M)

module type M_poly = Monad 
    with type value = State_poly.value
     and type st = State_poly.st
module M' : M_poly = Monad(State_poly) 
module O' = Opcodes(M')

open M
open O

