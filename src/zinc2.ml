(* compile hardware design from [Interp] *)

module S = Interp.State_poly
module M = Interp.Monad(struct let trace = false end)(S) 
module O = Interp.Opcodes(M)

(* S.print @@ snd @@ M.step S.empty (O.accn S.(const 3));; *)

module Compile = struct

  open Machine
  open Interp

  let initr x = Array.init Machine.num_machine_registers (fun _ -> x)
  let getr a i = a.( Machine.Enum_machine_register.from_enum i )
  let setr a i v = a.( Machine.Enum_machine_register.from_enum i ) <- v

  let initc x = Array.init Machine.num_cache_spaces (fun _ -> x)
  let getc a i = a.( Machine.Enum_cache.from_enum i )
  let setc a i v = a.( Machine.Enum_cache.from_enum i ) <- v

  (* schedule the instructions according to dependancies.
  
      We can do this according to the indices assigned to commands as an initial
      approximation.  This will include machine register reads which in fact are
      a bit different as they should really only affect the schedule after a write.

   *)
  
  let expr_is_const = function Const(_) -> true | _ -> false
  let expr_val = function Const(n) -> n | _ -> failwith "expr is not a constant"
  let expr_is e n = match e with Const(m) when n=m -> true | _ -> false

  let const_eval op e0 e1 = 
    if expr_is_const e0 && expr_is_const e1 then
      let e0, e1 = expr_val e0, expr_val e1 in
      match op with
      | "+" -> Some(Const(e0 + e1))
      | "-" -> Some(Const(e0 - e1))
      | "*" -> Some(Const(e0 * e1))
      | "/" -> Some(Const(e0 / e1))
      | "%" -> Some(Const(e0 mod e1))
      | "&" -> Some(Const(e0 land e1))
      | "|" -> Some(Const(e0 lor e1))
      | "^" -> Some(Const(e0 lxor e1))
      | "~" -> Some(Const(lnot e0))
      | "<<" -> Some(Const(e0 lsl e1))
      | ">>" -> Some(Const(e0 lsr e1))
      | ">>+" -> Some(Const(e0 asr e1))
      | "==" -> Some(Const(if e0 = e1 then 1 else 0))
      | "<>" -> Some(Const(if e0 <> e1 then 1 else 0))
      | _ -> None
    else 
      None

  let rec simplify_expr e = 
    match e with
    | Op(op, e0, e1) -> begin
      let e0, e1 = simplify_expr e0, simplify_expr e1 in
      match const_eval op e0 e1 with
      | Some(e) -> e
      | None -> begin
        match op with
        | "+" when expr_is e0 0 -> e1 (* a + 0 = a *)
        | "+" when expr_is e1 0 -> e0 (* 0 + a = a *)
        | "-" when expr_is e1 0 -> e0 (* a - 0 = a *)
        | "*" when expr_is e1 1 -> e0 (* a * 1 = a *)
        | "*" when expr_is e0 1 -> e1 (* 1 * a = a *)
        | "/" when expr_is e1 1 -> e0 (* a / 1 = a *)
        | "<<" when expr_is e1 0 -> e1 (* a lsl 0 = a *)
        | ">>" when expr_is e1 0 -> e1 (* a lsr 0 = a *)
        | ">>+" when expr_is e1 0 -> e1 (* a asr 0 = a *)
        | _ -> Op(op, e0, e1)
      end
    end
    | Const(_) -> e
    | Val(_) -> e

  let rec expr_deps e =
    match e with
    | Op(_, e0, e1) -> expr_deps e0 @ expr_deps e1
    | Val(id) -> [id]
    | Const(_) -> []

  let rec stat_deps x = 
    match x with
    | Get_reg(_, reg) -> `cmd(x, [])
    | Set_reg(_, value) -> `cmd(x, expr_deps value)
    | Get_mem(_, _, addr) -> `cmd(x, expr_deps addr)
    | Set_mem(_, addr, value) -> `cmd(x, expr_deps addr @ expr_deps value)
    | Cond(c, t, f) -> `cond(x, expr_deps c, List.map stat_deps t, List.map stat_deps f)
    | Iter(_, _, f, t, c) -> `iter(x, expr_deps f @ expr_deps t, List.map stat_deps c)
    
  let rec simplify_stat = function
    | Get_reg(id, reg) -> Get_reg(id, reg)
    | Set_reg(reg, value) -> Set_reg(reg, simplify_expr value)
    | Get_mem(id, cache, addr) -> Get_mem(id, cache, simplify_expr addr)
    | Set_mem(cache, addr, value) -> Set_mem(cache, simplify_expr addr, simplify_expr value)
    | Cond(c, t, f) -> 
      Cond(simplify_expr c, simplify_stats t, simplify_stats f)
    | Iter(ud, id, f, t, c) -> 
      Iter(ud, id, simplify_expr f, simplify_expr t, simplify_stats c)

  and simplify_stats x = List.map simplify_stat x

  let simplify st = { st with cmd = simplify_stats st.cmd }

  module M = Map.Make(struct
    type t = int
    let compare = compare
  end)

  (* 
      Lets try to understand the problems here.

      We are temporary values (ie _0, _1 etc) with calls to
      Get_reg and Get_mem *only*.  
      
      For Get_mem these become register assignments and a state update.
      Relative to Set_mem, therefore, memory accesses and the generated
      values are serialised.

      For Get_reg this reflects the current value of a register.  
      We use these values in combinatorial expressions passed
      to Set_reg, Set_mem and also used in the looping and if constructs.

      A Set_reg does not generate a new state, rather we update the 
      combinatorial expression.  When a Set_mem comes a long we do need
      to update the register.  At this point the combinatorial expressions
      become invalid.  Is that OK?  I guess they are committed at that 
      point so maybe it is.

      None-the-less the previous temporary value *IS* still in scope
      and is no longer valid.  Somehow we will need to latch the value
      and start using it instead in potential future expressions.
      Note that it is very likely the old value wont be used so
      we would hope this register could disappear somehow.

      An example sequence.

      _0 = sp;
      sp = _0 + 4; // s = sp + 4, but not yet committed
      _1 = sp;     // read new sp value
      pc = _0;     // pc = initial value of sp as read at start
      accu = _1;   // accu = new value of sp ie sp+4
      
      No states are generated here.

      This is affected by an intermediate state being generated

      _0 = sp;
      sp = _0 + 4;
      _1 = program[0]; // this will insert a state
      _2 = sp;
      pc = _0;         // pc = sp (new value) ** BAD VALUE **
      accu = _2;       // OK

  *)

  (* 

      Lets try to simplify things into passes.
      
      1) Split into states.  We should be able to generate
         the statemachine structure before we start.

      2) We could optionally derive the liveness of variables
         within states, but this is an optimisation.

      3) Generate logic for each state.  Understanding where
         variables are generated and used (ie if the states are
         different) will help in creating the logic structures)

  *)

  open HardCaml.Signal
  open Comb

  type env = 
    {
      id_to_wire : t M.t;
      regs : t array;
    }

  let dbits = 64 (* XXX *)

  let init_env () = 
    {
      id_to_wire = M.empty;
      regs = Array.init Machine.num_machine_registers
        (fun i ->
          let name = Machine.Show_machine_register.show
            (Machine.Enum_machine_register.to_enum i)
          in
          zero dbits -- name);
    }

  type state = 
    {
      n : int;
      next : 
        [ `next of int (* statemachine jumps (generally end of sequence?) *)
        | `mem of sp_cmd * int (* memory operation *)
        | `branch of sp_cmd * int * int (* branch *) 
        ];
      instrs : sp_cmd list;
    }

  let print_states sts = 
    List.iter (fun st ->
      let open Printf in
      printf "****** %i\n" st.n;
      S.(print {id=0; cmd=st.instrs});
      printf "next = %s\n"
        (match st.next with
        | `next(i) -> sprintf "%i" i
        | `mem(_,i) -> sprintf "%i [mem]" i
        | `branch(c,a,b) -> sprintf "%i/%i [branch]" a b);
      printf "\n"
    ) sts

  let compile st = 
    let cmd = State_poly.normalise st.cmd in
    let const_val = function
      | Const(x) -> x
      | _ -> failwith "not a constant expression"
    in
    let rec compile_expr env = function
      | Op(op, a', b') -> begin
        let a, b = 
          try compile_expr env a', compile_expr env b' 
          with _ -> failwith "failed to look up subexpression"
        in
        match op with
        | "+" -> a +: b
        | "-" -> a -: b
        | ">>" -> srl a (const_val b')
        | ">>+" -> sra a (const_val b')
        | "<<" -> sll a (const_val b')
        | _ -> failwith ("unknown expression operator '" ^ op ^ "'")
      end
      | Val(id) -> (try M.find id env.id_to_wire 
                    with _ -> failwith ("cant find variable " ^ string_of_int id))
      | Const(x) -> consti dbits x
    in
    let rec compile_stats env = function
      | [] -> env

      (* (psuedo-) update registers and expressions *)
      | Get_reg(id, reg) :: t -> begin
        let env = { env with id_to_wire = M.add id (getr env.regs reg) env.id_to_wire; } in
        compile_stats env t
      end
      | Set_reg(reg, vl) :: t -> begin
        setr env.regs reg (compile_expr env vl);
        compile_stats env t
      end

      (* memory io and state completion *)
      | Get_mem(id, c, a) :: t -> begin
        let env = { env with id_to_wire = M.add id (wire dbits) env.id_to_wire; } in
        compile_stats env t
      end
      | Set_mem(c, a, d) :: t -> begin
        compile_stats env t
      end

      | _ -> failwith "not yet"
    in

    let newst ?(next=(`next(0))) ?(instrs=[]) ?(n=0) () = { n; next; instrs } in
    let rs' rstate = function [] -> 0 | _ -> rstate in

    let rec compile_states (cs,rs,sts) instrs = function
      | [] -> cs+1,rs,(newst ~instrs ~n:cs () :: sts) 

      (* Get/Set_reg; push instructions *)
      | (Get_reg(_) as x) :: t -> 
        compile_states (cs,rs,sts) (x::instrs) t
      | (Set_reg(_) as x) :: t -> 
        compile_states (cs,rs,sts) (x::instrs) t
      
      (* Get/Set_mem generate new states *)
      | (Set_mem(_) as x) :: t -> 
        let st = newst ~instrs ~next:(`mem(x,if t=[] then rs else cs+1)) ~n:cs () in
        compile_states (cs+1,rs,st::sts) [] t

      | (Get_mem(_) as x) :: t -> 
        let st = newst ~instrs ~next:(`mem(x,if t=[] then rs else cs+1)) ~n:cs () in
        compile_states (cs+1,rs,st::sts) [] t

      (* control flow creates new states *)
      | (Cond(c,a,b) as x) :: t -> 
        (* create branch states *)
        let ns,bs0 = cs+1,cs+2 in
        let (bs1,_,sts) = compile_states (bs0,ns,sts) [] a in
        let (_  ,_,sts) = compile_states (bs1,ns,sts) [] b in
        (* create cur state *)
        let st = newst ~instrs ~n:cs ~next:(`branch(x,bs0,bs1)) () in
        (* continue with next states *)
        compile_states (ns,rs,st::sts) [] t

      | (Iter(_) as x) :: t -> failwith "not sure about iter yet!"
    in

    (*compile_stats (init_env ()) cmd*)
    let _,_,sts = compile_states (0,0,[]) [] cmd in
    let sts = List.sort (fun a b -> compare a.n  b.n) sts in

    let () = print_states sts in
    ()

end


