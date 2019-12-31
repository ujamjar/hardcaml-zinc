open Base
open Stdio.Out_channel
open Hardcaml_zinc

module M =
  Interp.Monad
    (struct
      let trace = false
    end)
    (Interp.State_poly)

module O = Interp.Opcodes (M)

let show instr =
  try
    let s = O.dispatch instr Interp.State_poly.empty in
    snd s |> Compile_hardware.Statement.simplify |> Interp.State_poly.print;
    printf "\n";
    print_s [%message "" ~_:(s : O.returns * Interp.sp_st)];
    printf "\n"
  with
  | _ -> printf "NOT IMPLEMENTED\n"
;;

let%expect_test "show all instruction implementations" =
  List.iter Opcode.all ~f:(fun opcode ->
      printf "____ %s ____\n\n" (Opcode.to_string opcode);
      show opcode);
  [%expect
    {|
    ____ ACC0 ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    accu = _1;

    (step
     ((id 2)
      (cmd
       ((Set_reg Accu (Val 1))
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ ACC1 ____

    _0 = sp;
    _1 = stack[((_0+8)>>3)];
    accu = _1;

    (step
     ((id 2)
      (cmd
       ((Set_reg Accu (Val 1))
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 1) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ ACC2 ____

    _0 = sp;
    _1 = stack[((_0+16)>>3)];
    accu = _1;

    (step
     ((id 2)
      (cmd
       ((Set_reg Accu (Val 1))
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 2) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ ACC3 ____

    _0 = sp;
    _1 = stack[((_0+24)>>3)];
    accu = _1;

    (step
     ((id 2)
      (cmd
       ((Set_reg Accu (Val 1))
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 3) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ ACC4 ____

    _0 = sp;
    _1 = stack[((_0+32)>>3)];
    accu = _1;

    (step
     ((id 2)
      (cmd
       ((Set_reg Accu (Val 1))
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 4) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ ACC5 ____

    _0 = sp;
    _1 = stack[((_0+40)>>3)];
    accu = _1;

    (step
     ((id 2)
      (cmd
       ((Set_reg Accu (Val 1))
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 5) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ ACC6 ____

    _0 = sp;
    _1 = stack[((_0+48)>>3)];
    accu = _1;

    (step
     ((id 2)
      (cmd
       ((Set_reg Accu (Val 1))
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 6) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ ACC7 ____

    _0 = sp;
    _1 = stack[((_0+56)>>3)];
    accu = _1;

    (step
     ((id 2)
      (cmd
       ((Set_reg Accu (Val 1))
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 7) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ ACC ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = sp;
    _4 = stack[((_3+((_1>>+32)<<3))>>3)];
    accu = _4;

    (step
     ((id 5)
      (cmd
       ((Set_reg Accu (Val 4))
        (Get_mem 4 Stack
         (Op >> (Op + (Val 3) (Op << (Op >>+ (Val 1) (Const 32)) (Const 3)))
          (Const 3)))
        (Get_reg 3 Sp) (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ PUSH ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;

    (step
     ((id 3)
      (cmd
       ((Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ PUSHACC0 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = sp;
    _4 = stack[(_3>>3)];
    accu = _4;

    (step
     ((id 5)
      (cmd
       ((Set_reg Accu (Val 4))
        (Get_mem 4 Stack
         (Op >> (Op + (Val 3) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 3 Sp)
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ PUSHACC1 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = sp;
    _4 = stack[((_3+8)>>3)];
    accu = _4;

    (step
     ((id 5)
      (cmd
       ((Set_reg Accu (Val 4))
        (Get_mem 4 Stack
         (Op >> (Op + (Val 3) (Op << (Const 1) (Const 3))) (Const 3)))
        (Get_reg 3 Sp)
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ PUSHACC2 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = sp;
    _4 = stack[((_3+16)>>3)];
    accu = _4;

    (step
     ((id 5)
      (cmd
       ((Set_reg Accu (Val 4))
        (Get_mem 4 Stack
         (Op >> (Op + (Val 3) (Op << (Const 2) (Const 3))) (Const 3)))
        (Get_reg 3 Sp)
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ PUSHACC3 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = sp;
    _4 = stack[((_3+24)>>3)];
    accu = _4;

    (step
     ((id 5)
      (cmd
       ((Set_reg Accu (Val 4))
        (Get_mem 4 Stack
         (Op >> (Op + (Val 3) (Op << (Const 3) (Const 3))) (Const 3)))
        (Get_reg 3 Sp)
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ PUSHACC4 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = sp;
    _4 = stack[((_3+32)>>3)];
    accu = _4;

    (step
     ((id 5)
      (cmd
       ((Set_reg Accu (Val 4))
        (Get_mem 4 Stack
         (Op >> (Op + (Val 3) (Op << (Const 4) (Const 3))) (Const 3)))
        (Get_reg 3 Sp)
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ PUSHACC5 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = sp;
    _4 = stack[((_3+40)>>3)];
    accu = _4;

    (step
     ((id 5)
      (cmd
       ((Set_reg Accu (Val 4))
        (Get_mem 4 Stack
         (Op >> (Op + (Val 3) (Op << (Const 5) (Const 3))) (Const 3)))
        (Get_reg 3 Sp)
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ PUSHACC6 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = sp;
    _4 = stack[((_3+48)>>3)];
    accu = _4;

    (step
     ((id 5)
      (cmd
       ((Set_reg Accu (Val 4))
        (Get_mem 4 Stack
         (Op >> (Op + (Val 3) (Op << (Const 6) (Const 3))) (Const 3)))
        (Get_reg 3 Sp)
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ PUSHACC7 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = sp;
    _4 = stack[((_3+56)>>3)];
    accu = _4;

    (step
     ((id 5)
      (cmd
       ((Set_reg Accu (Val 4))
        (Get_mem 4 Stack
         (Op >> (Op + (Val 3) (Op << (Const 7) (Const 3))) (Const 3)))
        (Get_reg 3 Sp)
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ PUSHACC ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = accu;
    _4 = sp;
    sp = (_4-8);
    _5 = sp;
    stack[(_5>>3)] = _3;
    _6 = sp;
    _7 = stack[((_6+((_1>>+32)<<3))>>3)];
    accu = _7;

    (step
     ((id 8)
      (cmd
       ((Set_reg Accu (Val 7))
        (Get_mem 7 Stack
         (Op >> (Op + (Val 6) (Op << (Op >>+ (Val 1) (Const 32)) (Const 3)))
          (Const 3)))
        (Get_reg 6 Sp)
        (Set_mem Stack
         (Op >> (Op + (Val 5) (Op << (Const 0) (Const 3))) (Const 3)) (Val 3))
        (Get_reg 5 Sp) (Set_reg Sp (Op - (Val 4) (Const 8))) (Get_reg 4 Sp)
        (Get_reg 3 Accu) (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ POP ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = sp;
    sp = (_3+((_1>>+32)<<3));

    (step
     ((id 4)
      (cmd
       ((Set_reg Sp (Op + (Val 3) (Op << (Op >>+ (Val 1) (Const 32)) (Const 3))))
        (Get_reg 3 Sp) (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ ASSIGN ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = accu;
    _4 = sp;
    stack[((_4+((_1>>+32)<<3))>>3)] = _3;
    accu = 1;

    (step
     ((id 5)
      (cmd
       ((Set_reg Accu (Const 1))
        (Set_mem Stack
         (Op >> (Op + (Val 4) (Op << (Op >>+ (Val 1) (Const 32)) (Const 3)))
          (Const 3))
         (Val 3))
        (Get_reg 4 Sp) (Get_reg 3 Accu) (Set_reg Pc (Op + (Val 2) (Const 4)))
        (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ ENVACC1 ____

    _0 = env;
    _1 = mem[((_0+8)>>3)];
    accu = _1;

    (step
     ((id 2)
      (cmd
       ((Set_reg Accu (Val 1))
        (Get_mem 1 Mem
         (Op >> (Op + (Val 0) (Op << (Const 1) (Const 3))) (Const 3)))
        (Get_reg 0 Env)))))

    ____ ENVACC2 ____

    _0 = env;
    _1 = mem[((_0+16)>>3)];
    accu = _1;

    (step
     ((id 2)
      (cmd
       ((Set_reg Accu (Val 1))
        (Get_mem 1 Mem
         (Op >> (Op + (Val 0) (Op << (Const 2) (Const 3))) (Const 3)))
        (Get_reg 0 Env)))))

    ____ ENVACC3 ____

    _0 = env;
    _1 = mem[((_0+24)>>3)];
    accu = _1;

    (step
     ((id 2)
      (cmd
       ((Set_reg Accu (Val 1))
        (Get_mem 1 Mem
         (Op >> (Op + (Val 0) (Op << (Const 3) (Const 3))) (Const 3)))
        (Get_reg 0 Env)))))

    ____ ENVACC4 ____

    _0 = env;
    _1 = mem[((_0+32)>>3)];
    accu = _1;

    (step
     ((id 2)
      (cmd
       ((Set_reg Accu (Val 1))
        (Get_mem 1 Mem
         (Op >> (Op + (Val 0) (Op << (Const 4) (Const 3))) (Const 3)))
        (Get_reg 0 Env)))))

    ____ ENVACC ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = env;
    _4 = mem[((_3+((_1>>+32)<<3))>>3)];
    accu = _4;

    (step
     ((id 5)
      (cmd
       ((Set_reg Accu (Val 4))
        (Get_mem 4 Mem
         (Op >> (Op + (Val 3) (Op << (Op >>+ (Val 1) (Const 32)) (Const 3)))
          (Const 3)))
        (Get_reg 3 Env) (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ PUSHENVACC1 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = env;
    _4 = mem[((_3+8)>>3)];
    accu = _4;

    (step
     ((id 5)
      (cmd
       ((Set_reg Accu (Val 4))
        (Get_mem 4 Mem
         (Op >> (Op + (Val 3) (Op << (Const 1) (Const 3))) (Const 3)))
        (Get_reg 3 Env)
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ PUSHENVACC2 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = env;
    _4 = mem[((_3+16)>>3)];
    accu = _4;

    (step
     ((id 5)
      (cmd
       ((Set_reg Accu (Val 4))
        (Get_mem 4 Mem
         (Op >> (Op + (Val 3) (Op << (Const 2) (Const 3))) (Const 3)))
        (Get_reg 3 Env)
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ PUSHENVACC3 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = env;
    _4 = mem[((_3+24)>>3)];
    accu = _4;

    (step
     ((id 5)
      (cmd
       ((Set_reg Accu (Val 4))
        (Get_mem 4 Mem
         (Op >> (Op + (Val 3) (Op << (Const 3) (Const 3))) (Const 3)))
        (Get_reg 3 Env)
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ PUSHENVACC4 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = env;
    _4 = mem[((_3+32)>>3)];
    accu = _4;

    (step
     ((id 5)
      (cmd
       ((Set_reg Accu (Val 4))
        (Get_mem 4 Mem
         (Op >> (Op + (Val 3) (Op << (Const 4) (Const 3))) (Const 3)))
        (Get_reg 3 Env)
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ PUSHENVACC ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = accu;
    _4 = sp;
    sp = (_4-8);
    _5 = sp;
    stack[(_5>>3)] = _3;
    _6 = env;
    _7 = mem[((_6+((_1>>+32)<<3))>>3)];
    accu = _7;

    (step
     ((id 8)
      (cmd
       ((Set_reg Accu (Val 7))
        (Get_mem 7 Mem
         (Op >> (Op + (Val 6) (Op << (Op >>+ (Val 1) (Const 32)) (Const 3)))
          (Const 3)))
        (Get_reg 6 Env)
        (Set_mem Stack
         (Op >> (Op + (Val 5) (Op << (Const 0) (Const 3))) (Const 3)) (Val 3))
        (Get_reg 5 Sp) (Set_reg Sp (Op - (Val 4) (Const 8))) (Get_reg 4 Sp)
        (Get_reg 3 Accu) (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ PUSH_RETADDR ____

    _0 = extra_args;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = ((_0<<1)|1);
    _3 = env;
    _4 = sp;
    sp = (_4-8);
    _5 = sp;
    stack[(_5>>3)] = _3;
    _6 = pc;
    _7 = pc;
    _8 = program[(_7>>3)];
    _9 = pc;
    pc = (_9+4);
    _10 = sp;
    sp = (_10-8);
    _11 = sp;
    stack[(_11>>3)] = (_6+((_8>>+32)<<2));

    (step
     ((id 12)
      (cmd
       ((Set_mem Stack
         (Op >> (Op + (Val 11) (Op << (Const 0) (Const 3))) (Const 3))
         (Op + (Val 6) (Op << (Op >>+ (Val 8) (Const 32)) (Const 2))))
        (Get_reg 11 Sp) (Set_reg Sp (Op - (Val 10) (Const 8))) (Get_reg 10 Sp)
        (Set_reg Pc (Op + (Val 9) (Const 4))) (Get_reg 9 Pc)
        (Get_mem 8 Program
         (Op >> (Op + (Val 7) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 7 Pc) (Get_reg 6 Pc)
        (Set_mem Stack
         (Op >> (Op + (Val 5) (Op << (Const 0) (Const 3))) (Const 3)) (Val 3))
        (Get_reg 5 Sp) (Set_reg Sp (Op - (Val 4) (Const 8))) (Get_reg 4 Sp)
        (Get_reg 3 Env)
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3))
         (Op | (Op << (Val 0) (Const 1)) (Const 1)))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Extra_args)))))

    ____ APPLY ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    extra_args = ((_1>>+32)-1);
    _3 = accu;
    _4 = mem[(_3>>3)];
    pc = _4;
    env = _3;

    (step
     ((id 5)
      (cmd
       ((Set_reg Env (Val 3)) (Set_reg Pc (Val 4))
        (Get_mem 4 Mem (Op >> (Val 3) (Const 3))) (Get_reg 3 Accu)
        (Set_reg Extra_args (Op - (Op >>+ (Val 1) (Const 32)) (Const 1)))
        (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ APPLY1 ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = extra_args;
    _4 = sp;
    sp = (_4-8);
    _5 = sp;
    stack[(_5>>3)] = ((_3<<1)|1);
    _6 = env;
    _7 = sp;
    sp = (_7-8);
    _8 = sp;
    stack[(_8>>3)] = _6;
    _9 = pc;
    _10 = sp;
    sp = (_10-8);
    _11 = sp;
    stack[(_11>>3)] = _9;
    _12 = sp;
    sp = (_12-8);
    _13 = sp;
    stack[(_13>>3)] = _1;
    _14 = accu;
    _15 = mem[(_14>>3)];
    pc = _15;
    env = _14;
    extra_args = 0;

    (step
     ((id 16)
      (cmd
       ((Set_reg Extra_args (Const 0)) (Set_reg Env (Val 14))
        (Set_reg Pc (Val 15)) (Get_mem 15 Mem (Op >> (Val 14) (Const 3)))
        (Get_reg 14 Accu)
        (Set_mem Stack
         (Op >> (Op + (Val 13) (Op << (Const 0) (Const 3))) (Const 3)) (Val 1))
        (Get_reg 13 Sp) (Set_reg Sp (Op - (Val 12) (Const 8))) (Get_reg 12 Sp)
        (Set_mem Stack
         (Op >> (Op + (Val 11) (Op << (Const 0) (Const 3))) (Const 3)) (Val 9))
        (Get_reg 11 Sp) (Set_reg Sp (Op - (Val 10) (Const 8))) (Get_reg 10 Sp)
        (Get_reg 9 Pc)
        (Set_mem Stack
         (Op >> (Op + (Val 8) (Op << (Const 0) (Const 3))) (Const 3)) (Val 6))
        (Get_reg 8 Sp) (Set_reg Sp (Op - (Val 7) (Const 8))) (Get_reg 7 Sp)
        (Get_reg 6 Env)
        (Set_mem Stack
         (Op >> (Op + (Val 5) (Op << (Const 0) (Const 3))) (Const 3))
         (Op | (Op << (Val 3) (Const 1)) (Const 1)))
        (Get_reg 5 Sp) (Set_reg Sp (Op - (Val 4) (Const 8))) (Get_reg 4 Sp)
        (Get_reg 3 Extra_args) (Set_reg Sp (Op + (Val 2) (Const 8)))
        (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ APPLY2 ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = sp;
    _4 = stack[(_3>>3)];
    _5 = sp;
    sp = (_5+8);
    _6 = extra_args;
    _7 = sp;
    sp = (_7-8);
    _8 = sp;
    stack[(_8>>3)] = ((_6<<1)|1);
    _9 = env;
    _10 = sp;
    sp = (_10-8);
    _11 = sp;
    stack[(_11>>3)] = _9;
    _12 = pc;
    _13 = sp;
    sp = (_13-8);
    _14 = sp;
    stack[(_14>>3)] = _12;
    _15 = sp;
    sp = (_15-8);
    _16 = sp;
    stack[(_16>>3)] = _4;
    _17 = sp;
    sp = (_17-8);
    _18 = sp;
    stack[(_18>>3)] = _1;
    _19 = accu;
    _20 = mem[(_19>>3)];
    pc = _20;
    env = _19;
    extra_args = 1;

    (step
     ((id 21)
      (cmd
       ((Set_reg Extra_args (Const 1)) (Set_reg Env (Val 19))
        (Set_reg Pc (Val 20)) (Get_mem 20 Mem (Op >> (Val 19) (Const 3)))
        (Get_reg 19 Accu)
        (Set_mem Stack
         (Op >> (Op + (Val 18) (Op << (Const 0) (Const 3))) (Const 3)) (Val 1))
        (Get_reg 18 Sp) (Set_reg Sp (Op - (Val 17) (Const 8))) (Get_reg 17 Sp)
        (Set_mem Stack
         (Op >> (Op + (Val 16) (Op << (Const 0) (Const 3))) (Const 3)) (Val 4))
        (Get_reg 16 Sp) (Set_reg Sp (Op - (Val 15) (Const 8))) (Get_reg 15 Sp)
        (Set_mem Stack
         (Op >> (Op + (Val 14) (Op << (Const 0) (Const 3))) (Const 3)) (Val 12))
        (Get_reg 14 Sp) (Set_reg Sp (Op - (Val 13) (Const 8))) (Get_reg 13 Sp)
        (Get_reg 12 Pc)
        (Set_mem Stack
         (Op >> (Op + (Val 11) (Op << (Const 0) (Const 3))) (Const 3)) (Val 9))
        (Get_reg 11 Sp) (Set_reg Sp (Op - (Val 10) (Const 8))) (Get_reg 10 Sp)
        (Get_reg 9 Env)
        (Set_mem Stack
         (Op >> (Op + (Val 8) (Op << (Const 0) (Const 3))) (Const 3))
         (Op | (Op << (Val 6) (Const 1)) (Const 1)))
        (Get_reg 8 Sp) (Set_reg Sp (Op - (Val 7) (Const 8))) (Get_reg 7 Sp)
        (Get_reg 6 Extra_args) (Set_reg Sp (Op + (Val 5) (Const 8)))
        (Get_reg 5 Sp)
        (Get_mem 4 Stack
         (Op >> (Op + (Val 3) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 3 Sp) (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ APPLY3 ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = sp;
    _4 = stack[(_3>>3)];
    _5 = sp;
    sp = (_5+8);
    _6 = sp;
    _7 = stack[(_6>>3)];
    _8 = sp;
    sp = (_8+8);
    _9 = extra_args;
    _10 = sp;
    sp = (_10-8);
    _11 = sp;
    stack[(_11>>3)] = ((_9<<1)|1);
    _12 = env;
    _13 = sp;
    sp = (_13-8);
    _14 = sp;
    stack[(_14>>3)] = _12;
    _15 = pc;
    _16 = sp;
    sp = (_16-8);
    _17 = sp;
    stack[(_17>>3)] = _15;
    _18 = sp;
    sp = (_18-8);
    _19 = sp;
    stack[(_19>>3)] = _7;
    _20 = sp;
    sp = (_20-8);
    _21 = sp;
    stack[(_21>>3)] = _4;
    _22 = sp;
    sp = (_22-8);
    _23 = sp;
    stack[(_23>>3)] = _1;
    _24 = accu;
    _25 = mem[(_24>>3)];
    pc = _25;
    env = _24;
    extra_args = 2;

    (step
     ((id 26)
      (cmd
       ((Set_reg Extra_args (Const 2)) (Set_reg Env (Val 24))
        (Set_reg Pc (Val 25)) (Get_mem 25 Mem (Op >> (Val 24) (Const 3)))
        (Get_reg 24 Accu)
        (Set_mem Stack
         (Op >> (Op + (Val 23) (Op << (Const 0) (Const 3))) (Const 3)) (Val 1))
        (Get_reg 23 Sp) (Set_reg Sp (Op - (Val 22) (Const 8))) (Get_reg 22 Sp)
        (Set_mem Stack
         (Op >> (Op + (Val 21) (Op << (Const 0) (Const 3))) (Const 3)) (Val 4))
        (Get_reg 21 Sp) (Set_reg Sp (Op - (Val 20) (Const 8))) (Get_reg 20 Sp)
        (Set_mem Stack
         (Op >> (Op + (Val 19) (Op << (Const 0) (Const 3))) (Const 3)) (Val 7))
        (Get_reg 19 Sp) (Set_reg Sp (Op - (Val 18) (Const 8))) (Get_reg 18 Sp)
        (Set_mem Stack
         (Op >> (Op + (Val 17) (Op << (Const 0) (Const 3))) (Const 3)) (Val 15))
        (Get_reg 17 Sp) (Set_reg Sp (Op - (Val 16) (Const 8))) (Get_reg 16 Sp)
        (Get_reg 15 Pc)
        (Set_mem Stack
         (Op >> (Op + (Val 14) (Op << (Const 0) (Const 3))) (Const 3)) (Val 12))
        (Get_reg 14 Sp) (Set_reg Sp (Op - (Val 13) (Const 8))) (Get_reg 13 Sp)
        (Get_reg 12 Env)
        (Set_mem Stack
         (Op >> (Op + (Val 11) (Op << (Const 0) (Const 3))) (Const 3))
         (Op | (Op << (Val 9) (Const 1)) (Const 1)))
        (Get_reg 11 Sp) (Set_reg Sp (Op - (Val 10) (Const 8))) (Get_reg 10 Sp)
        (Get_reg 9 Extra_args) (Set_reg Sp (Op + (Val 8) (Const 8)))
        (Get_reg 8 Sp)
        (Get_mem 7 Stack
         (Op >> (Op + (Val 6) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 6 Sp) (Set_reg Sp (Op + (Val 5) (Const 8))) (Get_reg 5 Sp)
        (Get_mem 4 Stack
         (Op >> (Op + (Val 3) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 3 Sp) (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ APPTERM ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = pc;
    _4 = program[(_3>>3)];
    _5 = pc;
    pc = (_5+4);
    _6 = sp;
    for _7=[((_1>>+32)-1) downto 0] do
      _8 = sp;
      _9 = stack[((_8+(_7<<3))>>3)];
      _10 = sp;
      stack[((_10+((_7+((_4>>+32)-(_1>>+32)))<<3))>>3)] = _9;
    done
    sp = (_6+(((_4>>+32)-(_1>>+32))<<3));
    _11 = accu;
    _12 = mem[(_11>>3)];
    pc = _12;
    env = _11;
    _13 = extra_args;
    extra_args = (_13+((_1>>+32)-1));

    (step
     ((id 14)
      (cmd
       ((Set_reg Extra_args
         (Op + (Val 13) (Op - (Op >>+ (Val 1) (Const 32)) (Const 1))))
        (Get_reg 13 Extra_args) (Set_reg Env (Val 11)) (Set_reg Pc (Val 12))
        (Get_mem 12 Mem (Op >> (Val 11) (Const 3))) (Get_reg 11 Accu)
        (Set_reg Sp
         (Op + (Val 6)
          (Op << (Op - (Op >>+ (Val 4) (Const 32)) (Op >>+ (Val 1) (Const 32)))
           (Const 3))))
        (Iter false 7 (Op - (Op >>+ (Val 1) (Const 32)) (Const 1)) (Const 0)
         ((Set_mem Stack
           (Op >>
            (Op + (Val 10)
             (Op <<
              (Op + (Val 7)
               (Op - (Op >>+ (Val 4) (Const 32)) (Op >>+ (Val 1) (Const 32))))
              (Const 3)))
            (Const 3))
           (Val 9))
          (Get_reg 10 Sp)
          (Get_mem 9 Stack
           (Op >> (Op + (Val 8) (Op << (Val 7) (Const 3))) (Const 3)))
          (Get_reg 8 Sp)))
        (Get_reg 6 Sp) (Set_reg Pc (Op + (Val 5) (Const 4))) (Get_reg 5 Pc)
        (Get_mem 4 Program
         (Op >> (Op + (Val 3) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 3 Pc) (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ APPTERM1 ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = pc;
    _4 = program[(_3>>3)];
    _5 = pc;
    pc = (_5+4);
    _6 = sp;
    sp = (_6+(((_4>>+32)-1)<<3));
    _7 = sp;
    sp = (_7-8);
    _8 = sp;
    stack[(_8>>3)] = _1;
    _9 = accu;
    _10 = mem[(_9>>3)];
    pc = _10;
    env = _9;
    _11 = extra_args;
    extra_args = _11;

    (step
     ((id 12)
      (cmd
       ((Set_reg Extra_args (Op + (Val 11) (Const 0))) (Get_reg 11 Extra_args)
        (Set_reg Env (Val 9)) (Set_reg Pc (Val 10))
        (Get_mem 10 Mem (Op >> (Val 9) (Const 3))) (Get_reg 9 Accu)
        (Set_mem Stack
         (Op >> (Op + (Val 8) (Op << (Const 0) (Const 3))) (Const 3)) (Val 1))
        (Get_reg 8 Sp) (Set_reg Sp (Op - (Val 7) (Const 8))) (Get_reg 7 Sp)
        (Set_reg Sp
         (Op + (Val 6)
          (Op << (Op - (Op >>+ (Val 4) (Const 32)) (Const 1)) (Const 3))))
        (Get_reg 6 Sp) (Set_reg Pc (Op + (Val 5) (Const 4))) (Get_reg 5 Pc)
        (Get_mem 4 Program
         (Op >> (Op + (Val 3) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 3 Pc) (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ APPTERM2 ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = sp;
    _4 = stack[(_3>>3)];
    _5 = sp;
    sp = (_5+8);
    _6 = pc;
    _7 = program[(_6>>3)];
    _8 = pc;
    pc = (_8+4);
    _9 = sp;
    sp = (_9+(((_7>>+32)-2)<<3));
    _10 = sp;
    sp = (_10-8);
    _11 = sp;
    stack[(_11>>3)] = _4;
    _12 = sp;
    sp = (_12-8);
    _13 = sp;
    stack[(_13>>3)] = _1;
    _14 = accu;
    _15 = mem[(_14>>3)];
    pc = _15;
    env = _14;
    _16 = extra_args;
    extra_args = (_16+1);

    (step
     ((id 17)
      (cmd
       ((Set_reg Extra_args (Op + (Val 16) (Const 1))) (Get_reg 16 Extra_args)
        (Set_reg Env (Val 14)) (Set_reg Pc (Val 15))
        (Get_mem 15 Mem (Op >> (Val 14) (Const 3))) (Get_reg 14 Accu)
        (Set_mem Stack
         (Op >> (Op + (Val 13) (Op << (Const 0) (Const 3))) (Const 3)) (Val 1))
        (Get_reg 13 Sp) (Set_reg Sp (Op - (Val 12) (Const 8))) (Get_reg 12 Sp)
        (Set_mem Stack
         (Op >> (Op + (Val 11) (Op << (Const 0) (Const 3))) (Const 3)) (Val 4))
        (Get_reg 11 Sp) (Set_reg Sp (Op - (Val 10) (Const 8))) (Get_reg 10 Sp)
        (Set_reg Sp
         (Op + (Val 9)
          (Op << (Op - (Op >>+ (Val 7) (Const 32)) (Const 2)) (Const 3))))
        (Get_reg 9 Sp) (Set_reg Pc (Op + (Val 8) (Const 4))) (Get_reg 8 Pc)
        (Get_mem 7 Program
         (Op >> (Op + (Val 6) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 6 Pc) (Set_reg Sp (Op + (Val 5) (Const 8))) (Get_reg 5 Sp)
        (Get_mem 4 Stack
         (Op >> (Op + (Val 3) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 3 Sp) (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ APPTERM3 ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = sp;
    _4 = stack[(_3>>3)];
    _5 = sp;
    sp = (_5+8);
    _6 = sp;
    _7 = stack[(_6>>3)];
    _8 = sp;
    sp = (_8+8);
    _9 = pc;
    _10 = program[(_9>>3)];
    _11 = pc;
    pc = (_11+4);
    _12 = sp;
    sp = (_12+(((_10>>+32)-3)<<3));
    _13 = sp;
    sp = (_13-8);
    _14 = sp;
    stack[(_14>>3)] = _7;
    _15 = sp;
    sp = (_15-8);
    _16 = sp;
    stack[(_16>>3)] = _4;
    _17 = sp;
    sp = (_17-8);
    _18 = sp;
    stack[(_18>>3)] = _1;
    _19 = accu;
    _20 = mem[(_19>>3)];
    pc = _20;
    env = _19;
    _21 = extra_args;
    extra_args = (_21+2);

    (step
     ((id 22)
      (cmd
       ((Set_reg Extra_args (Op + (Val 21) (Const 2))) (Get_reg 21 Extra_args)
        (Set_reg Env (Val 19)) (Set_reg Pc (Val 20))
        (Get_mem 20 Mem (Op >> (Val 19) (Const 3))) (Get_reg 19 Accu)
        (Set_mem Stack
         (Op >> (Op + (Val 18) (Op << (Const 0) (Const 3))) (Const 3)) (Val 1))
        (Get_reg 18 Sp) (Set_reg Sp (Op - (Val 17) (Const 8))) (Get_reg 17 Sp)
        (Set_mem Stack
         (Op >> (Op + (Val 16) (Op << (Const 0) (Const 3))) (Const 3)) (Val 4))
        (Get_reg 16 Sp) (Set_reg Sp (Op - (Val 15) (Const 8))) (Get_reg 15 Sp)
        (Set_mem Stack
         (Op >> (Op + (Val 14) (Op << (Const 0) (Const 3))) (Const 3)) (Val 7))
        (Get_reg 14 Sp) (Set_reg Sp (Op - (Val 13) (Const 8))) (Get_reg 13 Sp)
        (Set_reg Sp
         (Op + (Val 12)
          (Op << (Op - (Op >>+ (Val 10) (Const 32)) (Const 3)) (Const 3))))
        (Get_reg 12 Sp) (Set_reg Pc (Op + (Val 11) (Const 4))) (Get_reg 11 Pc)
        (Get_mem 10 Program
         (Op >> (Op + (Val 9) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 9 Pc) (Set_reg Sp (Op + (Val 8) (Const 8))) (Get_reg 8 Sp)
        (Get_mem 7 Stack
         (Op >> (Op + (Val 6) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 6 Sp) (Set_reg Sp (Op + (Val 5) (Const 8))) (Get_reg 5 Sp)
        (Get_mem 4 Stack
         (Op >> (Op + (Val 3) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 3 Sp) (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ RETURN ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = sp;
    sp = (_3+((_1>>+32)<<3));
    _4 = extra_args;
    if (_4>+0) then
      _5 = extra_args;
      extra_args = (_5-1);
      _6 = accu;
      _7 = mem[(_6>>3)];
      pc = _7;
      env = _6;
    else
      _8 = sp;
      _9 = stack[(_8>>3)];
      _10 = sp;
      sp = (_10+8);
      pc = _9;
      _11 = sp;
      _12 = stack[(_11>>3)];
      _13 = sp;
      sp = (_13+8);
      env = _12;
      _14 = sp;
      _15 = stack[(_14>>3)];
      _16 = sp;
      sp = (_16+8);
      extra_args = (_4>>+1);
    end

    (step
     ((id 17)
      (cmd
       ((Cond (Op >+ (Val 4) (Const 0))
         ((Set_reg Env (Val 6)) (Set_reg Pc (Val 7))
          (Get_mem 7 Mem (Op >> (Val 6) (Const 3))) (Get_reg 6 Accu)
          (Set_reg Extra_args (Op - (Val 5) (Const 1))) (Get_reg 5 Extra_args))
         ((Set_reg Extra_args (Op >>+ (Val 4) (Const 1)))
          (Set_reg Sp (Op + (Val 16) (Const 8))) (Get_reg 16 Sp)
          (Get_mem 15 Stack
           (Op >> (Op + (Val 14) (Op << (Const 0) (Const 3))) (Const 3)))
          (Get_reg 14 Sp) (Set_reg Env (Val 12))
          (Set_reg Sp (Op + (Val 13) (Const 8))) (Get_reg 13 Sp)
          (Get_mem 12 Stack
           (Op >> (Op + (Val 11) (Op << (Const 0) (Const 3))) (Const 3)))
          (Get_reg 11 Sp) (Set_reg Pc (Val 9))
          (Set_reg Sp (Op + (Val 10) (Const 8))) (Get_reg 10 Sp)
          (Get_mem 9 Stack
           (Op >> (Op + (Val 8) (Op << (Const 0) (Const 3))) (Const 3)))
          (Get_reg 8 Sp)))
        (Get_reg 4 Extra_args)
        (Set_reg Sp (Op + (Val 3) (Op << (Op >>+ (Val 1) (Const 32)) (Const 3))))
        (Get_reg 3 Sp) (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ RESTART ____

    _0 = env;
    _1 = mem[((_0-8)>>3)];
    _2 = env;
    for _3=[(((_1>>10)-2)-1) downto 0] do
      _4 = mem[((_2+((_3+2)<<3))>>3)];
      _5 = sp;
      sp = (_5-8);
      _6 = sp;
      stack[(_6>>3)] = _4;
    done
    _7 = mem[((_2+8)>>3)];
    env = _7;
    _8 = extra_args;
    extra_args = (_8+((_1>>10)-2));

    (step
     ((id 9)
      (cmd
       ((Set_reg Extra_args
         (Op + (Val 8) (Op - (Op >> (Val 1) (Const 10)) (Const 2))))
        (Get_reg 8 Extra_args) (Set_reg Env (Val 7))
        (Get_mem 7 Mem
         (Op >> (Op + (Val 2) (Op << (Const 1) (Const 3))) (Const 3)))
        (Iter false 3
         (Op - (Op - (Op >> (Val 1) (Const 10)) (Const 2)) (Const 1)) (Const 0)
         ((Set_mem Stack
           (Op >> (Op + (Val 6) (Op << (Const 0) (Const 3))) (Const 3)) (Val 4))
          (Get_reg 6 Sp) (Set_reg Sp (Op - (Val 5) (Const 8))) (Get_reg 5 Sp)
          (Get_mem 4 Mem
           (Op >> (Op + (Val 2) (Op << (Op + (Val 3) (Const 2)) (Const 3)))
            (Const 3)))))
        (Get_reg 2 Env)
        (Get_mem 1 Mem
         (Op >> (Op - (Val 0) (Op << (Const 1) (Const 3))) (Const 3)))
        (Get_reg 0 Env)))))

    ____ GRAB ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = extra_args;
    if (_3>=+(_1>>+32)) then
      extra_args = (_3-(_1>>+32));
    else
      _4 = alloc_base;
      alloc_base = (_4+((((_3+1)+2)+1)<<3));
      mem[(_4>>3)] = (247|(((_3+1)+2)<<10));
      _5 = env;
      mem[(((_4+8)+8)>>3)] = _5;
      for _6=[0 to (_3+1)] do
        _7 = sp;
        _8 = stack[(_7>>3)];
        _9 = sp;
        sp = (_9+8);
        mem[(((_4+8)+((_6+2)<<3))>>3)] = _8;
      done
      _10 = pc;
      mem[((_4+8)>>3)] = (_10-12);
      _11 = sp;
      _12 = stack[(_11>>3)];
      _13 = sp;
      sp = (_13+8);
      pc = _12;
      _14 = sp;
      _15 = stack[(_14>>3)];
      _16 = sp;
      sp = (_16+8);
      env = _15;
      _17 = sp;
      _18 = stack[(_17>>3)];
      _19 = sp;
      sp = (_19+8);
      extra_args = (_18>>+1);
      accu = (_4+8);
    end

    (step
     ((id 20)
      (cmd
       ((Cond (Op >=+ (Val 3) (Op >>+ (Val 1) (Const 32)))
         ((Set_reg Extra_args (Op - (Val 3) (Op >>+ (Val 1) (Const 32)))))
         ((Set_reg Accu (Op + (Val 4) (Op << (Const 1) (Const 3))))
          (Set_reg Extra_args (Op >>+ (Val 18) (Const 1)))
          (Set_reg Sp (Op + (Val 19) (Const 8))) (Get_reg 19 Sp)
          (Get_mem 18 Stack
           (Op >> (Op + (Val 17) (Op << (Const 0) (Const 3))) (Const 3)))
          (Get_reg 17 Sp) (Set_reg Env (Val 15))
          (Set_reg Sp (Op + (Val 16) (Const 8))) (Get_reg 16 Sp)
          (Get_mem 15 Stack
           (Op >> (Op + (Val 14) (Op << (Const 0) (Const 3))) (Const 3)))
          (Get_reg 14 Sp) (Set_reg Pc (Val 12))
          (Set_reg Sp (Op + (Val 13) (Const 8))) (Get_reg 13 Sp)
          (Get_mem 12 Stack
           (Op >> (Op + (Val 11) (Op << (Const 0) (Const 3))) (Const 3)))
          (Get_reg 11 Sp)
          (Set_mem Mem
           (Op >> (Op + (Val 4) (Op << (Const 1) (Const 3))) (Const 3))
           (Op - (Val 10) (Op << (Const 3) (Const 2))))
          (Get_reg 10 Pc)
          (Iter true 6 (Const 0) (Op + (Val 3) (Const 1))
           ((Set_mem Mem
             (Op >>
              (Op + (Op + (Val 4) (Op << (Const 1) (Const 3)))
               (Op << (Op + (Val 6) (Const 2)) (Const 3)))
              (Const 3))
             (Val 8))
            (Set_reg Sp (Op + (Val 9) (Const 8))) (Get_reg 9 Sp)
            (Get_mem 8 Stack
             (Op >> (Op + (Val 7) (Op << (Const 0) (Const 3))) (Const 3)))
            (Get_reg 7 Sp)))
          (Set_mem Mem
           (Op >>
            (Op + (Op + (Val 4) (Op << (Const 1) (Const 3)))
             (Op << (Const 1) (Const 3)))
            (Const 3))
           (Val 5))
          (Get_reg 5 Env)
          (Set_mem Mem (Op >> (Val 4) (Const 3))
           (Op |
            (Op | (Op << (Op & (Const 247) (Const 255)) (Const 0))
             (Op << (Op & (Const 0) (Const 3)) (Const 8)))
            (Op << (Op + (Op + (Val 3) (Const 1)) (Const 2)) (Const 10))))
          (Set_reg Alloc_base
           (Op + (Val 4)
            (Op << (Op + (Op + (Op + (Val 3) (Const 1)) (Const 2)) (Const 1))
             (Const 3))))
          (Get_reg 4 Alloc_base)))
        (Get_reg 3 Extra_args) (Set_reg Pc (Op + (Val 2) (Const 4)))
        (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ CLOSURE ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    if ((_1>>+32)>+0) then
      _3 = accu;
      _4 = sp;
      sp = (_4-8);
      _5 = sp;
      stack[(_5>>3)] = _3;
    else
    end
    _6 = alloc_base;
    alloc_base = (_6+((((_1>>+32)+1)+1)<<3));
    mem[(_6>>3)] = (247|(((_1>>+32)+1)<<10));
    for _7=[0 to (_1>>+32)] do
      _8 = sp;
      _9 = stack[(_8>>3)];
      _10 = sp;
      sp = (_10+8);
      mem[(((_6+8)+((_7+1)<<3))>>3)] = _9;
    done
    _11 = pc;
    _12 = program[(_11>>3)];
    _13 = pc;
    pc = (_13+4);
    _14 = pc;
    mem[((_6+8)>>3)] = (_14+(((_12>>+32)-1)<<2));
    accu = (_6+8);

    (step
     ((id 15)
      (cmd
       ((Set_reg Accu (Op + (Val 6) (Op << (Const 1) (Const 3))))
        (Set_mem Mem (Op >> (Op + (Val 6) (Op << (Const 1) (Const 3))) (Const 3))
         (Op + (Val 14)
          (Op << (Op - (Op >>+ (Val 12) (Const 32)) (Const 1)) (Const 2))))
        (Get_reg 14 Pc) (Set_reg Pc (Op + (Val 13) (Const 4))) (Get_reg 13 Pc)
        (Get_mem 12 Program
         (Op >> (Op + (Val 11) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 11 Pc)
        (Iter true 7 (Const 0) (Op >>+ (Val 1) (Const 32))
         ((Set_mem Mem
           (Op >>
            (Op + (Op + (Val 6) (Op << (Const 1) (Const 3)))
             (Op << (Op + (Val 7) (Const 1)) (Const 3)))
            (Const 3))
           (Val 9))
          (Set_reg Sp (Op + (Val 10) (Const 8))) (Get_reg 10 Sp)
          (Get_mem 9 Stack
           (Op >> (Op + (Val 8) (Op << (Const 0) (Const 3))) (Const 3)))
          (Get_reg 8 Sp)))
        (Set_mem Mem (Op >> (Val 6) (Const 3))
         (Op |
          (Op | (Op << (Op & (Const 247) (Const 255)) (Const 0))
           (Op << (Op & (Const 0) (Const 3)) (Const 8)))
          (Op << (Op + (Op >>+ (Val 1) (Const 32)) (Const 1)) (Const 10))))
        (Set_reg Alloc_base
         (Op + (Val 6)
          (Op << (Op + (Op + (Op >>+ (Val 1) (Const 32)) (Const 1)) (Const 1))
           (Const 3))))
        (Get_reg 6 Alloc_base)
        (Cond (Op >+ (Op >>+ (Val 1) (Const 32)) (Const 0))
         ((Set_mem Stack
           (Op >> (Op + (Val 5) (Op << (Const 0) (Const 3))) (Const 3)) (Val 3))
          (Get_reg 5 Sp) (Set_reg Sp (Op - (Val 4) (Const 8))) (Get_reg 4 Sp)
          (Get_reg 3 Accu))
         ())
        (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ CLOSUREREC ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = pc;
    _4 = program[(_3>>3)];
    _5 = pc;
    pc = (_5+4);
    if ((_4>>+32)>+0) then
      _6 = accu;
      _7 = sp;
      sp = (_7-8);
      _8 = sp;
      stack[(_8>>3)] = _6;
    else
    end
    _9 = alloc_base;
    alloc_base = (_9+((((((_1>>+32)<<1)-1)+(_4>>+32))+1)<<3));
    mem[(_9>>3)] = (247|(((((_1>>+32)<<1)-1)+(_4>>+32))<<10));
    for _10=[0 to (_4>>+32)] do
      _11 = sp;
      _12 = stack[(_11>>3)];
      _13 = sp;
      sp = (_13+8);
      mem[(((_9+8)+(((((_1>>+32)<<1)-1)+_10)<<3))>>3)] = _12;
    done
    _14 = pc;
    _15 = pc;
    _16 = program[(_15>>3)];
    _17 = pc;
    pc = (_17+4);
    mem[((_9+8)>>3)] = (_14+((_16>>+32)<<2));
    _18 = sp;
    sp = (_18-8);
    _19 = sp;
    stack[(_19>>3)] = (_9+8);
    for _20=[1 to (_1>>+32)] do
      mem[(((_9+8)+(((_20<<1)-1)<<3))>>3)] = (249|((_20<<1)<<10));
      _21 = pc;
      _22 = program[(_21>>3)];
      _23 = pc;
      pc = (_23+4);
      mem[(((_9+8)+((_20<<1)<<3))>>3)] = (_14+((_22>>+32)<<2));
      _24 = sp;
      sp = (_24-8);
      _25 = sp;
      stack[(_25>>3)] = ((_9+8)+((_20<<1)<<3));
    done
    accu = (_9+8);

    (step
     ((id 26)
      (cmd
       ((Set_reg Accu (Op + (Val 9) (Op << (Const 1) (Const 3))))
        (Iter true 20 (Const 1) (Op >>+ (Val 1) (Const 32))
         ((Set_mem Stack
           (Op >> (Op + (Val 25) (Op << (Const 0) (Const 3))) (Const 3))
           (Op + (Op + (Val 9) (Op << (Const 1) (Const 3)))
            (Op << (Op << (Val 20) (Const 1)) (Const 3))))
          (Get_reg 25 Sp) (Set_reg Sp (Op - (Val 24) (Const 8))) (Get_reg 24 Sp)
          (Set_mem Mem
           (Op >>
            (Op + (Op + (Val 9) (Op << (Const 1) (Const 3)))
             (Op << (Op << (Val 20) (Const 1)) (Const 3)))
            (Const 3))
           (Op + (Val 14) (Op << (Op >>+ (Val 22) (Const 32)) (Const 2))))
          (Set_reg Pc (Op + (Val 23) (Const 4))) (Get_reg 23 Pc)
          (Get_mem 22 Program
           (Op >> (Op + (Val 21) (Op << (Const 0) (Const 2))) (Const 3)))
          (Get_reg 21 Pc)
          (Set_mem Mem
           (Op >>
            (Op + (Op + (Val 9) (Op << (Const 1) (Const 3)))
             (Op << (Op - (Op << (Val 20) (Const 1)) (Const 1)) (Const 3)))
            (Const 3))
           (Op |
            (Op | (Op << (Op & (Const 249) (Const 255)) (Const 0))
             (Op << (Op & (Const 0) (Const 3)) (Const 8)))
            (Op << (Op << (Val 20) (Const 1)) (Const 10))))))
        (Set_mem Stack
         (Op >> (Op + (Val 19) (Op << (Const 0) (Const 3))) (Const 3))
         (Op + (Val 9) (Op << (Const 1) (Const 3))))
        (Get_reg 19 Sp) (Set_reg Sp (Op - (Val 18) (Const 8))) (Get_reg 18 Sp)
        (Set_mem Mem
         (Op >>
          (Op + (Op + (Val 9) (Op << (Const 1) (Const 3)))
           (Op << (Const 0) (Const 3)))
          (Const 3))
         (Op + (Val 14) (Op << (Op >>+ (Val 16) (Const 32)) (Const 2))))
        (Set_reg Pc (Op + (Val 17) (Const 4))) (Get_reg 17 Pc)
        (Get_mem 16 Program
         (Op >> (Op + (Val 15) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 15 Pc) (Get_reg 14 Pc)
        (Iter true 10 (Const 0) (Op >>+ (Val 4) (Const 32))
         ((Set_mem Mem
           (Op >>
            (Op + (Op + (Val 9) (Op << (Const 1) (Const 3)))
             (Op <<
              (Op +
               (Op - (Op << (Op >>+ (Val 1) (Const 32)) (Const 1)) (Const 1))
               (Val 10))
              (Const 3)))
            (Const 3))
           (Val 12))
          (Set_reg Sp (Op + (Val 13) (Const 8))) (Get_reg 13 Sp)
          (Get_mem 12 Stack
           (Op >> (Op + (Val 11) (Op << (Const 0) (Const 3))) (Const 3)))
          (Get_reg 11 Sp)))
        (Set_mem Mem (Op >> (Val 9) (Const 3))
         (Op |
          (Op | (Op << (Op & (Const 247) (Const 255)) (Const 0))
           (Op << (Op & (Const 0) (Const 3)) (Const 8)))
          (Op <<
           (Op + (Op - (Op << (Op >>+ (Val 1) (Const 32)) (Const 1)) (Const 1))
            (Op >>+ (Val 4) (Const 32)))
           (Const 10))))
        (Set_reg Alloc_base
         (Op + (Val 9)
          (Op <<
           (Op +
            (Op + (Op - (Op << (Op >>+ (Val 1) (Const 32)) (Const 1)) (Const 1))
             (Op >>+ (Val 4) (Const 32)))
            (Const 1))
           (Const 3))))
        (Get_reg 9 Alloc_base)
        (Cond (Op >+ (Op >>+ (Val 4) (Const 32)) (Const 0))
         ((Set_mem Stack
           (Op >> (Op + (Val 8) (Op << (Const 0) (Const 3))) (Const 3)) (Val 6))
          (Get_reg 8 Sp) (Set_reg Sp (Op - (Val 7) (Const 8))) (Get_reg 7 Sp)
          (Get_reg 6 Accu))
         ())
        (Set_reg Pc (Op + (Val 5) (Const 4))) (Get_reg 5 Pc)
        (Get_mem 4 Program
         (Op >> (Op + (Val 3) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 3 Pc) (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ OFFSETCLOSUREM2 ____

    _0 = env;
    accu = (_0+-16);

    (step
     ((id 1)
      (cmd
       ((Set_reg Accu (Op + (Val 0) (Op << (Const -2) (Const 3))))
        (Get_reg 0 Env)))))

    ____ OFFSETCLOSURE0 ____

    _0 = env;
    accu = _0;

    (step
     ((id 1)
      (cmd
       ((Set_reg Accu (Op + (Val 0) (Op << (Const 0) (Const 3))))
        (Get_reg 0 Env)))))

    ____ OFFSETCLOSURE2 ____

    _0 = env;
    accu = (_0+16);

    (step
     ((id 1)
      (cmd
       ((Set_reg Accu (Op + (Val 0) (Op << (Const 2) (Const 3))))
        (Get_reg 0 Env)))))

    ____ OFFSETCLOSURE ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = env;
    accu = (_3+((_1>>+32)<<3));

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu
         (Op + (Val 3) (Op << (Op >>+ (Val 1) (Const 32)) (Const 3))))
        (Get_reg 3 Env) (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ PUSHOFFSETCLOSUREM2 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = env;
    accu = (_3+-16);

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu (Op + (Val 3) (Op << (Const -2) (Const 3))))
        (Get_reg 3 Env)
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ PUSHOFFSETCLOSURE0 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = env;
    accu = _3;

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu (Op + (Val 3) (Op << (Const 0) (Const 3)))) (Get_reg 3 Env)
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ PUSHOFFSETCLOSURE2 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = env;
    accu = (_3+16);

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu (Op + (Val 3) (Op << (Const 2) (Const 3)))) (Get_reg 3 Env)
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ PUSHOFFSETCLOSURE ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = accu;
    _4 = sp;
    sp = (_4-8);
    _5 = sp;
    stack[(_5>>3)] = _3;
    _6 = env;
    accu = (_6+((_1>>+32)<<3));

    (step
     ((id 7)
      (cmd
       ((Set_reg Accu
         (Op + (Val 6) (Op << (Op >>+ (Val 1) (Const 32)) (Const 3))))
        (Get_reg 6 Env)
        (Set_mem Stack
         (Op >> (Op + (Val 5) (Op << (Const 0) (Const 3))) (Const 3)) (Val 3))
        (Get_reg 5 Sp) (Set_reg Sp (Op - (Val 4) (Const 8))) (Get_reg 4 Sp)
        (Get_reg 3 Accu) (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ GETGLOBAL ____

    _0 = global_data;
    _1 = pc;
    _2 = program[(_1>>3)];
    _3 = pc;
    pc = (_3+4);
    _4 = mem[((_0+((_2>>+32)<<3))>>3)];
    accu = _4;

    (step
     ((id 5)
      (cmd
       ((Set_reg Accu (Val 4))
        (Get_mem 4 Mem
         (Op >> (Op + (Val 0) (Op << (Op >>+ (Val 2) (Const 32)) (Const 3)))
          (Const 3)))
        (Set_reg Pc (Op + (Val 3) (Const 4))) (Get_reg 3 Pc)
        (Get_mem 2 Program
         (Op >> (Op + (Val 1) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 1 Pc) (Get_reg 0 Global_data)))))

    ____ PUSHGETGLOBAL ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = global_data;
    _4 = pc;
    _5 = program[(_4>>3)];
    _6 = pc;
    pc = (_6+4);
    _7 = mem[((_3+((_5>>+32)<<3))>>3)];
    accu = _7;

    (step
     ((id 8)
      (cmd
       ((Set_reg Accu (Val 7))
        (Get_mem 7 Mem
         (Op >> (Op + (Val 3) (Op << (Op >>+ (Val 5) (Const 32)) (Const 3)))
          (Const 3)))
        (Set_reg Pc (Op + (Val 6) (Const 4))) (Get_reg 6 Pc)
        (Get_mem 5 Program
         (Op >> (Op + (Val 4) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 4 Pc) (Get_reg 3 Global_data)
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ GETGLOBALFIELD ____

    _0 = global_data;
    _1 = pc;
    _2 = program[(_1>>3)];
    _3 = pc;
    pc = (_3+4);
    _4 = mem[((_0+((_2>>+32)<<3))>>3)];
    _5 = pc;
    _6 = program[(_5>>3)];
    _7 = pc;
    pc = (_7+4);
    _8 = mem[((_4+((_6>>+32)<<3))>>3)];
    accu = _8;

    (step
     ((id 9)
      (cmd
       ((Set_reg Accu (Val 8))
        (Get_mem 8 Mem
         (Op >> (Op + (Val 4) (Op << (Op >>+ (Val 6) (Const 32)) (Const 3)))
          (Const 3)))
        (Set_reg Pc (Op + (Val 7) (Const 4))) (Get_reg 7 Pc)
        (Get_mem 6 Program
         (Op >> (Op + (Val 5) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 5 Pc)
        (Get_mem 4 Mem
         (Op >> (Op + (Val 0) (Op << (Op >>+ (Val 2) (Const 32)) (Const 3)))
          (Const 3)))
        (Set_reg Pc (Op + (Val 3) (Const 4))) (Get_reg 3 Pc)
        (Get_mem 2 Program
         (Op >> (Op + (Val 1) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 1 Pc) (Get_reg 0 Global_data)))))

    ____ PUSHGETGLOBALFIELD ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = global_data;
    _4 = pc;
    _5 = program[(_4>>3)];
    _6 = pc;
    pc = (_6+4);
    _7 = mem[((_3+((_5>>+32)<<3))>>3)];
    _8 = pc;
    _9 = program[(_8>>3)];
    _10 = pc;
    pc = (_10+4);
    _11 = mem[((_7+((_9>>+32)<<3))>>3)];
    accu = _11;

    (step
     ((id 12)
      (cmd
       ((Set_reg Accu (Val 11))
        (Get_mem 11 Mem
         (Op >> (Op + (Val 7) (Op << (Op >>+ (Val 9) (Const 32)) (Const 3)))
          (Const 3)))
        (Set_reg Pc (Op + (Val 10) (Const 4))) (Get_reg 10 Pc)
        (Get_mem 9 Program
         (Op >> (Op + (Val 8) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 8 Pc)
        (Get_mem 7 Mem
         (Op >> (Op + (Val 3) (Op << (Op >>+ (Val 5) (Const 32)) (Const 3)))
          (Const 3)))
        (Set_reg Pc (Op + (Val 6) (Const 4))) (Get_reg 6 Pc)
        (Get_mem 5 Program
         (Op >> (Op + (Val 4) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 4 Pc) (Get_reg 3 Global_data)
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ SETGLOBAL ____

    _0 = global_data;
    _1 = pc;
    _2 = program[(_1>>3)];
    _3 = pc;
    pc = (_3+4);
    _4 = accu;
    mem[((_0+((_2>>+32)<<3))>>3)] = _4;
    accu = 1;

    (step
     ((id 5)
      (cmd
       ((Set_reg Accu (Const 1))
        (Set_mem Mem
         (Op >> (Op + (Val 0) (Op << (Op >>+ (Val 2) (Const 32)) (Const 3)))
          (Const 3))
         (Val 4))
        (Get_reg 4 Accu) (Set_reg Pc (Op + (Val 3) (Const 4))) (Get_reg 3 Pc)
        (Get_mem 2 Program
         (Op >> (Op + (Val 1) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 1 Pc) (Get_reg 0 Global_data)))))

    ____ ATOM0 ____

    _0 = atom_table;
    accu = _0;

    (step
     ((id 1)
      (cmd
       ((Set_reg Accu (Op + (Val 0) (Op << (Const 0) (Const 3))))
        (Get_reg 0 Atom_table)))))

    ____ ATOM ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = atom_table;
    accu = (_3+((_1>>+32)<<3));

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu
         (Op + (Val 3) (Op << (Op >>+ (Val 1) (Const 32)) (Const 3))))
        (Get_reg 3 Atom_table) (Set_reg Pc (Op + (Val 2) (Const 4)))
        (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ PUSHATOM0 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = atom_table;
    accu = _3;

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu (Op + (Val 3) (Op << (Const 0) (Const 3))))
        (Get_reg 3 Atom_table)
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ PUSHATOM ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = pc;
    _4 = program[(_3>>3)];
    _5 = pc;
    pc = (_5+4);
    _6 = atom_table;
    accu = (_6+((_4>>+32)<<3));

    (step
     ((id 7)
      (cmd
       ((Set_reg Accu
         (Op + (Val 6) (Op << (Op >>+ (Val 4) (Const 32)) (Const 3))))
        (Get_reg 6 Atom_table) (Set_reg Pc (Op + (Val 5) (Const 4)))
        (Get_reg 5 Pc)
        (Get_mem 4 Program
         (Op >> (Op + (Val 3) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 3 Pc)
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ MAKEBLOCK ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = pc;
    _4 = program[(_3>>3)];
    _5 = pc;
    pc = (_5+4);
    _6 = alloc_base;
    alloc_base = (_6+(((_1>>+32)+1)<<3));
    mem[(_6>>3)] = (0|((_1>>+32)<<10));
    _7 = accu;
    mem[((_6+8)>>3)] = _7;
    for _8=[1 to (_1>>+32)] do
      _9 = sp;
      _10 = stack[(_9>>3)];
      _11 = sp;
      sp = (_11+8);
      mem[(((_6+8)+(_8<<3))>>3)] = _10;
    done
    accu = (_6+8);

    (step
     ((id 12)
      (cmd
       ((Set_reg Accu (Op + (Val 6) (Op << (Const 1) (Const 3))))
        (Iter true 8 (Const 1) (Op >>+ (Val 1) (Const 32))
         ((Set_mem Mem
           (Op >>
            (Op + (Op + (Val 6) (Op << (Const 1) (Const 3)))
             (Op << (Val 8) (Const 3)))
            (Const 3))
           (Val 10))
          (Set_reg Sp (Op + (Val 11) (Const 8))) (Get_reg 11 Sp)
          (Get_mem 10 Stack
           (Op >> (Op + (Val 9) (Op << (Const 0) (Const 3))) (Const 3)))
          (Get_reg 9 Sp)))
        (Set_mem Mem
         (Op >>
          (Op + (Op + (Val 6) (Op << (Const 1) (Const 3)))
           (Op << (Const 0) (Const 3)))
          (Const 3))
         (Val 7))
        (Get_reg 7 Accu)
        (Set_mem Mem (Op >> (Val 6) (Const 3))
         (Op |
          (Op | (Op << (Op & (Op >>+ (Val 4) (Const 32)) (Const 255)) (Const 0))
           (Op << (Op & (Const 0) (Const 3)) (Const 8)))
          (Op << (Op >>+ (Val 1) (Const 32)) (Const 10))))
        (Set_reg Alloc_base
         (Op + (Val 6)
          (Op << (Op + (Op >>+ (Val 1) (Const 32)) (Const 1)) (Const 3))))
        (Get_reg 6 Alloc_base) (Set_reg Pc (Op + (Val 5) (Const 4)))
        (Get_reg 5 Pc)
        (Get_mem 4 Program
         (Op >> (Op + (Val 3) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 3 Pc) (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ MAKEBLOCK1 ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = alloc_base;
    alloc_base = (_3+16);
    mem[(_3>>3)] = 1024;
    _4 = accu;
    mem[((_3+8)>>3)] = _4;
    for _5=[1 to 1] do
      _6 = sp;
      _7 = stack[(_6>>3)];
      _8 = sp;
      sp = (_8+8);
      mem[(((_3+8)+(_5<<3))>>3)] = _7;
    done
    accu = (_3+8);

    (step
     ((id 9)
      (cmd
       ((Set_reg Accu (Op + (Val 3) (Op << (Const 1) (Const 3))))
        (Iter true 5 (Const 1) (Const 1)
         ((Set_mem Mem
           (Op >>
            (Op + (Op + (Val 3) (Op << (Const 1) (Const 3)))
             (Op << (Val 5) (Const 3)))
            (Const 3))
           (Val 7))
          (Set_reg Sp (Op + (Val 8) (Const 8))) (Get_reg 8 Sp)
          (Get_mem 7 Stack
           (Op >> (Op + (Val 6) (Op << (Const 0) (Const 3))) (Const 3)))
          (Get_reg 6 Sp)))
        (Set_mem Mem
         (Op >>
          (Op + (Op + (Val 3) (Op << (Const 1) (Const 3)))
           (Op << (Const 0) (Const 3)))
          (Const 3))
         (Val 4))
        (Get_reg 4 Accu)
        (Set_mem Mem (Op >> (Val 3) (Const 3))
         (Op |
          (Op | (Op << (Op & (Op >>+ (Val 1) (Const 32)) (Const 255)) (Const 0))
           (Op << (Op & (Const 0) (Const 3)) (Const 8)))
          (Op << (Const 1) (Const 10))))
        (Set_reg Alloc_base
         (Op + (Val 3) (Op << (Op + (Const 1) (Const 1)) (Const 3))))
        (Get_reg 3 Alloc_base) (Set_reg Pc (Op + (Val 2) (Const 4)))
        (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ MAKEBLOCK2 ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = alloc_base;
    alloc_base = (_3+24);
    mem[(_3>>3)] = 2048;
    _4 = accu;
    mem[((_3+8)>>3)] = _4;
    for _5=[1 to 2] do
      _6 = sp;
      _7 = stack[(_6>>3)];
      _8 = sp;
      sp = (_8+8);
      mem[(((_3+8)+(_5<<3))>>3)] = _7;
    done
    accu = (_3+8);

    (step
     ((id 9)
      (cmd
       ((Set_reg Accu (Op + (Val 3) (Op << (Const 1) (Const 3))))
        (Iter true 5 (Const 1) (Const 2)
         ((Set_mem Mem
           (Op >>
            (Op + (Op + (Val 3) (Op << (Const 1) (Const 3)))
             (Op << (Val 5) (Const 3)))
            (Const 3))
           (Val 7))
          (Set_reg Sp (Op + (Val 8) (Const 8))) (Get_reg 8 Sp)
          (Get_mem 7 Stack
           (Op >> (Op + (Val 6) (Op << (Const 0) (Const 3))) (Const 3)))
          (Get_reg 6 Sp)))
        (Set_mem Mem
         (Op >>
          (Op + (Op + (Val 3) (Op << (Const 1) (Const 3)))
           (Op << (Const 0) (Const 3)))
          (Const 3))
         (Val 4))
        (Get_reg 4 Accu)
        (Set_mem Mem (Op >> (Val 3) (Const 3))
         (Op |
          (Op | (Op << (Op & (Op >>+ (Val 1) (Const 32)) (Const 255)) (Const 0))
           (Op << (Op & (Const 0) (Const 3)) (Const 8)))
          (Op << (Const 2) (Const 10))))
        (Set_reg Alloc_base
         (Op + (Val 3) (Op << (Op + (Const 2) (Const 1)) (Const 3))))
        (Get_reg 3 Alloc_base) (Set_reg Pc (Op + (Val 2) (Const 4)))
        (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ MAKEBLOCK3 ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = alloc_base;
    alloc_base = (_3+32);
    mem[(_3>>3)] = 3072;
    _4 = accu;
    mem[((_3+8)>>3)] = _4;
    for _5=[1 to 3] do
      _6 = sp;
      _7 = stack[(_6>>3)];
      _8 = sp;
      sp = (_8+8);
      mem[(((_3+8)+(_5<<3))>>3)] = _7;
    done
    accu = (_3+8);

    (step
     ((id 9)
      (cmd
       ((Set_reg Accu (Op + (Val 3) (Op << (Const 1) (Const 3))))
        (Iter true 5 (Const 1) (Const 3)
         ((Set_mem Mem
           (Op >>
            (Op + (Op + (Val 3) (Op << (Const 1) (Const 3)))
             (Op << (Val 5) (Const 3)))
            (Const 3))
           (Val 7))
          (Set_reg Sp (Op + (Val 8) (Const 8))) (Get_reg 8 Sp)
          (Get_mem 7 Stack
           (Op >> (Op + (Val 6) (Op << (Const 0) (Const 3))) (Const 3)))
          (Get_reg 6 Sp)))
        (Set_mem Mem
         (Op >>
          (Op + (Op + (Val 3) (Op << (Const 1) (Const 3)))
           (Op << (Const 0) (Const 3)))
          (Const 3))
         (Val 4))
        (Get_reg 4 Accu)
        (Set_mem Mem (Op >> (Val 3) (Const 3))
         (Op |
          (Op | (Op << (Op & (Op >>+ (Val 1) (Const 32)) (Const 255)) (Const 0))
           (Op << (Op & (Const 0) (Const 3)) (Const 8)))
          (Op << (Const 3) (Const 10))))
        (Set_reg Alloc_base
         (Op + (Val 3) (Op << (Op + (Const 3) (Const 1)) (Const 3))))
        (Get_reg 3 Alloc_base) (Set_reg Pc (Op + (Val 2) (Const 4)))
        (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ MAKEFLOATBLOCK ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = alloc_base;
    alloc_base = (_3+(((_1>>+32)+1)<<3));
    mem[(_3>>3)] = (254|((_1>>+32)<<10));
    _4 = accu;
    mem[((_3+8)>>3)] = _4;
    for _5=[1 to (_1>>+32)] do
      _6 = sp;
      _7 = stack[(_6>>3)];
      _8 = sp;
      sp = (_8+8);
      mem[(((_3+8)+(_5<<3))>>3)] = _7;
    done
    accu = (_3+8);

    (step
     ((id 9)
      (cmd
       ((Set_reg Accu (Op + (Val 3) (Op << (Const 1) (Const 3))))
        (Iter true 5 (Const 1) (Op >>+ (Val 1) (Const 32))
         ((Set_mem Mem
           (Op >>
            (Op + (Op + (Val 3) (Op << (Const 1) (Const 3)))
             (Op << (Val 5) (Const 3)))
            (Const 3))
           (Val 7))
          (Set_reg Sp (Op + (Val 8) (Const 8))) (Get_reg 8 Sp)
          (Get_mem 7 Stack
           (Op >> (Op + (Val 6) (Op << (Const 0) (Const 3))) (Const 3)))
          (Get_reg 6 Sp)))
        (Set_mem Mem
         (Op >>
          (Op + (Op + (Val 3) (Op << (Const 1) (Const 3)))
           (Op << (Const 0) (Const 3)))
          (Const 3))
         (Val 4))
        (Get_reg 4 Accu)
        (Set_mem Mem (Op >> (Val 3) (Const 3))
         (Op |
          (Op | (Op << (Op & (Const 254) (Const 255)) (Const 0))
           (Op << (Op & (Const 0) (Const 3)) (Const 8)))
          (Op << (Op >>+ (Val 1) (Const 32)) (Const 10))))
        (Set_reg Alloc_base
         (Op + (Val 3)
          (Op << (Op + (Op >>+ (Val 1) (Const 32)) (Const 1)) (Const 3))))
        (Get_reg 3 Alloc_base) (Set_reg Pc (Op + (Val 2) (Const 4)))
        (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ GETFIELD0 ____

    _0 = accu;
    _1 = mem[(_0>>3)];
    accu = _1;

    (step
     ((id 2)
      (cmd
       ((Set_reg Accu (Val 1))
        (Get_mem 1 Mem
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Accu)))))

    ____ GETFIELD1 ____

    _0 = accu;
    _1 = mem[((_0+8)>>3)];
    accu = _1;

    (step
     ((id 2)
      (cmd
       ((Set_reg Accu (Val 1))
        (Get_mem 1 Mem
         (Op >> (Op + (Val 0) (Op << (Const 1) (Const 3))) (Const 3)))
        (Get_reg 0 Accu)))))

    ____ GETFIELD2 ____

    _0 = accu;
    _1 = mem[((_0+16)>>3)];
    accu = _1;

    (step
     ((id 2)
      (cmd
       ((Set_reg Accu (Val 1))
        (Get_mem 1 Mem
         (Op >> (Op + (Val 0) (Op << (Const 2) (Const 3))) (Const 3)))
        (Get_reg 0 Accu)))))

    ____ GETFIELD3 ____

    _0 = accu;
    _1 = mem[((_0+24)>>3)];
    accu = _1;

    (step
     ((id 2)
      (cmd
       ((Set_reg Accu (Val 1))
        (Get_mem 1 Mem
         (Op >> (Op + (Val 0) (Op << (Const 3) (Const 3))) (Const 3)))
        (Get_reg 0 Accu)))))

    ____ GETFIELD ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = accu;
    _4 = mem[((_3+((_1>>+32)<<3))>>3)];
    accu = _4;

    (step
     ((id 5)
      (cmd
       ((Set_reg Accu (Val 4))
        (Get_mem 4 Mem
         (Op >> (Op + (Val 3) (Op << (Op >>+ (Val 1) (Const 32)) (Const 3)))
          (Const 3)))
        (Get_reg 3 Accu) (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ GETFLOATFIELD ____

    _0 = accu;
    _1 = pc;
    _2 = program[(_1>>3)];
    _3 = pc;
    pc = (_3+4);
    _4 = mem[((_0+((_2>>+32)<<3))>>3)];
    _5 = alloc_base;
    alloc_base = (_5+16);
    mem[(_5>>3)] = 1277;
    mem[((_5+8)>>3)] = _4;
    accu = (_5+8);

    (step
     ((id 6)
      (cmd
       ((Set_reg Accu (Op + (Val 5) (Op << (Const 1) (Const 3))))
        (Set_mem Mem
         (Op >>
          (Op + (Op + (Val 5) (Op << (Const 1) (Const 3)))
           (Op << (Const 0) (Const 3)))
          (Const 3))
         (Val 4))
        (Set_mem Mem (Op >> (Val 5) (Const 3))
         (Op |
          (Op | (Op << (Op & (Const 253) (Const 255)) (Const 0))
           (Op << (Op & (Const 0) (Const 3)) (Const 8)))
          (Op << (Const 1) (Const 10))))
        (Set_reg Alloc_base
         (Op + (Val 5) (Op << (Op + (Const 1) (Const 1)) (Const 3))))
        (Get_reg 5 Alloc_base)
        (Get_mem 4 Mem
         (Op >> (Op + (Val 0) (Op << (Op >>+ (Val 2) (Const 32)) (Const 3)))
          (Const 3)))
        (Set_reg Pc (Op + (Val 3) (Const 4))) (Get_reg 3 Pc)
        (Get_mem 2 Program
         (Op >> (Op + (Val 1) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 1 Pc) (Get_reg 0 Accu)))))

    ____ SETFIELD0 ____

    _0 = accu;
    _1 = sp;
    _2 = stack[(_1>>3)];
    _3 = sp;
    sp = (_3+8);
    mem[(_0>>3)] = _2;
    accu = 1;

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu (Const 1))
        (Set_mem Mem (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3))
         (Val 2))
        (Set_reg Sp (Op + (Val 3) (Const 8))) (Get_reg 3 Sp)
        (Get_mem 2 Stack
         (Op >> (Op + (Val 1) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 1 Sp) (Get_reg 0 Accu)))))

    ____ SETFIELD1 ____

    _0 = accu;
    _1 = sp;
    _2 = stack[(_1>>3)];
    _3 = sp;
    sp = (_3+8);
    mem[((_0+8)>>3)] = _2;
    accu = 1;

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu (Const 1))
        (Set_mem Mem (Op >> (Op + (Val 0) (Op << (Const 1) (Const 3))) (Const 3))
         (Val 2))
        (Set_reg Sp (Op + (Val 3) (Const 8))) (Get_reg 3 Sp)
        (Get_mem 2 Stack
         (Op >> (Op + (Val 1) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 1 Sp) (Get_reg 0 Accu)))))

    ____ SETFIELD2 ____

    _0 = accu;
    _1 = sp;
    _2 = stack[(_1>>3)];
    _3 = sp;
    sp = (_3+8);
    mem[((_0+16)>>3)] = _2;
    accu = 1;

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu (Const 1))
        (Set_mem Mem (Op >> (Op + (Val 0) (Op << (Const 2) (Const 3))) (Const 3))
         (Val 2))
        (Set_reg Sp (Op + (Val 3) (Const 8))) (Get_reg 3 Sp)
        (Get_mem 2 Stack
         (Op >> (Op + (Val 1) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 1 Sp) (Get_reg 0 Accu)))))

    ____ SETFIELD3 ____

    _0 = accu;
    _1 = sp;
    _2 = stack[(_1>>3)];
    _3 = sp;
    sp = (_3+8);
    mem[((_0+24)>>3)] = _2;
    accu = 1;

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu (Const 1))
        (Set_mem Mem (Op >> (Op + (Val 0) (Op << (Const 3) (Const 3))) (Const 3))
         (Val 2))
        (Set_reg Sp (Op + (Val 3) (Const 8))) (Get_reg 3 Sp)
        (Get_mem 2 Stack
         (Op >> (Op + (Val 1) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 1 Sp) (Get_reg 0 Accu)))))

    ____ SETFIELD ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = accu;
    _4 = sp;
    _5 = stack[(_4>>3)];
    _6 = sp;
    sp = (_6+8);
    mem[((_3+((_1>>+32)<<3))>>3)] = _5;
    accu = 1;

    (step
     ((id 7)
      (cmd
       ((Set_reg Accu (Const 1))
        (Set_mem Mem
         (Op >> (Op + (Val 3) (Op << (Op >>+ (Val 1) (Const 32)) (Const 3)))
          (Const 3))
         (Val 5))
        (Set_reg Sp (Op + (Val 6) (Const 8))) (Get_reg 6 Sp)
        (Get_mem 5 Stack
         (Op >> (Op + (Val 4) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 4 Sp) (Get_reg 3 Accu) (Set_reg Pc (Op + (Val 2) (Const 4)))
        (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ SETFLOATFIELD ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = accu;
    _4 = sp;
    _5 = stack[(_4>>3)];
    _6 = sp;
    sp = (_6+8);
    mem[((_3+((_1>>+32)<<3))>>3)] = _5;
    accu = 1;

    (step
     ((id 7)
      (cmd
       ((Set_reg Accu (Const 1))
        (Set_mem Mem
         (Op >> (Op + (Val 3) (Op << (Op >>+ (Val 1) (Const 32)) (Const 3)))
          (Const 3))
         (Val 5))
        (Set_reg Sp (Op + (Val 6) (Const 8))) (Get_reg 6 Sp)
        (Get_mem 5 Stack
         (Op >> (Op + (Val 4) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 4 Sp) (Get_reg 3 Accu) (Set_reg Pc (Op + (Val 2) (Const 4)))
        (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ VECTLENGTH ____

    _0 = accu;
    _1 = mem[((_0-8)>>3)];
    accu = (((_1>>10)<<1)|1);

    (step
     ((id 2)
      (cmd
       ((Set_reg Accu
         (Op | (Op << (Op >> (Val 1) (Const 10)) (Const 1)) (Const 1)))
        (Get_mem 1 Mem
         (Op >> (Op - (Val 0) (Op << (Const 1) (Const 3))) (Const 3)))
        (Get_reg 0 Accu)))))

    ____ GETVECTITEM ____

    _0 = accu;
    _1 = sp;
    _2 = stack[(_1>>3)];
    _3 = sp;
    sp = (_3+8);
    _4 = mem[((_0+((_2>>+1)<<3))>>3)];
    accu = _4;

    (step
     ((id 5)
      (cmd
       ((Set_reg Accu (Val 4))
        (Get_mem 4 Mem
         (Op >> (Op + (Val 0) (Op << (Op >>+ (Val 2) (Const 1)) (Const 3)))
          (Const 3)))
        (Set_reg Sp (Op + (Val 3) (Const 8))) (Get_reg 3 Sp)
        (Get_mem 2 Stack
         (Op >> (Op + (Val 1) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 1 Sp) (Get_reg 0 Accu)))))

    ____ SETVECTITEM ____

    _0 = accu;
    _1 = sp;
    _2 = stack[(_1>>3)];
    _3 = sp;
    sp = (_3+8);
    _4 = sp;
    _5 = stack[(_4>>3)];
    _6 = sp;
    sp = (_6+8);
    mem[((_0+((_2>>+1)<<3))>>3)] = _5;
    accu = 1;

    (step
     ((id 7)
      (cmd
       ((Set_reg Accu (Const 1))
        (Set_mem Mem
         (Op >> (Op + (Val 0) (Op << (Op >>+ (Val 2) (Const 1)) (Const 3)))
          (Const 3))
         (Val 5))
        (Set_reg Sp (Op + (Val 6) (Const 8))) (Get_reg 6 Sp)
        (Get_mem 5 Stack
         (Op >> (Op + (Val 4) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 4 Sp) (Set_reg Sp (Op + (Val 3) (Const 8))) (Get_reg 3 Sp)
        (Get_mem 2 Stack
         (Op >> (Op + (Val 1) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 1 Sp) (Get_reg 0 Accu)))))

    ____ GETSTRINGCHAR ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    _4 = mem[((_3+((_1>>+1)&-8))>>3)];
    accu = ((((_4>>(((_1>>+1)&7)<<3))&255)<<1)|1);

    (step
     ((id 5)
      (cmd
       ((Set_reg Accu
         (Op |
          (Op <<
           (Op &
            (Op >> (Val 4)
             (Op << (Op & (Op >>+ (Val 1) (Const 1)) (Const 7)) (Const 3)))
            (Const 255))
           (Const 1))
          (Const 1)))
        (Get_mem 4 Mem
         (Op >>
          (Op + (Val 3)
           (Op & (Op >>+ (Val 1) (Const 1)) (Op ~ (Const 7) (Const 7))))
          (Const 3)))
        (Get_reg 3 Accu) (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ SETSTRINGCHAR ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = sp;
    _4 = stack[(_3>>3)];
    _5 = sp;
    sp = (_5+8);
    _6 = accu;
    _7 = mem[((_6+((_1>>+1)&-8))>>3)];
    mem[((_6+((_1>>+1)&-8))>>3)] = ((_7&((255<<(((_1>>+1)&7)<<3))~(255<<(((_1>>+1)&7)<<3))))|((_4>>+1)<<(((_1>>+1)&7)<<3)));
    accu = 1;

    (step
     ((id 8)
      (cmd
       ((Set_reg Accu (Const 1))
        (Set_mem Mem
         (Op >>
          (Op + (Val 6)
           (Op & (Op >>+ (Val 1) (Const 1)) (Op ~ (Const 7) (Const 7))))
          (Const 3))
         (Op |
          (Op & (Val 7)
           (Op ~
            (Op << (Const 255)
             (Op << (Op & (Op >>+ (Val 1) (Const 1)) (Const 7)) (Const 3)))
            (Op << (Const 255)
             (Op << (Op & (Op >>+ (Val 1) (Const 1)) (Const 7)) (Const 3)))))
          (Op << (Op >>+ (Val 4) (Const 1))
           (Op << (Op & (Op >>+ (Val 1) (Const 1)) (Const 7)) (Const 3)))))
        (Get_mem 7 Mem
         (Op >>
          (Op + (Val 6)
           (Op & (Op >>+ (Val 1) (Const 1)) (Op ~ (Const 7) (Const 7))))
          (Const 3)))
        (Get_reg 6 Accu) (Set_reg Sp (Op + (Val 5) (Const 8))) (Get_reg 5 Sp)
        (Get_mem 4 Stack
         (Op >> (Op + (Val 3) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 3 Sp) (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ BRANCH ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+((_1>>+32)<<2));

    (step
     ((id 3)
      (cmd
       ((Set_reg Pc (Op + (Val 2) (Op << (Op >>+ (Val 1) (Const 32)) (Const 2))))
        (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ BRANCHIF ____

    _0 = accu;
    if (_0<>1) then
      _1 = pc;
      _2 = program[(_1>>3)];
      _3 = pc;
      pc = (_3+((_2>>+32)<<2));
    else
      _4 = pc;
      pc = (_4+4);
    end

    (step
     ((id 5)
      (cmd
       ((Cond (Op <> (Val 0) (Const 1))
         ((Set_reg Pc
           (Op + (Val 3) (Op << (Op >>+ (Val 2) (Const 32)) (Const 2))))
          (Get_reg 3 Pc)
          (Get_mem 2 Program
           (Op >> (Op + (Val 1) (Op << (Const 0) (Const 2))) (Const 3)))
          (Get_reg 1 Pc))
         ((Set_reg Pc (Op + (Val 4) (Const 4))) (Get_reg 4 Pc)))
        (Get_reg 0 Accu)))))

    ____ BRANCHIFNOT ____

    _0 = accu;
    if (_0==1) then
      _1 = pc;
      _2 = program[(_1>>3)];
      _3 = pc;
      pc = (_3+((_2>>+32)<<2));
    else
      _4 = pc;
      pc = (_4+4);
    end

    (step
     ((id 5)
      (cmd
       ((Cond (Op == (Val 0) (Const 1))
         ((Set_reg Pc
           (Op + (Val 3) (Op << (Op >>+ (Val 2) (Const 32)) (Const 2))))
          (Get_reg 3 Pc)
          (Get_mem 2 Program
           (Op >> (Op + (Val 1) (Op << (Const 0) (Const 2))) (Const 3)))
          (Get_reg 1 Pc))
         ((Set_reg Pc (Op + (Val 4) (Const 4))) (Get_reg 4 Pc)))
        (Get_reg 0 Accu)))))

    ____ SWITCH ____

    _0 = accu;
    if ((_0~_0)&1) then
      _1 = pc;
      _2 = program[(_1>>3)];
      _3 = pc;
      pc = (_3+4);
      _4 = mem[((_0-8)>>3)];
      _5 = pc;
      _6 = program[((_5+((((_2>>+32)&65535)+(_4&255))<<2))>>3)];
      _7 = pc;
      pc = (_7+((_6>>+32)<<2));
    else
      _8 = pc;
      pc = (_8+4);
      _9 = pc;
      _10 = program[((_9+((_0>>+1)<<2))>>3)];
      _11 = pc;
      pc = (_11+((_10>>+32)<<2));
    end

    (step
     ((id 12)
      (cmd
       ((Cond (Op & (Op ~ (Val 0) (Val 0)) (Const 1))
         ((Set_reg Pc
           (Op + (Val 7) (Op << (Op >>+ (Val 6) (Const 32)) (Const 2))))
          (Get_reg 7 Pc)
          (Get_mem 6 Program
           (Op >>
            (Op + (Val 5)
             (Op <<
              (Op + (Op & (Op >>+ (Val 2) (Const 32)) (Const 65535))
               (Op & (Val 4) (Const 255)))
              (Const 2)))
            (Const 3)))
          (Get_reg 5 Pc)
          (Get_mem 4 Mem
           (Op >> (Op - (Val 0) (Op << (Const 1) (Const 3))) (Const 3)))
          (Set_reg Pc (Op + (Val 3) (Const 4))) (Get_reg 3 Pc)
          (Get_mem 2 Program
           (Op >> (Op + (Val 1) (Op << (Const 0) (Const 2))) (Const 3)))
          (Get_reg 1 Pc))
         ((Set_reg Pc
           (Op + (Val 11) (Op << (Op >>+ (Val 10) (Const 32)) (Const 2))))
          (Get_reg 11 Pc)
          (Get_mem 10 Program
           (Op >> (Op + (Val 9) (Op << (Op >>+ (Val 0) (Const 1)) (Const 2)))
            (Const 3)))
          (Get_reg 9 Pc) (Set_reg Pc (Op + (Val 8) (Const 4))) (Get_reg 8 Pc)))
        (Get_reg 0 Accu)))))

    ____ BOOLNOT ____

    _0 = accu;
    accu = (4-_0);

    (step
     ((id 1)
      (cmd
       ((Set_reg Accu (Op - (Op + (Const 1) (Const 3)) (Val 0)))
        (Get_reg 0 Accu)))))

    ____ PUSHTRAP ____

    _0 = extra_args;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = ((_0<<1)|1);
    _3 = env;
    _4 = sp;
    sp = (_4-8);
    _5 = sp;
    stack[(_5>>3)] = _3;
    _6 = trapsp;
    _7 = sp;
    sp = (_7-8);
    _8 = sp;
    stack[(_8>>3)] = _6;
    _9 = pc;
    _10 = program[(_9>>3)];
    _11 = pc;
    pc = (_11+4);
    _12 = pc;
    _13 = sp;
    sp = (_13-8);
    _14 = sp;
    stack[(_14>>3)] = (_12+(((_10>>+32)-1)<<2));
    _15 = sp;
    trapsp = _15;

    (step
     ((id 16)
      (cmd
       ((Set_reg Trapsp (Val 15)) (Get_reg 15 Sp)
        (Set_mem Stack
         (Op >> (Op + (Val 14) (Op << (Const 0) (Const 3))) (Const 3))
         (Op + (Val 12)
          (Op << (Op - (Op >>+ (Val 10) (Const 32)) (Const 1)) (Const 2))))
        (Get_reg 14 Sp) (Set_reg Sp (Op - (Val 13) (Const 8))) (Get_reg 13 Sp)
        (Get_reg 12 Pc) (Set_reg Pc (Op + (Val 11) (Const 4))) (Get_reg 11 Pc)
        (Get_mem 10 Program
         (Op >> (Op + (Val 9) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 9 Pc)
        (Set_mem Stack
         (Op >> (Op + (Val 8) (Op << (Const 0) (Const 3))) (Const 3)) (Val 6))
        (Get_reg 8 Sp) (Set_reg Sp (Op - (Val 7) (Const 8))) (Get_reg 7 Sp)
        (Get_reg 6 Trapsp)
        (Set_mem Stack
         (Op >> (Op + (Val 5) (Op << (Const 0) (Const 3))) (Const 3)) (Val 3))
        (Get_reg 5 Sp) (Set_reg Sp (Op - (Val 4) (Const 8))) (Get_reg 4 Sp)
        (Get_reg 3 Env)
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3))
         (Op | (Op << (Val 0) (Const 1)) (Const 1)))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Extra_args)))))

    ____ POPTRAP ____

    if 0 then
    else
      _0 = sp;
      _1 = stack[((_0+8)>>3)];
      trapsp = _1;
      _2 = sp;
      sp = (_2+32);
    end

    (step
     ((id 3)
      (cmd
       ((Cond (Const 0) ()
         ((Set_reg Sp (Op + (Val 2) (Op << (Const 4) (Const 3)))) (Get_reg 2 Sp)
          (Set_reg Trapsp (Val 1))
          (Get_mem 1 Stack
           (Op >> (Op + (Val 0) (Op << (Const 1) (Const 3))) (Const 3)))
          (Get_reg 0 Sp)))))))

    ____ RAISE ____

    NOT IMPLEMENTED
    ____ CHECK_SIGNALS ____

    if 0 then
    else
    end

    (step ((id 0) (cmd ((Cond (Const 0) () ())))))

    ____ C_CALL1 ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);

    ((c_call <opaque>)
     ((id 3)
      (cmd
       ((Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ C_CALL2 ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);

    ((c_call <opaque>)
     ((id 3)
      (cmd
       ((Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ C_CALL3 ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);

    ((c_call <opaque>)
     ((id 3)
      (cmd
       ((Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ C_CALL4 ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);

    ((c_call <opaque>)
     ((id 3)
      (cmd
       ((Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ C_CALL5 ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);

    ((c_call <opaque>)
     ((id 3)
      (cmd
       ((Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ C_CALLN ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);

    ((c_call <opaque>)
     ((id 3)
      (cmd
       ((Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ CONST0 ____

    accu = 1;

    (step
     ((id 0) (cmd ((Set_reg Accu (Op | (Op << (Const 0) (Const 1)) (Const 1)))))))

    ____ CONST1 ____

    accu = 3;

    (step
     ((id 0) (cmd ((Set_reg Accu (Op | (Op << (Const 1) (Const 1)) (Const 1)))))))

    ____ CONST2 ____

    accu = 5;

    (step
     ((id 0) (cmd ((Set_reg Accu (Op | (Op << (Const 2) (Const 1)) (Const 1)))))))

    ____ CONST3 ____

    accu = 7;

    (step
     ((id 0) (cmd ((Set_reg Accu (Op | (Op << (Const 3) (Const 1)) (Const 1)))))))

    ____ CONSTINT ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    accu = (((_1>>+32)<<1)|1);

    (step
     ((id 3)
      (cmd
       ((Set_reg Accu
         (Op | (Op << (Op >>+ (Val 1) (Const 32)) (Const 1)) (Const 1)))
        (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ PUSHCONST0 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    accu = 1;

    (step
     ((id 3)
      (cmd
       ((Set_reg Accu (Op | (Op << (Const 0) (Const 1)) (Const 1)))
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ PUSHCONST1 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    accu = 3;

    (step
     ((id 3)
      (cmd
       ((Set_reg Accu (Op | (Op << (Const 1) (Const 1)) (Const 1)))
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ PUSHCONST2 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    accu = 5;

    (step
     ((id 3)
      (cmd
       ((Set_reg Accu (Op | (Op << (Const 2) (Const 1)) (Const 1)))
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ PUSHCONST3 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    accu = 7;

    (step
     ((id 3)
      (cmd
       ((Set_reg Accu (Op | (Op << (Const 3) (Const 1)) (Const 1)))
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ PUSHCONSTINT ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = pc;
    _4 = program[(_3>>3)];
    _5 = pc;
    pc = (_5+4);
    accu = (((_4>>+32)<<1)|1);

    (step
     ((id 6)
      (cmd
       ((Set_reg Accu
         (Op | (Op << (Op >>+ (Val 4) (Const 32)) (Const 1)) (Const 1)))
        (Set_reg Pc (Op + (Val 5) (Const 4))) (Get_reg 5 Pc)
        (Get_mem 4 Program
         (Op >> (Op + (Val 3) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 3 Pc)
        (Set_mem Stack
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)) (Val 0))
        (Get_reg 2 Sp) (Set_reg Sp (Op - (Val 1) (Const 8))) (Get_reg 1 Sp)
        (Get_reg 0 Accu)))))

    ____ NEGINT ____

    _0 = accu;
    accu = (2-_0);

    (step
     ((id 1) (cmd ((Set_reg Accu (Op - (Const 2) (Val 0))) (Get_reg 0 Accu)))))

    ____ ADDINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = ((_3+_1)-1);

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu (Op - (Op + (Val 3) (Val 1)) (Const 1))) (Get_reg 3 Accu)
        (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ SUBINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = ((_3-_1)+1);

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu (Op + (Op - (Val 3) (Val 1)) (Const 1))) (Get_reg 3 Accu)
        (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ MULINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = ((((_3>>+1)*(_1>>+1))<<1)|1);

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu
         (Op |
          (Op << (Op * (Op >>+ (Val 3) (Const 1)) (Op >>+ (Val 1) (Const 1)))
           (Const 1))
          (Const 1)))
        (Get_reg 3 Accu) (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ DIVINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    if ((_1>>+1)==0) then
    else
      _3 = accu;
      accu = ((((_3>>+1)/(_1>>+1))<<1)|1);
    end

    (step
     ((id 4)
      (cmd
       ((Cond (Op == (Op >>+ (Val 1) (Const 1)) (Const 0)) ()
         ((Set_reg Accu
           (Op |
            (Op << (Op / (Op >>+ (Val 3) (Const 1)) (Op >>+ (Val 1) (Const 1)))
             (Const 1))
            (Const 1)))
          (Get_reg 3 Accu)))
        (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ MODINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    if ((_1>>+1)==0) then
    else
      _3 = accu;
      accu = ((((_3>>+1)%(_1>>+1))<<1)|1);
    end

    (step
     ((id 4)
      (cmd
       ((Cond (Op == (Op >>+ (Val 1) (Const 1)) (Const 0)) ()
         ((Set_reg Accu
           (Op |
            (Op << (Op % (Op >>+ (Val 3) (Const 1)) (Op >>+ (Val 1) (Const 1)))
             (Const 1))
            (Const 1)))
          (Get_reg 3 Accu)))
        (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ ANDINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (_3&_1);

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu (Op & (Val 3) (Val 1))) (Get_reg 3 Accu)
        (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ ORINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (_3|_1);

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu (Op | (Val 3) (Val 1))) (Get_reg 3 Accu)
        (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ XORINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = ((_3^_1)|1);

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu (Op | (Op ^ (Val 3) (Val 1)) (Const 1))) (Get_reg 3 Accu)
        (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ LSLINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (((_3-1)<<(_1>>+1))+1);

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu
         (Op + (Op << (Op - (Val 3) (Const 1)) (Op >>+ (Val 1) (Const 1)))
          (Const 1)))
        (Get_reg 3 Accu) (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ LSRINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (((_3-1)>>(_1>>+1))|1);

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu
         (Op | (Op >> (Op - (Val 3) (Const 1)) (Op >>+ (Val 1) (Const 1)))
          (Const 1)))
        (Get_reg 3 Accu) (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ ASRINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (((_3-1)>>+(_1>>+1))|1);

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu
         (Op | (Op >>+ (Op - (Val 3) (Const 1)) (Op >>+ (Val 1) (Const 1)))
          (Const 1)))
        (Get_reg 3 Accu) (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ EQ ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (((_3==_1)<<1)|1);

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu (Op | (Op << (Op == (Val 3) (Val 1)) (Const 1)) (Const 1)))
        (Get_reg 3 Accu) (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ NEQ ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (((_3<>_1)<<1)|1);

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu (Op | (Op << (Op <> (Val 3) (Val 1)) (Const 1)) (Const 1)))
        (Get_reg 3 Accu) (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ LTINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (((_3<+_1)<<1)|1);

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu (Op | (Op << (Op <+ (Val 3) (Val 1)) (Const 1)) (Const 1)))
        (Get_reg 3 Accu) (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ LEINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (((_3<=+_1)<<1)|1);

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu
         (Op | (Op << (Op <=+ (Val 3) (Val 1)) (Const 1)) (Const 1)))
        (Get_reg 3 Accu) (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ GTINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (((_3>+_1)<<1)|1);

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu (Op | (Op << (Op >+ (Val 3) (Val 1)) (Const 1)) (Const 1)))
        (Get_reg 3 Accu) (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ GEINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (((_3>=+_1)<<1)|1);

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu
         (Op | (Op << (Op >=+ (Val 3) (Val 1)) (Const 1)) (Const 1)))
        (Get_reg 3 Accu) (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ OFFSETINT ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = accu;
    accu = (_3+((_1>>+32)<<1));

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu
         (Op + (Val 3) (Op << (Op >>+ (Val 1) (Const 32)) (Const 1))))
        (Get_reg 3 Accu) (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ OFFSETREF ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = accu;
    _4 = mem[(_3>>3)];
    mem[(_3>>3)] = (_4+((_1>>+32)<<1));
    accu = 1;

    (step
     ((id 5)
      (cmd
       ((Set_reg Accu (Const 1))
        (Set_mem Mem (Op >> (Op + (Val 3) (Op << (Const 0) (Const 3))) (Const 3))
         (Op + (Val 4) (Op << (Op >>+ (Val 1) (Const 32)) (Const 1))))
        (Get_mem 4 Mem
         (Op >> (Op + (Val 3) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 3 Accu) (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ ISINT ____

    _0 = accu;
    accu = (((_0&1)<<1)|1);

    (step
     ((id 1)
      (cmd
       ((Set_reg Accu
         (Op | (Op << (Op & (Val 0) (Const 1)) (Const 1)) (Const 1)))
        (Get_reg 0 Accu)))))

    ____ GETMETHOD ____

    _0 = accu;
    _1 = sp;
    _2 = stack[(_1>>3)];
    _3 = mem[(_2>>3)];
    _4 = mem[((_3+((_0>>+1)<<3))>>3)];
    accu = _4;

    (step
     ((id 5)
      (cmd
       ((Set_reg Accu (Val 4))
        (Get_mem 4 Mem
         (Op >> (Op + (Val 3) (Op << (Op >>+ (Val 0) (Const 1)) (Const 3)))
          (Const 3)))
        (Get_mem 3 Mem
         (Op >> (Op + (Val 2) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_mem 2 Stack
         (Op >> (Op + (Val 1) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 1 Sp) (Get_reg 0 Accu)))))

    ____ BEQ ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = accu;
    if ((_1>>+32)==(_3>>+1)) then
      _4 = pc;
      _5 = program[(_4>>3)];
      _6 = pc;
      pc = (_6+((_5>>+32)<<2));
    else
      _7 = pc;
      pc = (_7+4);
    end

    (step
     ((id 8)
      (cmd
       ((Cond (Op == (Op >>+ (Val 1) (Const 32)) (Op >>+ (Val 3) (Const 1)))
         ((Set_reg Pc
           (Op + (Val 6) (Op << (Op >>+ (Val 5) (Const 32)) (Const 2))))
          (Get_reg 6 Pc)
          (Get_mem 5 Program
           (Op >> (Op + (Val 4) (Op << (Const 0) (Const 2))) (Const 3)))
          (Get_reg 4 Pc))
         ((Set_reg Pc (Op + (Val 7) (Const 4))) (Get_reg 7 Pc)))
        (Get_reg 3 Accu) (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ BNEQ ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = accu;
    if ((_1>>+32)<>(_3>>+1)) then
      _4 = pc;
      _5 = program[(_4>>3)];
      _6 = pc;
      pc = (_6+((_5>>+32)<<2));
    else
      _7 = pc;
      pc = (_7+4);
    end

    (step
     ((id 8)
      (cmd
       ((Cond (Op <> (Op >>+ (Val 1) (Const 32)) (Op >>+ (Val 3) (Const 1)))
         ((Set_reg Pc
           (Op + (Val 6) (Op << (Op >>+ (Val 5) (Const 32)) (Const 2))))
          (Get_reg 6 Pc)
          (Get_mem 5 Program
           (Op >> (Op + (Val 4) (Op << (Const 0) (Const 2))) (Const 3)))
          (Get_reg 4 Pc))
         ((Set_reg Pc (Op + (Val 7) (Const 4))) (Get_reg 7 Pc)))
        (Get_reg 3 Accu) (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ BLTINT ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = accu;
    if ((_1>>+32)<+(_3>>+1)) then
      _4 = pc;
      _5 = program[(_4>>3)];
      _6 = pc;
      pc = (_6+((_5>>+32)<<2));
    else
      _7 = pc;
      pc = (_7+4);
    end

    (step
     ((id 8)
      (cmd
       ((Cond (Op <+ (Op >>+ (Val 1) (Const 32)) (Op >>+ (Val 3) (Const 1)))
         ((Set_reg Pc
           (Op + (Val 6) (Op << (Op >>+ (Val 5) (Const 32)) (Const 2))))
          (Get_reg 6 Pc)
          (Get_mem 5 Program
           (Op >> (Op + (Val 4) (Op << (Const 0) (Const 2))) (Const 3)))
          (Get_reg 4 Pc))
         ((Set_reg Pc (Op + (Val 7) (Const 4))) (Get_reg 7 Pc)))
        (Get_reg 3 Accu) (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ BLEINT ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = accu;
    if ((_1>>+32)<=+(_3>>+1)) then
      _4 = pc;
      _5 = program[(_4>>3)];
      _6 = pc;
      pc = (_6+((_5>>+32)<<2));
    else
      _7 = pc;
      pc = (_7+4);
    end

    (step
     ((id 8)
      (cmd
       ((Cond (Op <=+ (Op >>+ (Val 1) (Const 32)) (Op >>+ (Val 3) (Const 1)))
         ((Set_reg Pc
           (Op + (Val 6) (Op << (Op >>+ (Val 5) (Const 32)) (Const 2))))
          (Get_reg 6 Pc)
          (Get_mem 5 Program
           (Op >> (Op + (Val 4) (Op << (Const 0) (Const 2))) (Const 3)))
          (Get_reg 4 Pc))
         ((Set_reg Pc (Op + (Val 7) (Const 4))) (Get_reg 7 Pc)))
        (Get_reg 3 Accu) (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ BGTINT ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = accu;
    if ((_1>>+32)>+(_3>>+1)) then
      _4 = pc;
      _5 = program[(_4>>3)];
      _6 = pc;
      pc = (_6+((_5>>+32)<<2));
    else
      _7 = pc;
      pc = (_7+4);
    end

    (step
     ((id 8)
      (cmd
       ((Cond (Op >+ (Op >>+ (Val 1) (Const 32)) (Op >>+ (Val 3) (Const 1)))
         ((Set_reg Pc
           (Op + (Val 6) (Op << (Op >>+ (Val 5) (Const 32)) (Const 2))))
          (Get_reg 6 Pc)
          (Get_mem 5 Program
           (Op >> (Op + (Val 4) (Op << (Const 0) (Const 2))) (Const 3)))
          (Get_reg 4 Pc))
         ((Set_reg Pc (Op + (Val 7) (Const 4))) (Get_reg 7 Pc)))
        (Get_reg 3 Accu) (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ BGEINT ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = accu;
    if ((_1>>+32)>=+(_3>>+1)) then
      _4 = pc;
      _5 = program[(_4>>3)];
      _6 = pc;
      pc = (_6+((_5>>+32)<<2));
    else
      _7 = pc;
      pc = (_7+4);
    end

    (step
     ((id 8)
      (cmd
       ((Cond (Op >=+ (Op >>+ (Val 1) (Const 32)) (Op >>+ (Val 3) (Const 1)))
         ((Set_reg Pc
           (Op + (Val 6) (Op << (Op >>+ (Val 5) (Const 32)) (Const 2))))
          (Get_reg 6 Pc)
          (Get_mem 5 Program
           (Op >> (Op + (Val 4) (Op << (Const 0) (Const 2))) (Const 3)))
          (Get_reg 4 Pc))
         ((Set_reg Pc (Op + (Val 7) (Const 4))) (Get_reg 7 Pc)))
        (Get_reg 3 Accu) (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ ULTINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (((_3<_1)<<1)|1);

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu (Op | (Op << (Op < (Val 3) (Val 1)) (Const 1)) (Const 1)))
        (Get_reg 3 Accu) (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ UGEINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (((_3>=_1)<<1)|1);

    (step
     ((id 4)
      (cmd
       ((Set_reg Accu (Op | (Op << (Op >= (Val 3) (Val 1)) (Const 1)) (Const 1)))
        (Get_reg 3 Accu) (Set_reg Sp (Op + (Val 2) (Const 8))) (Get_reg 2 Sp)
        (Get_mem 1 Stack
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 3))) (Const 3)))
        (Get_reg 0 Sp)))))

    ____ BULTINT ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = accu;
    if ((_1>>+32)<(_3>>+1)) then
      _4 = pc;
      _5 = program[(_4>>3)];
      _6 = pc;
      pc = (_6+((_5>>+32)<<2));
    else
      _7 = pc;
      pc = (_7+4);
    end

    (step
     ((id 8)
      (cmd
       ((Cond (Op < (Op >>+ (Val 1) (Const 32)) (Op >>+ (Val 3) (Const 1)))
         ((Set_reg Pc
           (Op + (Val 6) (Op << (Op >>+ (Val 5) (Const 32)) (Const 2))))
          (Get_reg 6 Pc)
          (Get_mem 5 Program
           (Op >> (Op + (Val 4) (Op << (Const 0) (Const 2))) (Const 3)))
          (Get_reg 4 Pc))
         ((Set_reg Pc (Op + (Val 7) (Const 4))) (Get_reg 7 Pc)))
        (Get_reg 3 Accu) (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ BUGEINT ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = accu;
    if ((_1>>+32)>=(_3>>+1)) then
      _4 = pc;
      _5 = program[(_4>>3)];
      _6 = pc;
      pc = (_6+((_5>>+32)<<2));
    else
      _7 = pc;
      pc = (_7+4);
    end

    (step
     ((id 8)
      (cmd
       ((Cond (Op >= (Op >>+ (Val 1) (Const 32)) (Op >>+ (Val 3) (Const 1)))
         ((Set_reg Pc
           (Op + (Val 6) (Op << (Op >>+ (Val 5) (Const 32)) (Const 2))))
          (Get_reg 6 Pc)
          (Get_mem 5 Program
           (Op >> (Op + (Val 4) (Op << (Const 0) (Const 2))) (Const 3)))
          (Get_reg 4 Pc))
         ((Set_reg Pc (Op + (Val 7) (Const 4))) (Get_reg 7 Pc)))
        (Get_reg 3 Accu) (Set_reg Pc (Op + (Val 2) (Const 4))) (Get_reg 2 Pc)
        (Get_mem 1 Program
         (Op >> (Op + (Val 0) (Op << (Const 0) (Const 2))) (Const 3)))
        (Get_reg 0 Pc)))))

    ____ GETPUBMET ____

    NOT IMPLEMENTED
    ____ GETDYNMET ____

    NOT IMPLEMENTED
    ____ STOP ____


    (stop ((id 0) (cmd ())))

    ____ EVENT ____

    NOT IMPLEMENTED
    ____ BREAK ____

    NOT IMPLEMENTED
    ____ RERAISE ____

    NOT IMPLEMENTED
    ____ RAISE_NOTRACE ____

    NOT IMPLEMENTED |}]
;;
