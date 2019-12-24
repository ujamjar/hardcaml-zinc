open Base
open Stdio.Out_channel
open Hardcaml_zinc
open Zinc2

let show instr =
  try S.print @@ Compile.simplify @@ snd @@ O.dispatch instr S.empty with
  | _ -> printf "NOT IMPLEMENTED\n"
;;

let%expect_test "show all instruction implementations" =
  List.iter Opcode.all ~f:(fun opcode ->
      printf "____ %s ____\n\n" (Opcode.to_string opcode);
      show opcode;
      printf "\n");
  [%expect
    {|
    ____ ACC0 ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    accu = _1;

    ____ ACC1 ____

    _0 = sp;
    _1 = stack[((_0+8)>>3)];
    accu = _1;

    ____ ACC2 ____

    _0 = sp;
    _1 = stack[((_0+16)>>3)];
    accu = _1;

    ____ ACC3 ____

    _0 = sp;
    _1 = stack[((_0+24)>>3)];
    accu = _1;

    ____ ACC4 ____

    _0 = sp;
    _1 = stack[((_0+32)>>3)];
    accu = _1;

    ____ ACC5 ____

    _0 = sp;
    _1 = stack[((_0+40)>>3)];
    accu = _1;

    ____ ACC6 ____

    _0 = sp;
    _1 = stack[((_0+48)>>3)];
    accu = _1;

    ____ ACC7 ____

    _0 = sp;
    _1 = stack[((_0+56)>>3)];
    accu = _1;

    ____ ACC ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = sp;
    _4 = stack[((_3+((_1>>+32)<<3))>>3)];
    accu = _4;

    ____ PUSH ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;

    ____ PUSHACC0 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = sp;
    _4 = stack[(_3>>3)];
    accu = _4;

    ____ PUSHACC1 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = sp;
    _4 = stack[((_3+8)>>3)];
    accu = _4;

    ____ PUSHACC2 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = sp;
    _4 = stack[((_3+16)>>3)];
    accu = _4;

    ____ PUSHACC3 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = sp;
    _4 = stack[((_3+24)>>3)];
    accu = _4;

    ____ PUSHACC4 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = sp;
    _4 = stack[((_3+32)>>3)];
    accu = _4;

    ____ PUSHACC5 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = sp;
    _4 = stack[((_3+40)>>3)];
    accu = _4;

    ____ PUSHACC6 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = sp;
    _4 = stack[((_3+48)>>3)];
    accu = _4;

    ____ PUSHACC7 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = sp;
    _4 = stack[((_3+56)>>3)];
    accu = _4;

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

    ____ POP ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = sp;
    sp = (_3+((_1>>+32)<<3));

    ____ ASSIGN ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = accu;
    _4 = sp;
    stack[((_4+((_1>>+32)<<3))>>3)] = _3;
    accu = 1;

    ____ ENVACC1 ____

    _0 = env;
    _1 = mem[((_0+8)>>3)];
    accu = _1;

    ____ ENVACC2 ____

    _0 = env;
    _1 = mem[((_0+16)>>3)];
    accu = _1;

    ____ ENVACC3 ____

    _0 = env;
    _1 = mem[((_0+24)>>3)];
    accu = _1;

    ____ ENVACC4 ____

    _0 = env;
    _1 = mem[((_0+32)>>3)];
    accu = _1;

    ____ ENVACC ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = env;
    _4 = mem[((_3+((_1>>+32)<<3))>>3)];
    accu = _4;

    ____ PUSHENVACC1 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = env;
    _4 = mem[((_3+8)>>3)];
    accu = _4;

    ____ PUSHENVACC2 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = env;
    _4 = mem[((_3+16)>>3)];
    accu = _4;

    ____ PUSHENVACC3 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = env;
    _4 = mem[((_3+24)>>3)];
    accu = _4;

    ____ PUSHENVACC4 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = env;
    _4 = mem[((_3+32)>>3)];
    accu = _4;

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

    ____ OFFSETCLOSUREM2 ____

    _0 = env;
    accu = (_0+-16);

    ____ OFFSETCLOSURE0 ____

    _0 = env;
    accu = _0;

    ____ OFFSETCLOSURE2 ____

    _0 = env;
    accu = (_0+16);

    ____ OFFSETCLOSURE ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = env;
    accu = (_3+((_1>>+32)<<3));

    ____ PUSHOFFSETCLOSUREM2 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = env;
    accu = (_3+-16);

    ____ PUSHOFFSETCLOSURE0 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = env;
    accu = _3;

    ____ PUSHOFFSETCLOSURE2 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = env;
    accu = (_3+16);

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

    ____ GETGLOBAL ____

    _0 = global_data;
    _1 = pc;
    _2 = program[(_1>>3)];
    _3 = pc;
    pc = (_3+4);
    _4 = mem[((_0+((_2>>+32)<<3))>>3)];
    accu = _4;

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

    ____ SETGLOBAL ____

    _0 = global_data;
    _1 = pc;
    _2 = program[(_1>>3)];
    _3 = pc;
    pc = (_3+4);
    _4 = accu;
    mem[((_0+((_2>>+32)<<3))>>3)] = _4;
    accu = 1;

    ____ ATOM0 ____

    _0 = atom_table;
    accu = _0;

    ____ ATOM ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = atom_table;
    accu = (_3+((_1>>+32)<<3));

    ____ PUSHATOM0 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    _3 = atom_table;
    accu = _3;

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

    ____ GETFIELD0 ____

    _0 = accu;
    _1 = mem[(_0>>3)];
    accu = _1;

    ____ GETFIELD1 ____

    _0 = accu;
    _1 = mem[((_0+8)>>3)];
    accu = _1;

    ____ GETFIELD2 ____

    _0 = accu;
    _1 = mem[((_0+16)>>3)];
    accu = _1;

    ____ GETFIELD3 ____

    _0 = accu;
    _1 = mem[((_0+24)>>3)];
    accu = _1;

    ____ GETFIELD ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = accu;
    _4 = mem[((_3+((_1>>+32)<<3))>>3)];
    accu = _4;

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

    ____ SETFIELD0 ____

    _0 = accu;
    _1 = sp;
    _2 = stack[(_1>>3)];
    _3 = sp;
    sp = (_3+8);
    mem[(_0>>3)] = _2;
    accu = 1;

    ____ SETFIELD1 ____

    _0 = accu;
    _1 = sp;
    _2 = stack[(_1>>3)];
    _3 = sp;
    sp = (_3+8);
    mem[((_0+8)>>3)] = _2;
    accu = 1;

    ____ SETFIELD2 ____

    _0 = accu;
    _1 = sp;
    _2 = stack[(_1>>3)];
    _3 = sp;
    sp = (_3+8);
    mem[((_0+16)>>3)] = _2;
    accu = 1;

    ____ SETFIELD3 ____

    _0 = accu;
    _1 = sp;
    _2 = stack[(_1>>3)];
    _3 = sp;
    sp = (_3+8);
    mem[((_0+24)>>3)] = _2;
    accu = 1;

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

    ____ VECTLENGTH ____

    _0 = accu;
    _1 = mem[((_0-8)>>3)];
    accu = (((_1>>10)<<1)|1);

    ____ GETVECTITEM ____

    _0 = accu;
    _1 = sp;
    _2 = stack[(_1>>3)];
    _3 = sp;
    sp = (_3+8);
    _4 = mem[((_0+((_2>>+1)<<3))>>3)];
    accu = _4;

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

    ____ GETSTRINGCHAR ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    _4 = mem[((_3+((_1>>+1)&-8))>>3)];
    accu = ((((_4>>(((_1>>+1)&7)<<3))&255)<<1)|1);

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

    ____ BRANCH ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+((_1>>+32)<<2));

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

    ____ BOOLNOT ____

    _0 = accu;
    accu = (4-_0);

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

    ____ POPTRAP ____

    if 0 then
    else
      _0 = sp;
      _1 = stack[((_0+8)>>3)];
      trapsp = _1;
      _2 = sp;
      sp = (_2+32);
    end

    ____ RAISE ____

    NOT IMPLEMENTED

    ____ CHECK_SIGNALS ____

    if 0 then
    else
    end

    ____ C_CALL1 ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);

    ____ C_CALL2 ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);

    ____ C_CALL3 ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);

    ____ C_CALL4 ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);

    ____ C_CALL5 ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);

    ____ C_CALLN ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);

    ____ CONST0 ____

    accu = 1;

    ____ CONST1 ____

    accu = 3;

    ____ CONST2 ____

    accu = 5;

    ____ CONST3 ____

    accu = 7;

    ____ CONSTINT ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    accu = (((_1>>+32)<<1)|1);

    ____ PUSHCONST0 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    accu = 1;

    ____ PUSHCONST1 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    accu = 3;

    ____ PUSHCONST2 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    accu = 5;

    ____ PUSHCONST3 ____

    _0 = accu;
    _1 = sp;
    sp = (_1-8);
    _2 = sp;
    stack[(_2>>3)] = _0;
    accu = 7;

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

    ____ NEGINT ____

    _0 = accu;
    accu = (2-_0);

    ____ ADDINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = ((_3+_1)-1);

    ____ SUBINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = ((_3-_1)+1);

    ____ MULINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = ((((_3>>+1)*(_1>>+1))<<1)|1);

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

    ____ ANDINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (_3&_1);

    ____ ORINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (_3|_1);

    ____ XORINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = ((_3^_1)|1);

    ____ LSLINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (((_3-1)<<(_1>>+1))+1);

    ____ LSRINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (((_3-1)>>(_1>>+1))|1);

    ____ ASRINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (((_3-1)>>+(_1>>+1))|1);

    ____ EQ ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (((_3==_1)<<1)|1);

    ____ NEQ ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (((_3<>_1)<<1)|1);

    ____ LTINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (((_3<+_1)<<1)|1);

    ____ LEINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (((_3<=+_1)<<1)|1);

    ____ GTINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (((_3>+_1)<<1)|1);

    ____ GEINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (((_3>=+_1)<<1)|1);

    ____ OFFSETINT ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = accu;
    accu = (_3+((_1>>+32)<<1));

    ____ OFFSETREF ____

    _0 = pc;
    _1 = program[(_0>>3)];
    _2 = pc;
    pc = (_2+4);
    _3 = accu;
    _4 = mem[(_3>>3)];
    mem[(_3>>3)] = (_4+((_1>>+32)<<1));
    accu = 1;

    ____ ISINT ____

    _0 = accu;
    accu = (((_0&1)<<1)|1);

    ____ GETMETHOD ____

    _0 = accu;
    _1 = sp;
    _2 = stack[(_1>>3)];
    _3 = mem[(_2>>3)];
    _4 = mem[((_3+((_0>>+1)<<3))>>3)];
    accu = _4;

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

    ____ ULTINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (((_3<_1)<<1)|1);

    ____ UGEINT ____

    _0 = sp;
    _1 = stack[(_0>>3)];
    _2 = sp;
    sp = (_2+8);
    _3 = accu;
    accu = (((_3>=_1)<<1)|1);

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

    ____ GETPUBMET ____

    NOT IMPLEMENTED

    ____ GETDYNMET ____

    NOT IMPLEMENTED

    ____ STOP ____


    ____ EVENT ____

    NOT IMPLEMENTED

    ____ BREAK ____

    NOT IMPLEMENTED

    ____ RERAISE ____

    NOT IMPLEMENTED

    ____ RAISE_NOTRACE ____

    NOT IMPLEMENTED |}]
;;
