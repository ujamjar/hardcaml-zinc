type opcodes = 
  ACC0 | ACC1 | ACC2 | ACC3 | ACC4 | ACC5 | ACC6 | ACC7 | ACC | 
  
  PUSH | PUSHACC0 | PUSHACC1 | PUSHACC2 | PUSHACC3 |
  PUSHACC4 | PUSHACC5 | PUSHACC6 | PUSHACC7 | PUSHACC | 
  
  POP | ASSIGN |
  
  ENVACC1 | ENVACC2 | ENVACC3 | ENVACC4 | ENVACC |
  
  PUSHENVACC1 | PUSHENVACC2 | PUSHENVACC3 | PUSHENVACC4 | PUSHENVACC |
  
  PUSH_RETADDR | 
  
  APPLY | APPLY1 | APPLY2 | APPLY3 |
  
  APPTERM | APPTERM1 | APPTERM2 | APPTERM3 |
  
  RETURN | RESTART | GRAB | CLOSURE | CLOSUREREC |
  
  OFFSETCLOSUREM2 | OFFSETCLOSURE0 | OFFSETCLOSURE2 | OFFSETCLOSURE |

  PUSHOFFSETCLOSUREM2 | PUSHOFFSETCLOSURE0 | PUSHOFFSETCLOSURE2 | PUSHOFFSETCLOSURE |
  
  GETGLOBAL | PUSHGETGLOBAL | GETGLOBALFIELD | PUSHGETGLOBALFIELD | SETGLOBAL |
  
  ATOM0 | ATOM | 
  
  PUSHATOM0 | PUSHATOM |
  
  MAKEBLOCK | MAKEBLOCK1 | MAKEBLOCK2 | MAKEBLOCK3 | MAKEFLOATBLOCK |
  
  GETFIELD0 | GETFIELD1 | GETFIELD2 | GETFIELD3 | GETFIELD | GETFLOATFIELD |

  SETFIELD0 | SETFIELD1 | SETFIELD2 | SETFIELD3 | SETFIELD | SETFLOATFIELD |
  
  VECTLENGTH | GETVECTITEM | SETVECTITEM |
  
  GETSTRINGCHAR | SETSTRINGCHAR |
  
  BRANCH | BRANCHIF | BRANCHIFNOT | SWITCH | 
  
  BOOLNOT |
  
  PUSHTRAP | POPTRAP | RAISE |
  
  CHECK_SIGNALS |
  
  C_CALL1 | C_CALL2 | C_CALL3 | C_CALL4 | C_CALL5 | C_CALLN |
  
  CONST0 | CONST1 | CONST2 | CONST3 | CONSTINT |
  
  PUSHCONST0 | PUSHCONST1 | PUSHCONST2 | PUSHCONST3 | PUSHCONSTINT |
  
  NEGINT | ADDINT | SUBINT | MULINT | DIVINT | MODINT |
  ANDINT | ORINT | XORINT | LSLINT | LSRINT | ASRINT |
  
  EQ | NEQ | LTINT | LEINT | GTINT | GEINT |
  
  OFFSETINT | OFFSETREF | ISINT | GETMETHOD |
  
  BEQ | BNEQ |  BLTINT | BLEINT | BGTINT | BGEINT |
  
  ULTINT | UGEINT | BULTINT | BUGEINT |
  
  GETPUBMET | GETDYNMET |
  
  STOP | EVENT | BREAK | RERAISE | RAISE_NOTRACE

  deriving(Enum,Bounded,Show)



(* simple coding API for the byte codes, which includes any arguments.
 * we'll do a simple translation into an exe so we can run test sequences 
 * through the testbench *)

type instruction = opcodes * Int32.t list
type bytecode = instruction list

let acc0 = ACC0, []
let acc1 = ACC1, []
let acc2 = ACC2, []
let acc3 = ACC3, []
let acc4 = ACC4, []
let acc5 = ACC5, []
let acc6 = ACC6, []
let acc7 = ACC7, []
let acc n = ACC, [n]

let push = PUSH, []
let pushacc0 = PUSHACC0, []
let pushacc1 = PUSHACC1, []
let pushacc2 = PUSHACC2, []
let pushacc3 = PUSHACC3, []
let pushacc4 = PUSHACC4, []
let pushacc5 = PUSHACC5, []
let pushacc6 = PUSHACC6, []
let pushacc7 = PUSHACC7, []
let pushacc n = PUSHACC, [n]

let pop n = POP, [n]
let assign n = ASSIGN, [n]

let envacc1 = ENVACC1, []
let envacc2 = ENVACC1, []
let envacc3 = ENVACC1, []
let envacc4 = ENVACC1, []
let envacc n = ENVACC, [n]

let pushenvacc1 = PUSHENVACC1, []
let pushenvacc2 = PUSHENVACC2, []
let pushenvacc3 = PUSHENVACC3, []
let pushenvacc4 = PUSHENVACC4, []
let pushenvacc n = PUSHENVACC, [n]

let push_retaddr n = PUSH_RETADDR, [n]
let apply n = APPLY, [n]
let apply1 n = APPLY1, [n]
let apply2 n = APPLY2, [n]
let apply3 n = APPLY3, [n]
let appterm m n = APPTERM, [m;n]
let appterm1 n = APPTERM1, [n]
let appterm2 n = APPTERM2, [n]
let appterm3 n = APPTERM3, [n]
let return n = RETURN, [n]
let restart = RESTART, []
let grab n = GRAB, [n]

let closure n_vars func = CLOSURE, [n_vars;func]
let closurerec n_vars funcs = CLOSUREREC, n_vars::(Int32.of_int (List.length funcs))::funcs

let pushoffsetclosure n = PUSHOFFSETCLOSURE, [n]
let offsetclosure n = OFFSETCLOSURE, [n]
let pushoffsetclosurem2 = PUSHOFFSETCLOSUREM2, []
let offsetclosure0 = OFFSETCLOSURE0, []
let pushoffsetclosure0 = PUSHOFFSETCLOSURE0, []
let offsetclosure2 = OFFSETCLOSURE2, []

let pushgetglobal n = PUSHGETGLOBAL, [n]
let getglobal n = GETGLOBAL, [n]
let pushgetglobalfield m n = PUSHGETGLOBALFIELD, [n;m]
let getglobalfield m n = GETGLOBALFIELD, [n;m]
let setglobal n = SETGLOBAL, [n]

let pushatom0 = PUSHATOM0, []
let atom0 = ATOM0, []
let pushatom n = PUSHATOM, [n]
let atom n = ATOM, [n]

let makeblock m n  = MAKEBLOCK, [m;n]
let makeblock1 n = MAKEBLOCK1, [n]
let makeblock2 n = MAKEBLOCK2, [n]
let makeblock3 n = MAKEBLOCK3, [n]
let makefloatblock n = MAKEFLOATBLOCK, [n]

let getfield0 = GETFIELD0, []
let getfield1 = GETFIELD1, []
let getfield2 = GETFIELD2, []
let getfield3 = GETFIELD3, []
let getfield n = GETFIELD, [n]
let getfloatfield n  = GETFLOATFIELD, [n]

let setfield0 = SETFIELD0, []
let setfield1 = SETFIELD1, []
let setfield2 = SETFIELD2, []
let setfield3 = SETFIELD3, []
let setfield n = SETFIELD, [n]
let setfloatfield n = SETFLOATFIELD, [n]

let vectlength = VECTLENGTH, []
let getvectitem = GETVECTITEM, []
let setvectitem = SETVECTITEM, []

let getstringchar = GETSTRINGCHAR, []
let setstringchar = SETSTRINGCHAR, []

let branch n = BRANCH, [n]
let branchif n = BRANCHIF, [n]
let branchifnot n = BRANCHIFNOT, [n]

let switch sizes indices = SWITCH, sizes::indices (* XXX *)

let boolnot = BOOLNOT, []

let pushtrap n = PUSHTRAP, [n]
let poptrap = POPTRAP, []

let raise_notrace = RAISE_NOTRACE, []
let reraise = RERAISE
let raise_ = RAISE

let check_signals = CHECK_SIGNALS, []

let c_call1 n = C_CALL1, [n]
let c_call2 n = C_CALL2, [n]
let c_call3 n = C_CALL3, [n]
let c_call4 n = C_CALL4, [n]
let c_call5 n = C_CALL5, [n]
let c_calln n_args m = C_CALLN, [n_args;m]

let const0 = CONST0, []
let const1 = CONST1, []
let const2 = CONST2, []
let const3 = CONST3, []

let pushconst0 = PUSHCONST0, []
let pushconst1 = PUSHCONST1, []
let pushconst2 = PUSHCONST2, []
let pushconst3 = PUSHCONST3, []

let pushconstint n = PUSHCONSTINT, [n]
let constint n = CONSTINT, [n]

let negint = NEGINT, []
let addint = ADDINT, []
let subint = SUBINT, []
let mulint = MULINT, []
let divint = DIVINT, []
let modint = MODINT, []
let andint = ANDINT, []
let orint = ORINT, []
let xorint = XORINT, []
let lslint = LSLINT, []
let lsrint = LSRINT, []
let asrint = ASRINT, []

let eq = EQ, []
let neq = NEQ, []
let ltint = LTINT, []
let leint = LEINT, []
let gtint = GTINT, []
let geint = GEINT, []
let ultint = ULTINT, []
let ugeint = UGEINT, []

let beq = BEQ, []
let bneq = BNEQ, []
let bltint = BLTINT, []
let bleint = BLEINT, []
let bgtint = BGTINT, []
let bgeint = BGEINT, []
let bultint = BULTINT, []
let bugeint = BUGEINT, []

let offsetint n = OFFSETINT, [n]
let offsetref n = OFFSETREF, [n]
let isint = ISINT, []

let getmethod = GETMETHOD, []
let getpubmet n = GETPUBMET, [n] (* not sure - some weird cache things *)
let getdynmet n = GETDYNMET, [n]

let stop = STOP, []
let event = EVENT, []
let break = BREAK, []

let to_array codes = 
  Array.of_list @@
    List.concat @@ 
      List.map 
        (fun (o,a) -> (Int32.of_int @@ Enum.from_enum<opcodes> o) :: a) 
        codes

(* gc colours *)
let white = 0
let gray = 1
let blue = 2
let black = 3

(* tags *)
let lazy_tag = Obj.lazy_tag
let closure_tag = Obj.closure_tag
let object_tag = Obj.object_tag
let infix_tag = Obj.infix_tag
let forward_tag = Obj.forward_tag
let no_scan_tag = Obj.no_scan_tag
let abstract_tag = Obj.abstract_tag
let string_tag = Obj.string_tag
let double_tag = Obj.double_tag
let double_array_tag = Obj.double_array_tag
let custom_tag = Obj.custom_tag

