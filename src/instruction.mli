type t = Opcode.t * int32 list [@@deriving sexp_of]

val acc0 : t
val acc1 : t
val acc2 : t
val acc3 : t
val acc4 : t
val acc5 : t
val acc6 : t
val acc7 : t
val acc : int32 -> t
val push : t
val pushacc0 : t
val pushacc1 : t
val pushacc2 : t
val pushacc3 : t
val pushacc4 : t
val pushacc5 : t
val pushacc6 : t
val pushacc7 : t
val pushacc : int32 -> t
val pop : int32 -> t
val assign : int32 -> t
val envacc1 : t
val envacc2 : t
val envacc3 : t
val envacc4 : t
val envacc : int32 -> t
val pushenvacc1 : t
val pushenvacc2 : t
val pushenvacc3 : t
val pushenvacc4 : t
val pushenvacc : int32 -> t
val push_retaddr : int32 -> t
val apply : int32 -> t
val apply1 : int32 -> t
val apply2 : int32 -> t
val apply3 : int32 -> t
val appterm : int32 -> int32 -> t
val appterm1 : int32 -> t
val appterm2 : int32 -> t
val appterm3 : int32 -> t
val return : int32 -> t
val restart : t
val grab : int32 -> t
val closure : int32 -> int32 -> t
val closurerec : int32 -> int32 list -> t
val pushoffsetclosure : int32 -> t
val offsetclosure : int32 -> t
val pushoffsetclosurem2 : t
val offsetclosure0 : t
val pushoffsetclosure0 : t
val offsetclosure2 : t
val pushgetglobal : int32 -> t
val getglobal : int32 -> t
val pushgetglobalfield : int32 -> int32 -> t
val getglobalfield : int32 -> int32 -> t
val setglobal : int32 -> t
val pushatom0 : t
val atom0 : t
val pushatom : int32 -> t
val atom : int32 -> t
val makeblock : int32 -> int32 -> t
val makeblock1 : int32 -> t
val makeblock2 : int32 -> t
val makeblock3 : int32 -> t
val makefloatblock : int32 -> t
val getfield0 : t
val getfield1 : t
val getfield2 : t
val getfield3 : t
val getfield : int32 -> t
val getfloatfield : int32 -> t
val setfield0 : t
val setfield1 : t
val setfield2 : t
val setfield3 : t
val setfield : int32 -> t
val setfloatfield : int32 -> t
val vectlength : t
val getvectitem : t
val setvectitem : t
val getstringchar : t
val setstringchar : t
val branch : int32 -> t
val branchif : int32 -> t
val branchifnot : int32 -> t
val switch : int32 -> int32 list -> t
val boolnot : t
val pushtrap : int32 -> t
val poptrap : t
val raise_notrace : t
val reraise : t
val raise_ : t
val check_signals : t
val c_call1 : int32 -> t
val c_call2 : int32 -> t
val c_call3 : int32 -> t
val c_call4 : int32 -> t
val c_call5 : int32 -> t
val c_calln : int32 -> int32 -> t
val const0 : t
val const1 : t
val const2 : t
val const3 : t
val pushconst0 : t
val pushconst1 : t
val pushconst2 : t
val pushconst3 : t
val pushconstint : int32 -> t
val constint : int32 -> t
val negint : t
val addint : t
val subint : t
val mulint : t
val divint : t
val modint : t
val andint : t
val orint : t
val xorint : t
val lslint : t
val lsrint : t
val asrint : t
val eq : t
val neq : t
val ltint : t
val leint : t
val gtint : t
val geint : t
val ultint : t
val ugeint : t
val beq : t
val bneq : t
val bltint : t
val bleint : t
val bgtint : t
val bgeint : t
val bultint : t
val bugeint : t
val offsetint : int32 -> t
val offsetref : int32 -> t
val isint : t
val getmethod : t
val getpubmet : int32 -> t
val getdynmet : int32 -> t
val stop : t
val event : t
val break : t
val to_array : t list -> int32 array
