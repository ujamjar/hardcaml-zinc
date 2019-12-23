type instruction = Opcode.t * int32 list

type bytecode = instruction list

val acc0 : instruction

val acc1 : instruction

val acc2 : instruction

val acc3 : instruction

val acc4 : instruction

val acc5 : instruction

val acc6 : instruction

val acc7 : instruction

val acc : int32 -> instruction

val push : instruction

val pushacc0 : instruction

val pushacc1 : instruction

val pushacc2 : instruction

val pushacc3 : instruction

val pushacc4 : instruction

val pushacc5 : instruction

val pushacc6 : instruction

val pushacc7 : instruction

val pushacc : int32 -> instruction

val pop : int32 -> instruction

val assign : int32 -> instruction

val envacc1 : instruction

val envacc2 : instruction

val envacc3 : instruction

val envacc4 : instruction

val envacc : int32 -> instruction

val pushenvacc1 : instruction

val pushenvacc2 : instruction

val pushenvacc3 : instruction

val pushenvacc4 : instruction

val pushenvacc : int32 -> instruction

val push_retaddr : int32 -> instruction

val apply : int32 -> instruction

val apply1 : int32 -> instruction

val apply2 : int32 -> instruction

val apply3 : int32 -> instruction

val appterm : int32 -> int32 -> instruction

val appterm1 : int32 -> instruction

val appterm2 : int32 -> instruction

val appterm3 : int32 -> instruction

val return : int32 -> instruction

val restart : instruction

val grab : int32 -> instruction

val closure : int32 -> int32 -> instruction

val closurerec : int32 -> int32 list -> instruction

val pushoffsetclosure : int32 -> instruction

val offsetclosure : int32 -> instruction

val pushoffsetclosurem2 : instruction

val offsetclosure0 : instruction

val pushoffsetclosure0 : instruction

val offsetclosure2 : instruction

val pushgetglobal : int32 -> instruction

val getglobal : int32 -> instruction

val pushgetglobalfield : int32 -> int32 -> instruction

val getglobalfield : int32 -> int32 -> instruction

val setglobal : int32 -> instruction

val pushatom0 : instruction

val atom0 : instruction

val pushatom : int32 -> instruction

val atom : int32 -> instruction

val makeblock : int32 -> int32 -> instruction

val makeblock1 : int32 -> instruction

val makeblock2 : int32 -> instruction

val makeblock3 : int32 -> instruction

val makefloatblock : int32 -> instruction

val getfield0 : instruction

val getfield1 : instruction

val getfield2 : instruction

val getfield3 : instruction

val getfield : int32 -> instruction

val getfloatfield : int32 -> instruction

val setfield0 : instruction

val setfield1 : instruction

val setfield2 : instruction

val setfield3 : instruction

val setfield : int32 -> instruction

val setfloatfield : int32 -> instruction

val vectlength : instruction

val getvectitem : instruction

val setvectitem : instruction

val getstringchar : instruction

val setstringchar : instruction

val branch : int32 -> instruction

val branchif : int32 -> instruction

val branchifnot : int32 -> instruction

val switch : int32 -> int32 list -> instruction

val boolnot : instruction

val pushtrap : int32 -> instruction

val poptrap : instruction

val raise_notrace : instruction

val reraise : instruction

val raise_ : instruction

val check_signals : instruction

val c_call1 : int32 -> instruction

val c_call2 : int32 -> instruction

val c_call3 : int32 -> instruction

val c_call4 : int32 -> instruction

val c_call5 : int32 -> instruction

val c_calln : int32 -> int32 -> instruction

val const0 : instruction

val const1 : instruction

val const2 : instruction

val const3 : instruction

val pushconst0 : instruction

val pushconst1 : instruction

val pushconst2 : instruction

val pushconst3 : instruction

val pushconstint : int32 -> instruction

val constint : int32 -> instruction

val negint : instruction

val addint : instruction

val subint : instruction

val mulint : instruction

val divint : instruction

val modint : instruction

val andint : instruction

val orint : instruction

val xorint : instruction

val lslint : instruction

val lsrint : instruction

val asrint : instruction

val eq : instruction

val neq : instruction

val ltint : instruction

val leint : instruction

val gtint : instruction

val geint : instruction

val ultint : instruction

val ugeint : instruction

val beq : instruction

val bneq : instruction

val bltint : instruction

val bleint : instruction

val bgtint : instruction

val bgeint : instruction

val bultint : instruction

val bugeint : instruction

val offsetint : int32 -> instruction

val offsetref : int32 -> instruction

val isint : instruction

val getmethod : instruction

val getpubmet : int32 -> instruction

val getdynmet : int32 -> instruction

val stop : instruction

val event : instruction

val break : instruction

val to_array : bytecode -> int32 array
