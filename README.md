# General

An implementation of the ZINC Abstract Machine (ZAM) in HardCaml
for running OCaml bytecode programs in hardware.

**Status** Transitioning to a new way of implementating the hardware 
core which includes a software model, derivied from the same code,
for much easier (and faster) debugging.

# New version

The module Interp includes a reasonably well tested implementation
of the bytecode instruction set.  It is functorized
and can execute bytecode programs or produce an AST.

The executable implementation is useful for tracing bytecode programs,
testing the implementation of the instruction set and, crucially,
the included C-runtime.  A fair amount of tracing functionality is 
included and can be compared to the traces generated by `ocamlrund`.
Further, the OCaml REPL can be used as a simple debugger 
(`Framework.Interp.interactive`).

The AST implementation records how machine registers are updated
along with the memory accesses required.  Such a sequence of
operations covers much of the functionality of the instruction
set.  In addition we have loops, conditionals and some more
abstract AST nodes to represent operations we want to defer the
implementation of (c calls, oo method ops). 

This leads us to the current status of the project; compile the
AST into hardware.

The initial plan is to compile each instruction seperately into 
a statemachine (sequenced by memory accesses), and mux them together 
based on an instruction decoder.  This should quickly lead to a 
working and pretty complete implementation.  On the downside it
will be a lot bigger and slower (frequency and cycles/instruction)
than necessary.

Various ongoing tasks to make this core useful include;

* Port to FPGA - get something real working
  * Include RISC companion core for runtime
* Analyse the AST to expose more efficient hardware implementations
* Runtime
  * Improve simple version in `C\_runtime`
  * Port ocamlrun c-code and use that as the runtime
  * How much could be moved to ocaml code?
* Garbage collector ... not looked at this at all yet.

# Main modules

* Machine - Types representing the ZINC machine
* Load - Read bytecode, parse global data
* Instr - Bytecode instruction set definitions
* Mlvalues - Manipulating the OCaml representation of values
* Trace - Debugging functions
* Repr - Conversion of OCaml values to int64 arrays
* Interp - Abstract implementation of bytecode instruction set
* C\_runtime - Various functions from the interpreters C-runtime 
* Zinc - Old hardware implementation
* Framework - Testbench and tracing

# Old version

**TO BE DEPRECIATED**

Implemented as a large statemachine in `Zinc`.  A major concern with this
approach is the size of the statemachine - dozens of states so far, and 
a number of the more complex instructions still to go.  A complete version
would probably need hundreds of states which makes this approach a bit
impractical.

* **Status; says 'hello world!'** which actually exercises a pretty good chuck
  of the bytecode instruction set, and requires integration with a very basic
  (C-)runtime

* Simulation only at the moment and will need some additions before a move 
  to real hardware is worthwhile (proper memory interface - and probably 
  caches - along with a more complete runtime system)

* Currently only models a 64 bit OCaml ZAM as this matches my compiler.  
  Should be fairly straight forward to model 32 bit as well.

* Memory layout is shown below.  A tiny c-heap is provided seperately for
  the c-runtime to simplify things for the time being.  The ml-heap grows
  upwards and the stack starts at the top of memory and grows downwards.

| Memory layout |
|---------------|
| program data  |
| atom table    |
| global data   |
| c-heap        |
| ml-heap       |
| stack         |

* No garbage collector for the time being.  Will need to address this in stages;
  1. unify the c and ml heaps and provide a general allocation procedure
  2. some form of basic garbage collector implemented in the testbench so we 
     can run bigger test cases
  3. figure out what to do for an actual hardware system (lots of choices here 
     as we can provide hardware assistance)

* For C_CALLs the testbench intercepts the call and executes it with routines 
  defined in C\_runtime.  Hopefully there's a better way than just re-implementing 
  lots and lots of OCaml C primitives here.

* A number of instructions are not yet implemented - todo as testcases are found.

