# General

An implementation of the ZINC Abstract Machine (ZAM) in HardCaml
for running OCaml bytecode programs in hardware.

# Overview

* **Status; says 'hello world!'** which actually exercizes a pretty good chuck
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

# Modules

* Arbiter, State - some internal rtl design stuff
* Load - Read bytecode, parse global data
* Instr - Bytecode instruction set definitions
* C\_runtime - A __very__ basic set of c-runtime primitives
* Zinc - Hardware implementation
* Framework - Testbench and tracing

