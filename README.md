# General

An implementation of the ZINC Abstract Machine (ZAM) in HardCaml
for running OCaml bytecode programs in hardware.

# Implementation

The module `Interp` provides a functorised implementation of the 
bytecode instruction set. It can be directly executed or produce a 
simple AST representation which is compiled to hardware.

Alongside this is an incomplete implementation of the OCaml runtime, itself
implemented in OCaml using a rather scary amount of `Obj.magic`.  This is 
used to build tests for the project.

# Status

Working

- Basic software interpreter based on `Interp`
- Simple hardware compilation strategy for /most/ instructions

Todo

- Build top level infrastructure and testbench.  Start running bytecode 
  programs in hardware simulation.
- Optimise hardware compilation to something more reasonable.
- 32 bit port
- Port to an FPGA board with companion processor (microblaze, picorv32?)
- Compile OCaml runtime to target companion processor
