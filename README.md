# Overview

An implementation of the ZINC OCaml bytecode machine in HardCaml
for compilation into hardware (in particular FPGAs).

```
    +------------------+ 
    | memory interface | 
    +------------------+ 
              |
       +-------------+
       |             |
  +---------+   +---------+
  | ZINC SM |---| RISC uP |
  +---------+   +---------+
```

The ZINC stack machine will run OCaml bytecode while the RISC processor
will handle the garbage collector and c-calls.

A much more interesting design will include multiple ZINC SM nodes and 
the multicore OCaml runtime system.

# ZINC Stack Machine

```
        +------------------+ 
 +------| memory interface | 
 |      +------------------+ 
 |                |
 |     +---------------------+
 |     |          |          |
 | +-------+  +-------+  +-------+
 | | Instr |  | Minor |  | Stack |
 | | cache |  | heap  |  |       |
 | +-------+  +-------+  +-------+
 |     |          |          |
 |     +---------------------+
 |                |
 |        +---------------+    +--------------+
 +--------| state machine |----| uP Interface |
          +---------------+    +--------------+
                  |
               +-----+
               | ALU |
               +-----+
```


