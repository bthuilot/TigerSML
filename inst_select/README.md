# Assignment 6 - Instruction Selection

**Group:**

- Bryce Thuilot
- Khalil Haji

In this assignment, we added a mips code generator to our compiler.

**New Files:**

- `mipsgen.sml`
- `assem.sml` (provided to us)
- `olincanon2.sml` (provided to us)



# Factorial program

We have included the output for the factorial program. It should be noted that without proper liveness analysis and register allocation, this file is not yet executable.

## Generating the factorial program

We have included the factorial program assembly output in the file `fact.s`. This file is generated by running 
```shell
$ sml run.sml fact.tig
```

This will out a the file `fact.s` in the directory that program is run in


## Function calls

In our code generator, we move the first 4 arguments of a function call to the $a registers. We include the static link here. The function bodies, however, still refer to the temps as the "view shift" has not yet been implemented.

We use `jalr` instead of `jr` to call functions as the former saves the return address.

## Special Registers

We introduced the following special registers for MIPS:

- `$fp`: frame pointer
- `$sp`: stack pointer
- `$v0`: function return value
- `$ra`: function return address
- `$zero`: zero register


