# Assignment 3 - Static Semantics

**Group:**
    - Bryce Thuilot
    - Khalil Haji

This assignment had our group had to write a static type checker for Tiger in SML

**New files:**
    - `semant.sml`
    - `env.sml`
    - `types.sml`
    - `translate.sml`

There were a few areas of interest in our code base:

## Recursive types

When coming across header entries we add a value of `T.NAME(name, ref NONE)` entry for the declared type to the environment and after the bodies of the types are added to the environment using the values added, we iterate over the declarations and replace the NONE with a ref to the type value.

We then perform some additional iterations to ensure the block is free of cycles and that the type names are unique. We also
check that we can access the actual type of each entry which ensures that all types are resolved in the type table.

## Breaks

For breaks, every time we encounter a while loop or for loop we set increase a variable that is passed to all function indicating that break statements are valid in this level.

## For Counter

To fulfill the restriction on assigning to the for counter, we introduced a second entry type in the variable environment called 
`CounterEntry` which represents a counter variable. This is essentially a read-only variable. We error if an assignment statement
tries to assign to such a variable.

## Style

A note on style. We formatted our code using `sml-mode` in emacs. This automatically indents each line to the correct level.
For some reason, some lines appear to be weirdly indented on the github previewer tool and in vscode. For this reason
we recommend using emacs with `sml-mode` to read our source code.

## Exceptions

We used the provided error library to print errors while type checking. In most cases, we determined that it was safe and
possibly useful to continue type checking after printing an error. For this reason, we return `dummy` after printing errors
in a number of places. `dummy` satisfies the return type of `transExp` and uses `UNIT` as its type. While the return type of an
erroneous expression is not actually `UNIT`, we believe that the printing of the error message is enough to signal to the user
that there is an error. We do raise an exception when a cycle is detected in a block of type declarations to avoid looping
forever.

## Testing

The `run.sml` driver calls our `main` module which in turn lexes, parses, and type checks the input program. The `run` driver
will test `merge.tig`, `queen.tig`, and all the files named `test*.tig` or `error*.tig`. Files whose names begin with error
should produce an error when type checked.

## SML and the logic in general 

As a more general note on the project as a whole, this was both of our first times writing significant code
in SML but we had both written pretty similar code (conceptually) to this in Programming Languages (CS4400) so it wasn't as hard as we went in believing it would be since we already had a grasp on the general flow of a program like this
