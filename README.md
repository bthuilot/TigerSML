# TigerSML

A Tiger compiler written in Standard ML from Andrew Appel's 'Modern Compiler Implementation in ML' 

*Written By Bryce Thuilot & Khalil Haji*

## Running

To run the compiler simply give the name of the `.tig` as an argument while running `run.sml` with the `sml` command (optionally add `-o [OUTPUT FILE]` where `[OUTPUT FILE]` is the name of the output file to write to, otherwise `a.out` will be used)

**i.e.** to the compile the file `factorial.tig`

```shell
sml run.sml factorial.tig -o factorial.s
```

## Directory Structure

Each directory will contain the additional files that were added for each project throughout the semester, in addition to a `README.md` file describing the changes we made for that project

- **`lex`**: Code for lexing

- **`parse`**: Code for parsing

- **`semantics`**: Code for static semantics

- **`frame`**: Code for frame analysis

- **`ir`**: Code for IR generation

- **`inst_select`**: Code for instruction selection

- **`liveness`**: Code for liveness analysis

- **`register`**: Code for register selection

- **`integration`**: Code provided to assist in linking runtime and translation to machine code

- **`util`**: Utility files used across multiple projects
