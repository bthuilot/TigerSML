# Assignment 5 - IR Generation

**Group:**

- Bryce Thuilot
- Khalil Haji

In this assignment, we designed the IR Generator for our compiler

**New Files:**

- `tree.sml`
- `printtree.sml`

## Changes to New Frames for functions

We had to change how we were parsing functions in `transFunDecs`. We were putting function headers on a new level with the body which caused "function undefined" errors.
We switched to putting function headers on the same level they are defined and increasing the body level by one to fix the issuesr

## If/Else generation

For If/Else generation, we had done a lot of refactoring and design to handle the different cases where the else is Cx, Ex or Nx. This resulted in code that we feel is efficient when handling if else's when the else case has a Cx

## For loops

The text book had suggested we translate For loops into while loops to reduce the amount of repeated code, However we found this approach cause more issues for us. In partiuclar to type check the body the of the For we had to run it through `transExp` twice which caused us to allocatae certain fragments twice so we decided to implement for loops as its own thing

## Printing Frags

We were provided with code to print trees, however, `transProg` returns a list of frags. In order to debug our code, we wrote a print frags function in the Frame module. This will print the tree for each `PROC` and print the string for each `STRING`.
