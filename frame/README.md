# Assignment 4 - Frame Analysis

**Group:**

- Bryce Thuilot
- Khalil Haji

In this assignment, we designed the frame layout for our tiger compiler

**New Files:**

- `frame.sig`
- `mipsframe.sml`
- `findescape.sml`
- `translate.sml`
- `temp.sig`
- `temp.sml`

## Static Link Formal

As suggested in the textbook, we add an extra formal to the head of the list of function formals when creating a new frame. This represents the static link. The textbook suggests not to return this from `Translate.formals`, however, we found it more convenient to handle the extra formal parameter in our `semant` module. If the need arises we can easily address this in a future assignment.

## Function Parameters

We allow more than 4 function parameters. We allow four parameters to be allocated in temps with the rest in the stack frame. For example, if we have five parameters and one of them escapes, the remaining four will be given temps.

In our `semant` module, when we encounter a new function definition, we allocate a new level. We then call `Translate.formals` on the new level in order to get the `access` values we need for our variable environment.

## FindEscape

If we encounter an undeclared variable during our first traversal while determining whether variables escape there is the possibility of encountering an undefnied variable. In this case, we simply ignore it as we will catch this and raise an error in the `semant` module.

## Style

A note on style. We formatted our code using `sml-mode` in emacs. This automatically indents each line to the correct level.
For some reason, some lines appear to be weirdly indented on the github previewer tool and in vscode. For this reason
we recommend using emacs with `sml-mode` to read our source code.
