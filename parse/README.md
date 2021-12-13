# Assignment 1 - Lexer 
**Group:**
    - Bryce Thuilot
    - Khalil Haji

This assignment had our group use ML-Yacc to create a parser for the Tiger programming lanuage. There were a few notable
precedence rules we had to define to avoid shift/reduce conflicts. They are outlined below:

## Dangling Else
The dangling else conflict arises when you have an expression such as `if exp then if exp then exp else exp`.
The default action of the parser is to shift which includes the `else` in the inner `if`. As this is the expected behavior
we were able to supress the shift/reduce error by assigning the `ELSE` token a higher precedence than `THEN`.

## Declarations
For function and type declarations we encountered a conflict where the parser could either reduce a list of function declarations
or shift another function declaration. The correct action here is to shift another function declaration to build the largest
possible list of contiguous function declarations. The same issue is present for type declarations. To solve this we assigned
the `FUNCTION` and `TYPE` tokens higher precedence than the rules reduce function and type declaration lists. We introduced a
`LOWEST` auxillary non-terminal to represent the lowest possible precedence for our parser. This is only used for type 
and function declaration lists.

## Left Brackets
An `ID` followed by a left bracket can either be `ID[exp]` or `ID[exp] OF exp`. Naturally, left bracket must be given
precendence over `ID` in order to shift after reading an `ID`. However, we also needed to add a redundant rule that makes
explicit the `ID [exp]` case. Without this, the parser must first reduce an `ID` to an `lvalue` in order to reach that case.
However, reducing to an `lvalue` means ruling out `ID[exp] OF exp` which we cannot do at this point!

## DO
We needed to give `DO` an explicit precedence that is lower than the operators such that we can completely read in an expression
before continuing with the `DO` and subsequent expression of a while expression.