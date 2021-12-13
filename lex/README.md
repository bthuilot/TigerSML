# Assignment 1 - Lexer 
**Group:**
    - Bryce Thuilot
    - Khalil Haji

This assignment had our group use ML-Lex to create a lexical analyizer for the Tiger programming lanuage. 

## Comments
To handle comments, we had used ML-Lex to find `/*` from the `INITIAL` state to then go to a `COMMENT` state. (tiger.lex line 76). Because comments in the Tiger programming langauge may be nested, we have a global counter `nest_cmt_ct` which counts the level of nested comments the lexer is currently in. It begins at `1` when the state switches from `INITIAL` to `COMMENT` and increases by 1 everytime a new `/*` is encountered, and decremented by 1 everytime a `*/` is encountered. If a `*/` is encoutnered and the `nest_cmt_ct` is decremented to 0, the state is switched back to `INITIAL`.

## End of file (EOF)
To handle EOF we have two checks when `EOF` is reached. First we check the nested comment counter, `nest_cmt_ct`, to see if we are currently in a pasring comment state. If the counter is not zero, we know that there is an unclosed comment and we throw and error. The other check is for the `str_state` variable. This variable is set when the lexer is is the `STRING` state. This variable is set to `true` when an open `"` is found and the state is set to `STRING`, and is set back to `false` when the state is set to `INITIAL` when an unclosed `"` is found. If both `nest_cmt_ct` is zero and `str_state` is false, we know the EOF of the file is valid

## Errors
In our lexer, we make use of Appels error module. We have a 9 places were a lexer error could occur:
1. When parsing a ASCII code in a string is invalid (line 39)
2. When parsing a ASCII control code in a string fails (line 37)
3. When a EOF occurs while in a comment (line 58)
4. When a EOF occurs while in a string (line 59)
5. When a unescaped new line occurs in a string (line 77)
6. An invalid escape character is give (line 88)
7. A non whitespace character is present in a string whitespace escape (line 93)
8. When a comment closing string `*/` occurs without being in a comment (line 96)
9. A catch all case, where if a token that was not expected occurs while lexing in the `INITIAL` state (line 145)


### Helpful resources and links

Some links that we found very helpful in trying to learn both SML and ML-Lex
- [User's guide to ML-Lex and ML-Yacc](http://rogerprice.org/ug/ug.pdf)
- [Learn Standard ML in Y Minutes](https://learnxinyminutes.com/docs/standard-ml/)
- [ML-Lex's Homepage](https://www.smlnj.org/doc/ML-Lex/manual.html)