type svalue = Tokens.svalue
type pos = int
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

(* Error Handling *)
(* -------------- *)

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val err = ErrorMsg.error


(* Line num parsing *)
(* ---------------- *)

fun newLine(yypos: int) = (lineNum := (!lineNum)+1; linePos := yypos :: !linePos)

(* Comment parsing *)
(* -------------- *)

(* Nested comment counter *)
val nest_cmt_ct = ref 0


(* String parsing *)
(* -------------- *)

(* String Buffer And Initial position for reading in strings *)
val str_buf = ref ""
val init_str_pos = ref 0
val str_state = ref false

(* Function to parse ASCII character code i.e. \010 *)
fun parseCharCode(digit_str : string, yypos: int) = 
  let
    val chr = Char.fromString("\\" ^ digit_str)
  in
    case chr of
      SOME c => str_buf := !str_buf ^ Char.toString(c)
    | NONE => err yypos ("Invalid ASCII Character code: \\" ^ digit_str)
  end
(* Function to parse ASCII control code i.e. \^J *)
fun parseControlCode(control_str: string, yypos: int) =
  let 
    val control_chr = Char.fromString("\\" ^control_str)
  in
    case control_chr of
      NONE  => err yypos ("Invalid ASCII Control Sequence: \\" ^ control_str)
    | SOME c => str_buf := !str_buf ^ Char.toString(c)
  end

(* EOF parsing *)
(* -------------- *)

fun eof() = 
  let 
    val pos = hd(!linePos)
  in 
    if !nest_cmt_ct > 0 then  (err pos ("EOF occurred while in a comment, missing */"))
    else if !str_state then (err pos ("EOF occurred while in a string, missing \"")) else ();
    Tokens.EOF(pos,pos)
  end

%% 
%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));

digits=[0-9]+;
id=[a-zA-Z][_0-9a-zA-Z]*;
whitespace=[\ \t\b\^L]+;

%s COMMENT STRING STRESCAPE STRWHITESPACE;
%%

<INITIAL> \n	=> (newLine(yypos); continue());


<INITIAL> \" =>  (YYBEGIN STRING; str_state := true; str_buf := ""; init_str_pos := yypos; continue());
<STRING> \" => (YYBEGIN INITIAL; str_state := false; Tokens.STRING(!str_buf, !init_str_pos, yypos+1));
<STRING> \n => (newLine(yypos); err yypos ("Unescaped newline in string"); continue());
<STRING> \\ => (YYBEGIN STRESCAPE; continue());
<STRING> . =>  (str_buf := !str_buf ^ yytext; continue());

<STRESCAPE> {whitespace} => (YYBEGIN STRWHITESPACE; continue());
<STRESCAPE> \n => (newLine(yypos); YYBEGIN STRWHITESPACE; continue());
<STRESCAPE> [0-9]{3} => (parseCharCode(yytext, yypos); YYBEGIN STRING; continue());
<STRESCAPE> "\^". => (parseControlCode(yytext, yypos); YYBEGIN STRING; continue());
<STRESCAPE> n =>  (str_buf := !str_buf ^ "\n"; YYBEGIN STRING; continue());
<STRESCAPE> t =>  (str_buf := !str_buf ^ "\t"; YYBEGIN STRING; continue());
<STRESCAPE> \" => (str_buf := !str_buf ^ "\""; YYBEGIN STRING; continue());
<STRESCAPE> . => (err yypos ("Invalid escape character: " ^ yytext); YYBEGIN STRING; continue());

<STRWHITESPACE> \n => (newLine(yypos); continue());
<STRWHITESPACE> {whitespace} => (continue());
<STRWHITESPACE> \\ => (YYBEGIN STRING; continue());
<STRWHITESPACE> . => (err yypos ("Invalid character in string whitespace esacpe: " ^ yytext); continue());
    
<INITIAL> "/*" => (YYBEGIN COMMENT; nest_cmt_ct := 1; continue());
<INITIAL> "*/" => (err yypos ("Found comment closing tag found outside of comment"); continue());
<COMMENT> "/*" => (nest_cmt_ct := !nest_cmt_ct + 1; continue());
<COMMENT> "*/" => ((if (!nest_cmt_ct=1) then (YYBEGIN INITIAL; nest_cmt_ct := 0) else (nest_cmt_ct := !nest_cmt_ct - 1)); continue());
<COMMENT> \n => (newLine(yypos); continue());
<COMMENT> . => (continue());

<INITIAL> ","	=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL> ":"  => (Tokens.COLON(yypos, yypos+1));
<INITIAL> ";"  => (Tokens.SEMICOLON(yypos, yypos+1));
<INITIAL> "("    => (Tokens.LPAREN(yypos, yypos+1));
<INITIAL> ")"    => (Tokens.RPAREN(yypos, yypos+1));
<INITIAL>"["    => (Tokens.LBRACK(yypos, yypos+1));
<INITIAL> "]"    => (Tokens.RBRACK(yypos, yypos+1));
<INITIAL> "{"    => (Tokens.LBRACE(yypos, yypos+1));
<INITIAL> "}"    => (Tokens.RBRACE(yypos, yypos+1));
<INITIAL> "."    => (Tokens.DOT(yypos, yypos+1));

<INITIAL> "="    => (Tokens.EQ(yypos,yypos+1));
<INITIAL> "+"    => (Tokens.PLUS(yypos, yypos+1));
<INITIAL> "-"    => (Tokens.MINUS(yypos, yypos+1));
<INITIAL> "*"    => (Tokens.TIMES(yypos, yypos+1));
<INITIAL> "/"    => (Tokens.DIVIDE(yypos, yypos+1));
<INITIAL> ">"    => (Tokens.GT(yypos, yypos+1));
<INITIAL> "<"    => (Tokens.LT(yypos, yypos+1));
<INITIAL> ">="    => (Tokens.GE(yypos, yypos+2));
<INITIAL> "<="    => (Tokens.LE(yypos, yypos+2));
<INITIAL> "<>"    => (Tokens.NEQ(yypos, yypos+2));
<INITIAL> "&"     => (Tokens.AND(yypos, yypos+1));
<INITIAL> "|"     => (Tokens.OR(yypos, yypos+1));
<INITIAL> ":="    => (Tokens.ASSIGN(yypos, yypos+2));

<INITIAL> type => (Tokens.TYPE(yypos,yypos+4));
<INITIAL> var  	=> (Tokens.VAR(yypos,yypos+3));
<INITIAL> function => (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL> break => (Tokens.BREAK(yypos,yypos+5));
<INITIAL> of => (Tokens.OF(yypos,yypos+2));
<INITIAL> end => (Tokens.END(yypos,yypos+3));
<INITIAL> in => (Tokens.IN(yypos, yypos+2));
<INITIAL> nil => (Tokens.NIL(yypos, yypos+3));
<INITIAL> let => (Tokens.LET(yypos,yypos+3));
<INITIAL> do => (Tokens.DO(yypos, yypos+2));
<INITIAL> to => (Tokens.TO(yypos, yypos+2));
<INITIAL> for => (Tokens.FOR(yypos, yypos+3));
<INITIAL> while => (Tokens.WHILE(yypos, yypos+5));
<INITIAL> else => (Tokens.ELSE(yypos, yypos+4));
<INITIAL> then => (Tokens.THEN(yypos, yypos+4));
<INITIAL> if  => (Tokens.IF(yypos, yypos+2));
<INITIAL> array => (Tokens.ARRAY(yypos,yypos+5));

<INITIAL> {id} => (Tokens.ID(yytext,yypos,yypos+size yytext));
<INITIAL> {digits}	=> (
  let
    val i = Option.getOpt(Int.fromString(yytext), 0)
  in 
     Tokens.INT(i, yypos,yypos+size(yytext))
 end);
<INITIAL> {whitespace} => (continue());
<INITIAL> . => (err yypos ("illegal character: " ^ yytext); continue());
