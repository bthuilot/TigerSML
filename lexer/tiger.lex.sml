functor TigerLexFun(structure Tokens: Tiger_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
STRING | STRWHITESPACE | COMMENT | STRESCAPE | INITIAL
    structure UserDeclarations = 
      struct

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



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      (newLine(yypos); continue()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN STRING; str_state := true; str_buf := ""; init_str_pos := yypos; continue()))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; str_state := false; Tokens.STRING(!str_buf, !init_str_pos, yypos+1)))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (newLine(yypos); err yypos ("Unescaped newline in string"); continue()))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN STRESCAPE; continue()))
fun yyAction5 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (str_buf := !str_buf ^ yytext; continue())
      end
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN STRWHITESPACE; continue()))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (newLine(yypos); YYBEGIN STRWHITESPACE; continue()))
fun yyAction8 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (parseCharCode(yytext, yypos); YYBEGIN STRING; continue())
      end
fun yyAction9 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (parseControlCode(yytext, yypos); YYBEGIN STRING; continue())
      end
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (str_buf := !str_buf ^ "\n"; YYBEGIN STRING; continue()))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (str_buf := !str_buf ^ "\t"; YYBEGIN STRING; continue()))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (str_buf := !str_buf ^ "\""; YYBEGIN STRING; continue()))
fun yyAction13 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (err yypos ("Invalid escape character: " ^ yytext); YYBEGIN STRING; continue())
      end
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (newLine(yypos); continue()))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN STRING; continue()))
fun yyAction17 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (err yypos ("Invalid character in string whitespace esacpe: " ^ yytext); continue())
      end
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN COMMENT; nest_cmt_ct := 1; continue()))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (err yypos ("Found comment closing tag found outside of comment"); continue()))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (nest_cmt_ct := !nest_cmt_ct + 1; continue()))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      ((if (!nest_cmt_ct=1) then (YYBEGIN INITIAL; nest_cmt_ct := 0) else (nest_cmt_ct := !nest_cmt_ct - 1)); continue()))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (newLine(yypos); continue()))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COMMA(yypos,yypos+1)))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COLON(yypos, yypos+1)))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SEMICOLON(yypos, yypos+1)))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LPAREN(yypos, yypos+1)))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RPAREN(yypos, yypos+1)))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBRACK(yypos, yypos+1)))
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBRACK(yypos, yypos+1)))
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBRACE(yypos, yypos+1)))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBRACE(yypos, yypos+1)))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DOT(yypos, yypos+1)))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.EQ(yypos,yypos+1)))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PLUS(yypos, yypos+1)))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MINUS(yypos, yypos+1)))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TIMES(yypos, yypos+1)))
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DIVIDE(yypos, yypos+1)))
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GT(yypos, yypos+1)))
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LT(yypos, yypos+1)))
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GE(yypos, yypos+2)))
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LE(yypos, yypos+2)))
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NEQ(yypos, yypos+2)))
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.AND(yypos, yypos+1)))
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.OR(yypos, yypos+1)))
fun yyAction46 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ASSIGN(yypos, yypos+2)))
fun yyAction47 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TYPE(yypos,yypos+4)))
fun yyAction48 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.VAR(yypos,yypos+3)))
fun yyAction49 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.FUNCTION(yypos,yypos+8)))
fun yyAction50 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.BREAK(yypos,yypos+5)))
fun yyAction51 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.OF(yypos,yypos+2)))
fun yyAction52 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.END(yypos,yypos+3)))
fun yyAction53 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.IN(yypos, yypos+2)))
fun yyAction54 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NIL(yypos, yypos+3)))
fun yyAction55 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LET(yypos,yypos+3)))
fun yyAction56 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DO(yypos, yypos+2)))
fun yyAction57 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TO(yypos, yypos+2)))
fun yyAction58 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.FOR(yypos, yypos+3)))
fun yyAction59 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.WHILE(yypos, yypos+5)))
fun yyAction60 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ELSE(yypos, yypos+4)))
fun yyAction61 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.THEN(yypos, yypos+4)))
fun yyAction62 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.IF(yypos, yypos+2)))
fun yyAction63 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ARRAY(yypos,yypos+5)))
fun yyAction64 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ID(yytext,yypos,yypos+size yytext))
      end
fun yyAction65 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (
  let
    val i = Option.getOpt(Int.fromString(yytext), 0)
  in 
     Tokens.INT(i, yypos,yypos+size(yytext))
 end)
      end
fun yyAction66 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction67 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (err yypos ("illegal character: " ^ yytext); continue())
      end
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp = #"`"
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction59(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction59(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction59(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction59(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction59(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
            else if inp = #"`"
              then yyAction59(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ71(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                  else yyAction59(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
              else yyAction59(strm, yyNO_MATCH)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ75(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ74(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ73(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"h"
              then yyQ72(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"h"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction48(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction48(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction48(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction48(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction48(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
            else if inp = #"`"
              then yyAction48(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ71(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                  else yyAction48(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
              else yyAction48(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ77(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ76(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"`"
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ71(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ82(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ81(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction57(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction57(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction57(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction57(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction57(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
            else if inp = #"`"
              then yyAction57(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ71(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                  else yyAction57(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
              else yyAction57(strm, yyNO_MATCH)
      (* end case *))
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction61(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction61(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction61(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction61(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction61(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp = #"`"
              then yyAction61(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ71(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                  else yyAction61(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
              else yyAction61(strm, yyNO_MATCH)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ84(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ83(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction64(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyAction64(strm, yyNO_MATCH)
                else if inp = #"_"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"_"
                  then if inp <= #"Z"
                      then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyAction64(strm, yyNO_MATCH)
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"i"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"i"
                  then if inp = #"h"
                      then yyQ78(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"o"
                  then yyQ79(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp = #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"z"
              then if inp = #"y"
                  then yyQ80(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ85 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction51(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction51(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction51(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction51(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp = #"`"
              then yyAction51(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ71(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyAction51(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
              else yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ85(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ87 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction54(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction54(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction54(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction54(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
            else if inp = #"`"
              then yyAction54(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ71(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
                  else yyAction54(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
              else yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ86 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ87(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ86(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ89 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction55(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction55(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction55(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction55(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction55(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
            else if inp = #"`"
              then yyAction55(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ71(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                  else yyAction55(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
              else yyAction55(strm, yyNO_MATCH)
      (* end case *))
fun yyQ88 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ89(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ88(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ91 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction53(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"`"
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ71(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ90 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction62(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction62(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction62(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction62(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction62(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
            else if inp = #"`"
              then yyAction62(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ71(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                  else yyAction62(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
              else yyAction62(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction64(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyAction64(strm, yyNO_MATCH)
                else if inp = #"["
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ91(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"f"
                  then yyQ90(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ99 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction49(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction49(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction49(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction49(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp = #"`"
              then yyAction49(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ71(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                  else yyAction49(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
              else yyAction49(strm, yyNO_MATCH)
      (* end case *))
fun yyQ98 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ99(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ97 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ98(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ96 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ97(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ95 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ96(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ94 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"c"
              then yyQ95(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"c"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ93 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ94(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ100 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction58(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction58(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction58(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction58(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction58, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction58(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction58, yyNO_MATCH))
            else if inp = #"`"
              then yyAction58(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ71(strm', yyMATCH(strm, yyAction58, yyNO_MATCH))
                  else yyAction58(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction58, yyNO_MATCH))
              else yyAction58(strm, yyNO_MATCH)
      (* end case *))
fun yyQ92 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ100(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction64(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyAction64(strm, yyNO_MATCH)
                else if inp = #"["
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ93(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"o"
                  then yyQ92(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ103 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction52(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction52(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction52(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction52(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction52(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
            else if inp = #"`"
              then yyAction52(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ71(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
                  else yyAction52(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
              else yyAction52(strm, yyNO_MATCH)
      (* end case *))
fun yyQ102 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"d"
              then yyQ103(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"d"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ105 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction60(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction60(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction60(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction60(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction60(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
            else if inp = #"`"
              then yyAction60(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ71(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
                  else yyAction60(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
              else yyAction60(strm, yyNO_MATCH)
      (* end case *))
fun yyQ104 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ105(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ101 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ104(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction64(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyAction64(strm, yyNO_MATCH)
                else if inp = #"["
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ102(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"l"
                  then yyQ101(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ106 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction56(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction56(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction56(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction56(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp = #"`"
              then yyAction56(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ71(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                  else yyAction56(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
              else yyAction56(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ106(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ110 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction50(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction50(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction50(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp = #"`"
              then yyAction50(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ71(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ109 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"k"
              then yyQ110(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"k"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ108 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ109(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ107 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ108(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ107(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ114 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction63(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction63(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction63(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction63(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction63(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
            else if inp = #"`"
              then yyAction63(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ71(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else yyAction63(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
              else yyAction63(strm, yyNO_MATCH)
      (* end case *))
fun yyQ113 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"y"
              then yyQ114(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"y"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp = #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ112 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ113(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ111 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ112(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ111(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ115 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction66(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"!"
              then yyAction66(strm, yyNO_MATCH)
            else if inp < #"!"
              then if inp = #"\n"
                  then yyAction66(strm, yyNO_MATCH)
                else if inp < #"\n"
                  then if inp <= #"\a"
                      then yyAction66(strm, yyNO_MATCH)
                      else yyQ115(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                else if inp = #" "
                  then yyQ115(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                  else yyAction66(strm, yyNO_MATCH)
            else if inp = #"M"
              then yyAction66(strm, yyNO_MATCH)
            else if inp < #"M"
              then if inp = #"L"
                  then yyQ115(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                  else yyAction66(strm, yyNO_MATCH)
            else if inp = #"^"
              then yyQ115(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
              else yyAction66(strm, yyNO_MATCH)
      (* end case *))
fun yyQ116 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"L"
              then yyQ116(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"L"
              then if inp = #"!"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #"!"
                  then if inp = #"\n"
                      then yyAction64(strm, yyNO_MATCH)
                    else if inp < #"\n"
                      then if inp <= #"\a"
                          then yyAction64(strm, yyNO_MATCH)
                          else yyQ115(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                    else if inp = #" "
                      then yyQ115(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyAction64(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"["
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"^"
                  then yyQ115(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"a"
              then yyAction64(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"L"
              then yyQ116(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"L"
              then if inp = #"!"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #"!"
                  then if inp = #"\n"
                      then yyAction64(strm, yyNO_MATCH)
                    else if inp < #"\n"
                      then if inp <= #"\a"
                          then yyAction64(strm, yyNO_MATCH)
                          else yyQ115(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                    else if inp = #" "
                      then yyQ115(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyAction64(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp = #"_"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"["
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"^"
                  then yyQ115(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"a"
              then yyAction64(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction64(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp = #"`"
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ117 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ117(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ119 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ118 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ119(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
            else if inp < #">"
              then if inp = #"="
                  then yyQ118(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                  else yyAction40(strm, yyNO_MATCH)
              else yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ120 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ120(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
              else yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ121 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction65(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ121(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
            else if inp < #"0"
              then yyAction65(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ121(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
              else yyAction65(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction65(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ121(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
            else if inp < #"0"
              then yyAction65(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ121(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
              else yyAction65(strm, yyNO_MATCH)
      (* end case *))
fun yyQ122 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ122(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ123 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyQ123(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction66(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"!"
              then yyAction66(strm, yyNO_MATCH)
            else if inp < #"!"
              then if inp = #"\n"
                  then yyAction66(strm, yyNO_MATCH)
                else if inp < #"\n"
                  then if inp <= #"\a"
                      then yyAction66(strm, yyNO_MATCH)
                      else yyQ115(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                else if inp = #" "
                  then yyQ115(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                  else yyAction66(strm, yyNO_MATCH)
            else if inp = #"M"
              then yyAction66(strm, yyNO_MATCH)
            else if inp < #"M"
              then if inp = #"L"
                  then yyQ115(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                  else yyAction66(strm, yyNO_MATCH)
            else if inp = #"^"
              then yyQ115(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
              else yyAction66(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction67(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction67(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyQ54(strm', lastMatch)
            else if inp < #"["
              then if inp = #","
                  then yyQ42(strm', lastMatch)
                else if inp < #","
                  then if inp = #"#"
                      then yyQ33(strm', lastMatch)
                    else if inp < #"#"
                      then if inp = #"\v"
                          then yyQ33(strm', lastMatch)
                        else if inp < #"\v"
                          then if inp = #"\b"
                              then yyQ34(strm', lastMatch)
                            else if inp < #"\b"
                              then yyQ33(strm', lastMatch)
                            else if inp = #"\n"
                              then yyQ35(strm', lastMatch)
                              else yyQ34(strm', lastMatch)
                        else if inp = #"!"
                          then yyQ33(strm', lastMatch)
                        else if inp < #"!"
                          then if inp = #" "
                              then yyQ34(strm', lastMatch)
                              else yyQ33(strm', lastMatch)
                          else yyQ36(strm', lastMatch)
                    else if inp = #"("
                      then yyQ38(strm', lastMatch)
                    else if inp < #"("
                      then if inp = #"&"
                          then yyQ37(strm', lastMatch)
                          else yyQ33(strm', lastMatch)
                    else if inp = #"*"
                      then yyQ40(strm', lastMatch)
                    else if inp = #")"
                      then yyQ39(strm', lastMatch)
                      else yyQ41(strm', lastMatch)
                else if inp = #"<"
                  then yyQ49(strm', lastMatch)
                else if inp < #"<"
                  then if inp = #"0"
                      then yyQ46(strm', lastMatch)
                    else if inp < #"0"
                      then if inp = #"."
                          then yyQ44(strm', lastMatch)
                        else if inp = #"-"
                          then yyQ43(strm', lastMatch)
                          else yyQ45(strm', lastMatch)
                    else if inp = #":"
                      then yyQ47(strm', lastMatch)
                    else if inp = #";"
                      then yyQ48(strm', lastMatch)
                      else yyQ46(strm', lastMatch)
                else if inp = #"A"
                  then yyQ52(strm', lastMatch)
                else if inp < #"A"
                  then if inp = #">"
                      then yyQ51(strm', lastMatch)
                    else if inp = #"="
                      then yyQ50(strm', lastMatch)
                      else yyQ33(strm', lastMatch)
                else if inp = #"L"
                  then yyQ53(strm', lastMatch)
                  else yyQ52(strm', lastMatch)
            else if inp = #"l"
              then yyQ62(strm', lastMatch)
            else if inp < #"l"
              then if inp = #"c"
                  then yyQ52(strm', lastMatch)
                else if inp < #"c"
                  then if inp = #"_"
                      then yyQ33(strm', lastMatch)
                    else if inp < #"_"
                      then if inp = #"]"
                          then yyQ55(strm', lastMatch)
                        else if inp = #"\\"
                          then yyQ33(strm', lastMatch)
                          else yyQ34(strm', lastMatch)
                    else if inp = #"a"
                      then yyQ56(strm', lastMatch)
                    else if inp = #"b"
                      then yyQ57(strm', lastMatch)
                      else yyQ33(strm', lastMatch)
                else if inp = #"g"
                  then yyQ52(strm', lastMatch)
                else if inp < #"g"
                  then if inp = #"e"
                      then yyQ59(strm', lastMatch)
                    else if inp = #"d"
                      then yyQ58(strm', lastMatch)
                      else yyQ60(strm', lastMatch)
                else if inp = #"i"
                  then yyQ61(strm', lastMatch)
                  else yyQ52(strm', lastMatch)
            else if inp = #"v"
              then yyQ66(strm', lastMatch)
            else if inp < #"v"
              then if inp = #"p"
                  then yyQ52(strm', lastMatch)
                else if inp < #"p"
                  then if inp = #"n"
                      then yyQ63(strm', lastMatch)
                    else if inp = #"m"
                      then yyQ52(strm', lastMatch)
                      else yyQ64(strm', lastMatch)
                else if inp = #"t"
                  then yyQ65(strm', lastMatch)
                  else yyQ52(strm', lastMatch)
            else if inp = #"|"
              then yyQ69(strm', lastMatch)
            else if inp < #"|"
              then if inp = #"x"
                  then yyQ52(strm', lastMatch)
                else if inp < #"x"
                  then yyQ67(strm', lastMatch)
                else if inp = #"{"
                  then yyQ68(strm', lastMatch)
                  else yyQ52(strm', lastMatch)
            else if inp = #"}"
              then yyQ70(strm', lastMatch)
              else yyQ33(strm', lastMatch)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"!"
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"!"
              then if inp = #"\n"
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < #"\n"
                  then if inp <= #"\a"
                      then yyAction6(strm, yyNO_MATCH)
                      else yyQ30(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp = #" "
                  then yyQ30(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp = #"M"
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"M"
              then if inp = #"L"
                  then yyQ30(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp = #"^"
              then yyQ30(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"!"
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"!"
              then if inp = #"\n"
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < #"\n"
                  then if inp <= #"\a"
                      then yyAction6(strm, yyNO_MATCH)
                      else yyQ30(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp = #" "
                  then yyQ30(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp = #"M"
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"M"
              then if inp = #"L"
                  then yyQ30(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp = #"^"
              then yyQ30(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"!"
              then yyQ28(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp < #"!"
              then if inp = #"\n"
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < #"\n"
                  then if inp <= #"\a"
                      then yyQ28(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                      else yyQ29(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp = #" "
                  then yyQ29(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyQ28(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp = #"M"
              then yyQ28(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp < #"M"
              then if inp = #"L"
                  then yyQ29(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyQ28(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp = #"^"
              then yyQ29(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyQ28(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ32(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ32(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ31(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
            else if inp < #"0"
              then yyAction13(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ31(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"!"
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"!"
              then if inp = #"\n"
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < #"\n"
                  then if inp <= #"\a"
                      then yyAction6(strm, yyNO_MATCH)
                      else yyQ30(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp = #" "
                  then yyQ30(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp = #"M"
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"M"
              then if inp = #"L"
                  then yyQ30(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp = #"^"
              then yyQ30(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyQ20(strm', lastMatch)
            else if inp < #":"
              then if inp = #" "
                  then yyQ21(strm', lastMatch)
                else if inp < #" "
                  then if inp = #"\n"
                      then yyQ22(strm', lastMatch)
                    else if inp < #"\n"
                      then if inp <= #"\a"
                          then yyQ20(strm', lastMatch)
                          else yyQ21(strm', lastMatch)
                      else yyQ20(strm', lastMatch)
                else if inp = #"#"
                  then yyQ20(strm', lastMatch)
                else if inp < #"#"
                  then if inp = #"!"
                      then yyQ20(strm', lastMatch)
                      else yyQ23(strm', lastMatch)
                else if inp <= #"/"
                  then yyQ20(strm', lastMatch)
                  else yyQ24(strm', lastMatch)
            else if inp = #"_"
              then yyQ20(strm', lastMatch)
            else if inp < #"_"
              then if inp = #"M"
                  then yyQ20(strm', lastMatch)
                else if inp < #"M"
                  then if inp = #"L"
                      then yyQ21(strm', lastMatch)
                      else yyQ20(strm', lastMatch)
                else if inp = #"^"
                  then yyQ25(strm', lastMatch)
                  else yyQ20(strm', lastMatch)
            else if inp = #"o"
              then yyQ20(strm', lastMatch)
            else if inp < #"o"
              then if inp = #"n"
                  then yyQ26(strm', lastMatch)
                  else yyQ20(strm', lastMatch)
            else if inp = #"t"
              then yyQ27(strm', lastMatch)
              else yyQ20(strm', lastMatch)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ18(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyQ19(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ16(strm', lastMatch)
            else if inp < #"*"
              then if inp = #"\n"
                  then yyQ15(strm', lastMatch)
                  else yyQ14(strm', lastMatch)
            else if inp = #"/"
              then yyQ17(strm', lastMatch)
              else yyQ14(strm', lastMatch)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"!"
              then yyAction15(strm, yyNO_MATCH)
            else if inp < #"!"
              then if inp = #"\n"
                  then yyAction15(strm, yyNO_MATCH)
                else if inp < #"\n"
                  then if inp <= #"\a"
                      then yyAction15(strm, yyNO_MATCH)
                      else yyQ13(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp = #" "
                  then yyQ13(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
            else if inp = #"M"
              then yyAction15(strm, yyNO_MATCH)
            else if inp < #"M"
              then if inp = #"L"
                  then yyQ13(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
            else if inp = #"^"
              then yyQ13(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
              else yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"!"
              then yyAction15(strm, yyNO_MATCH)
            else if inp < #"!"
              then if inp = #"\n"
                  then yyAction15(strm, yyNO_MATCH)
                else if inp < #"\n"
                  then if inp <= #"\a"
                      then yyAction15(strm, yyNO_MATCH)
                      else yyQ13(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp = #" "
                  then yyQ13(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
            else if inp = #"M"
              then yyAction15(strm, yyNO_MATCH)
            else if inp < #"M"
              then if inp = #"L"
                  then yyQ13(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
            else if inp = #"^"
              then yyQ13(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
              else yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"L"
              then yyQ10(strm', lastMatch)
            else if inp < #"L"
              then if inp = #"\v"
                  then yyQ9(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\b"
                      then yyQ10(strm', lastMatch)
                    else if inp < #"\b"
                      then yyQ9(strm', lastMatch)
                    else if inp = #"\n"
                      then yyQ11(strm', lastMatch)
                      else yyQ10(strm', lastMatch)
                else if inp = #" "
                  then yyQ10(strm', lastMatch)
                  else yyQ9(strm', lastMatch)
            else if inp = #"]"
              then yyQ9(strm', lastMatch)
            else if inp < #"]"
              then if inp = #"\\"
                  then yyQ12(strm', lastMatch)
                  else yyQ9(strm', lastMatch)
            else if inp = #"^"
              then yyQ10(strm', lastMatch)
              else yyQ9(strm', lastMatch)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyQ7(strm', lastMatch)
            else if inp < #"\""
              then if inp = #"\n"
                  then yyQ6(strm', lastMatch)
                  else yyQ5(strm', lastMatch)
            else if inp = #"\\"
              then yyQ8(strm', lastMatch)
              else yyQ5(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of STRING => yyQ0(!(yystrm), yyNO_MATCH)
    | STRWHITESPACE => yyQ1(!(yystrm), yyNO_MATCH)
    | COMMENT => yyQ2(!(yystrm), yyNO_MATCH)
    | STRESCAPE => yyQ3(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ4(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    end

  end
