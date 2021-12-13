structure Main : sig val main : string * string -> unit end =
struct 
structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)
structure Lex = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
structure F : FRAME = MipsFrame
structure TigerP = Join(structure ParserData = TigerLrVals.ParserData
			structure Lex=Lex
			structure LrParser = LrParser)


(* Headers for MIPS .text section, used for function definitions *)
val textHeader  = "\n.text\n\n.globl main\n\n"
(* Data header for MIPS .data section, used for strings*)
val dataHeader = "\n.data\n\n"


(* allocatedTempToStr looks in the provided allocated table to find the temp and returns it *)
fun allocatedTempToStr alloc temp =
    case Temp.Table.look(alloc,temp) of
        SOME(r) => r
      | NONE => raise Color.SpillError

(* canon runs the canonicalizer on the body of the frame *)
fun canon (body) =
    let
	val linear = Canon.linearize body
	val basicBlocks = Canon.basicBlocks linear
    in
	Canon.traceSchedule(basicBlocks)
    end


(* genAssmeblyStr generates a string of the assembly code for a Frag *)
fun genAssemblyStr (Frame.PROC{body, frame}) =
    let
	val canonicalize = canon body
	val inst = List.concat(map (MipsGen.codegen frame) canonicalize)
	val inst' = Frame.procEntryExit2 (frame,inst)
        val (inst'',alloc) = RegAlloc.alloc(inst',frame)
        val {prolog,body,epilog} = Frame.procEntryExit3(frame,inst'')
        val assemFormatter = Assem.format(allocatedTempToStr alloc)
	val assem = foldl (fn (inst, acc) =>  acc ^ (assemFormatter inst)) "" body
    in
	(prolog ^ assem ^ epilog)
    end
  | genAssemblyStr (Frame.STRING(label, str)) = F.string(label, str)

(* Front end handles lexing, parsing, static semantics and frame anaylsis of raw file input
   and returns a list of Frags (IR).
 *)
fun frontEnd (inFile) =
    let
	fun parseerror(s,p1,p2) = ErrorMsg.error p1 s
	val lexer = LrParser.Stream.streamify (Lex.makeLexer
						   (fn _ => TextIO.input inFile))
	val (absyn, _) = TigerP.parse(30,lexer,parseerror,())
    in
	(FindEscape.findEscape(absyn); Semant.transProg(absyn))
    end

(* Helper function to filter strings of frags *)
fun isString (Frame.STRING(_)) = true
  | isString (_) = false 
	
(* Back end handles translation from IR to assembly, liveness analysis and register allocation
 *)    
fun backEnd (frags, outFile) =
    let
	val (strings, procs) = List.partition isString frags
	val procAssems = map genAssemblyStr procs
	val stringAssems = map genAssemblyStr strings
	val runtime = TextIO.input (TextIO.openIn "integration/runtime.s")
	val writeStr = fn str => TextIO.output(outFile, str)
    in
	(writeStr(runtime);
	 writeStr(dataHeader); (* Write .text header *)
	 app writeStr stringAssems; (* Write prodecure assembly code *)
	 writeStr(textHeader); (* Write .data header *)
	 app writeStr procAssems) (* Write string assembly code *)
    end


fun main (inputPath, outputPath) =
    let val _ = (ErrorMsg.reset(); ErrorMsg.fileName := inputPath)
		    
	val inFile = TextIO.openIn inputPath
	val outFile = TextIO.openOut outputPath
				     
	val frags = frontEnd(inFile)
    in
	backEnd(frags, outFile)
    end handle LrParser.ParseError => raise ErrorMsg.Error

end