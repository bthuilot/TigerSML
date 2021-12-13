structure MipsFrame : FRAME =
struct
structure A = Assem
datatype access = InFrame of int 
                | InReg of Temp.temp

type frame = {name: Temp.label, formals: access list, locals: access list ref, localsInFrame: int ref}

type register = string


val wordsize = 4
		   
val FP = Temp.newtemp()
val SP = Temp.newtemp()
val RV = Temp.newtemp()
val RA = Temp.newtemp()
val ZERO = Temp.newtemp()

val specialregisters = [
    ("$fp", FP),
    ("$sp", SP),
    ("$v0", RV),
    ("$ra", RA),
    ("$zero", ZERO)
]

val argregs = List.tabulate(4,
			    (fn i => ("$a" ^ Int.toString(i), Temp.newtemp()))
			   )
val callersaves = List.tabulate(10,
				(fn i => ("$t" ^ Int.toString(i), Temp.newtemp()))
			       )
val calleesaves = List.tabulate(8,
				(fn i => ("$s" ^ Int.toString(i), Temp.newtemp()))
			       )
val registers = map #1 (argregs@callersaves @ calleesaves @ specialregisters)
val temps = map #2 (argregs@callersaves@calleesaves @ specialregisters)

val tempMap =
    let
	val table = Temp.Table.empty
    in
	foldl
	    (fn ((regName, regTemp), t) => Temp.Table.enter(t, regTemp, regName))
	    table
	    (argregs @ callersaves @ calleesaves@ specialregisters)
    end
fun tempToStr (temp) = case Temp.Table.look(tempMap, temp) of
			   SOME(name) => name
			 | NONE => Temp.makestring(temp)
						  

datatype frag = PROC of {body: Tree.stm, frame: frame}
              | STRING of Temp.label * string

fun generateLocalAccess(escape, localsInFrame) =
    if escape
    then let val res = InFrame (!localsInFrame * wordsize)
         in
             (localsInFrame := !localsInFrame + 1; res)
         end
    else InReg(Temp.newtemp())

fun generateFrameAccess([], availableReg, frameOffset) = []
  | generateFrameAccess(escape :: rst, availableReg, frameOffset) =
    if (not escape andalso availableReg > 0)
    then InReg(Temp.newtemp()) :: generateFrameAccess(rst, availableReg - 1, frameOffset)
    else InFrame (frameOffset * wordsize) :: generateFrameAccess(rst, availableReg, frameOffset+1)

    
fun newFrame {name : Temp.label, formals} : frame =
    let
	val formals' = generateFrameAccess(formals, 4, 0)
    in
	{name=name, formals=formals', locals= ref [], localsInFrame=ref 0}
    end
fun name {name, formals, locals, localsInFrame} = name
fun formals {name, formals, locals, localsInFrame} = formals
fun allocLocal {name, formals, locals, localsInFrame} escape =
    let val newAccess = generateLocalAccess(escape, localsInFrame)
    in
	(locals := newAccess :: !locals;
	 newAccess)
    end



fun exp (InFrame k) treeexp = Tree.MEM(Tree.BINOP(Tree.PLUS, treeexp, Tree.CONST(k)))
  | exp (InReg t) treeexp = Tree.TEMP(t)

fun genShiftInst(access, reg) = Tree.MOVE(exp access (Tree.TEMP FP), Tree.TEMP reg)

fun externalCall (s, args) = Tree.CALL(Tree.NAME(Temp.namedlabel s), args)

(* This is also in `translate.sml` but it creats a circular import if used from this file, TODO: move to util *)
fun seq(fst :: []) = fst
  | seq(fst :: rst) = Tree.SEQ(fst, seq(rst))
  | seq([]) = ErrorMsg.impossible "cannot build seq from empty list"
				      
fun procEntryExit1 ({name, formals, locals, localsInFrame}, body) =
    let
	val viewShiftInst = ListPair.map genShiftInst (formals, (map #2 argregs))
	val offset = ref (!localsInFrame * wordsize)
	val calleeSaveTemps = map (fn reg => (let val oldoffset = !offset
										in
										(offset := wordsize + !offset;
										(InFrame (oldoffset + wordsize), reg))
										end))
				  (RA :: (map #2 calleesaves))
	val saveInst = map (fn (dst, reg) => Tree.MOVE(exp dst (Tree.TEMP(FP)), Tree.TEMP reg)) calleeSaveTemps
	val restoreInst = map (fn (src, reg) => Tree.MOVE(Tree.TEMP reg, exp src (Tree.TEMP(FP)))) (List.rev calleeSaveTemps)
	val sequence = 	seq(viewShiftInst (* View shift instructions (move out of argument registers) *)
	    @ saveInst (* Save Callee saves register on stack *)
	    @ [body] (* Body of frame *)
	    @ restoreInst) (* Restore callee save registers from stack *)
    in
	sequence
    end

fun procEntryExit2 (frame, body) =
    body @
    [A.OPER{assem="",
	    src=[SP,RA,ZERO]@(map #2 calleesaves),
	    dst=[], jump=SOME[]
    }]
	
fun procEntryExit3 ({name, formals, locals, localsInFrame}, body) =
    let
	(* Taken from 
	   https://stackoverflow.com/questions/8192517/
	 *)
	fun intToString i =
	    if i < 0
	    then ("-" ^ Int.toString (~i))
	    else Int.toString(i)
	val maximumLegalValue =  !localsInFrame - ((List.length argregs) * wordsize)
    in
	(* First need to save old stack pointer and make FP be the stack pointer
	   then offset the stack pointer by the size of the outgoing parameters of the CALL instruction
	   (here we just calculate the maximum size as described on page 261)
	 *)
	{prolog=Symbol.name name ^ ":\n" 
		^ "\tsw $fp, 0($sp)\n"
		^ "\tmove $fp, $sp\n" 
		^ "\taddi $sp, $sp " ^ intToString(maximumLegalValue) ^ "\n"
	,
	 body=body,
	epilog="\tmove $sp, $fp\n"
	       ^ "\tlw $fp, 0($sp)\n"
	       ^ "\tjr $ra\n"
	}
    end

fun printFrags (frags) = app printFrag frags
and printFrag (PROC {body, frame}) = Printtree.printtree(TextIO.stdOut, body)
  | printFrag (STRING (label, str)) = print (str ^ "\n")

fun string(l, str) = (Symbol.name l)  ^ ":\n .asciiz \"" ^ String.toCString str ^ "\"\n"
					    
end
structure Frame : FRAME = MipsFrame
