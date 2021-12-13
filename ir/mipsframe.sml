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
val callersaves = List.tabulate(8,
				(fn i => ("$t" ^ Int.toString(i), Temp.newtemp()))
			       )
val calleesaves = List.tabulate(8,
				(fn i => ("$s" ^ Int.toString(i), Temp.newtemp()))
			       )

val tempMap =
    let
	val table = Temp.Table.empty
    in
	foldl
	    (fn ((regName, regTemp), t) => Temp.Table.enter(t, regTemp, regName))
	    table
	    (specialregisters @ argregs @ callersaves @ calleesaves)
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

fun newFrame {name : Temp.label, formals} : frame = {name=name, formals=generateFrameAccess(formals, 4, 0), locals= ref [], localsInFrame=ref 0}
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

fun externalCall (s, args) = Tree.CALL(Tree.NAME(Temp.namedlabel s), args)

fun procEntryExit1 (frame, body) = body

fun procEntryExit2 (frame, body) =
    body @
    [A.OPER{assem="",
	    src=[ZERO, RA, SP]@(map (fn (_,t) => t) calleesaves),
	    dst=[], jump=SOME[]
    }]
	
fun procEntryExit3 ({name, formals, locals, localsInFrame}, body) =
    {prolog=Symbol.name name ^ ":\n",
     body=body,
     epilog="END jump to $ra\n" (*TODO implement jr $ra *)
    }

fun printFrags (frags) = app printFrag frags
and printFrag (PROC {body, frame}) = Printtree.printtree(TextIO.stdOut, body)
  | printFrag (STRING (label, str)) = print (str ^ "\n")

end
structure Frame : FRAME = MipsFrame
