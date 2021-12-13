signature CODEGEN =
sig
    structure Frame : FRAME
    val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end

structure MipsGen : CODEGEN =
struct

structure T = Tree
structure A = Assem
structure Frame = MipsFrame
val err = ErrorMsg.error
val impossible = ErrorMsg.impossible

fun codegen (frame) (stm: Tree.stm) : Assem.instr list =
    let (* Functions given 
	   ---------------
	 *)
	val ilist = ref (nil : A.instr list)
	fun emit x = ilist := x :: !ilist
	(* Generates a new temp and and calls the function supplied 
	   with the temp and returns the temp *)
	fun result gen =
	    let
		val t = Temp.newtemp()
	    in
		(gen t; t)
	    end

	(* Helpers 
	 ---------
	 *)

        (* Taken from 
	   https://stackoverflow.com/questions/8192517/
	 *)
	fun intToString i =
	    if i < 0
	    then ("-" ^ Int.toString (~i))
	    else Int.toString(i)

	(* binopToAssem: Tree.binop -> string *)
	fun binopToAssem T.PLUS = "add"
	  | binopToAssem T.MINUS = "sub"
	  | binopToAssem T.MUL = "mul"
	  | binopToAssem T.DIV = "div"
	  | binopToAssem T.AND = "and"
	  | binopToAssem T.OR = "or"
	  | binopToAssem T.LSHIFT = "sllv"
	  | binopToAssem T.RSHIFT = "srlv"
	  | binopToAssem T.ARSHIFT = "srav"
	  | binopToAssem T.XOR = "xor" 

	fun relopToAssem T.EQ = "beq"
	  | relopToAssem T.NE = "bne"
	  | relopToAssem T.GT = "bgt"
	  | relopToAssem T.GE = "bge"
	  | relopToAssem T.LT = "blt"
	  | relopToAssem T.LE = "ble"
	  | relopToAssem T.ULT = "bltu"
	  | relopToAssem T.ULE = "bleu"
	  | relopToAssem T.UGT = "bgtu"
	  | relopToAssem T.UGE = "bgeu"
				     
	fun munchArgs (i, arg::args) =
	    let
		(* numArgRegs: number of arugment registers *)
		val numArgRegs = List.length Frame.argregs
		(* offset: the offset of the argument when put onto the frame *)
		val offset = Frame.wordsize * (i-numArgRegs)
		(* argReg: The register this argument will be assigned to 
		   if the argument number is less than the amount of argument registers
		   otherwise will be a new temp that is not used *)
		val argReg = if i < numArgRegs
			     then (#2 (List.nth (Frame.argregs, i)))
			     else Temp.newtemp()
		(* argToFrame: moves the argument to the frame *)
		fun argToFrame arg = munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP(Frame.SP), T.CONST offset)), arg))
		(* argToReg moves the argument to the given temp, used when moving arguments to arg register*)
		fun argToReg (t, arg) = munchStm(T.MOVE(T.TEMP(t), arg))
	    in
		if i < numArgRegs
		then (argToReg (argReg, arg); argReg :: munchArgs(i + 1, args))
		else (argToFrame (arg); munchArgs(i + 1, args))
	    end
	  | munchArgs (i, []) = nil
				    
	(* munchStm: Tree.stm -> uint
	   munchStm consumes a Tree.stm and coverts it into a `Assem.instr list`
	   and concats the list to `ilist`
	 *)
	and munchStm (T.SEQ(a, b)) = (munchStm a; munchStm b)
	  | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) =
	    munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2))
	  | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) =
	    emit(A.OPER{assem="sw `s1, "^ intToString i ^"(`s0)\n",
			src=[munchExp e1, munchExp e2],
			dst=[],
			jump=NONE})
	  | munchStm (T.MOVE(T.TEMP t, T.CONST i)) =
	    emit(A.OPER{
		      assem="li `d0, " ^ intToString i ^ "\n",
		      src=[], dst=[t], jump=NONE
		})
	  | munchStm (T.MOVE(T.MEM(T.CONST i), e)) =
	    emit(A.OPER{
		      assem="sw `s0, " ^ intToString i ^ "($r0)\n",
		      src=[munchExp e], dst=[], jump=NONE
		})
	  | munchStm (T.MOVE(T.MEM(e1), e2)) =
	    emit(A.OPER{
		      assem="sw `s1, 0(`s0)\n",
		      src=[munchExp e1, munchExp e2], dst=[], jump=NONE
		})
	  | munchStm (T.MOVE(T.TEMP t, e2)) =
	    emit(A.MOVE{
		      assem="move `d0,`s0\n",
		      src=munchExp e2, dst=t
		})
	  | munchStm (T.LABEL l) =
	    emit(A.LABEL{assem=Symbol.name(l) ^ ":\n", lab=l})
	  | munchStm (T.EXP e) = (munchExp(e); ())
	  | munchStm (T.JUMP(T.NAME(l), labels)) =
	    emit(A.OPER{
		      assem="j " ^ Symbol.name l ^ "\n",
		      src=[], dst=[], jump=SOME(labels)
		})
	  | munchStm (T.JUMP(e, labels)) =
	    emit(A.OPER{
		      assem="j `j0\n",
		      src=[munchExp e],dst=[],
		      jump=SOME(labels)
		})
	  | munchStm (T.CJUMP(relop, e1, e2, l1, l2)) =
	    emit(A.OPER{
		      assem= relopToAssem relop ^ " `s0, `s1, `j0\n",
		      src=[munchExp e1, munchExp e2],
		      dst=[], jump=SOME[l1, l2]
		})
	  | munchStm stm =
	    (Printtree.printtree(TextIO.stdOut, stm);
	     impossible "Could not munch statement")

		
		
	(* munchExp: Tree.exp -> Tree.temp
	   munchExp consumes a `Tree.exp` converts it into a Temp.temp containg the result
	   of the evaulated expression
	 *)
	and munchExp (T.MEM(T.BINOP(T.PLUS, e1, T.CONST i))) =
	    result(fn r =>
		      emit(A.OPER{
				assem="lw `d0, " ^ intToString i ^ "(`s0)\n",
				src=[munchExp e1], dst=[r], jump=NONE
		  }))
	  | munchExp (T.MEM(T.BINOP(T.PLUS, T.CONST i, e1))) =
	    munchExp (T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)))
	  | munchExp (T.MEM(T.CONST i)) =
	    result(fn r =>
		      emit(A.OPER{
				assem="lw `d0, " ^ intToString i ^"($zero)\n",
				src=[], dst=[r], jump=NONE
		  }))
	  | munchExp (T.BINOP(T.PLUS, e1, T.CONST i)) =
	    result(fn r =>
		      emit(A.OPER{
				assem="addi `d0, `s0, " ^ intToString i ^ "\n",
				src=[munchExp e1], dst=[r], jump=NONE
		  }))
	  | munchExp (T.BINOP(T.PLUS, T.CONST i, e1)) =
	    munchExp (T.BINOP(T.PLUS, e1, T.CONST i))
	  | munchExp (T.BINOP(T.MINUS, e1, T.CONST i)) =
	    result(fn r =>
		      emit(A.OPER{
				assem="addi `d0, `s0, "^ intToString (~i) ^ "\n",
				src=[munchExp e1], dst=[r], jump=NONE
		  }))
	  | munchExp (T.BINOP(T.MINUS, T.CONST i, e1)) =
	    munchExp (T.BINOP(T.MINUS, e1, T.CONST i)) (* TODO fix this *)
	  | munchExp (T.BINOP(binop, e1, e2)) =
	    result(fn r =>
		      emit(A.OPER{
				assem= binopToAssem binop ^  " `d0, `s0, `s1\n",
				src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
		  }))
	  | munchExp (T.TEMP t) = t
	  | munchExp (T.MEM(e1)) =
	    result(fn r =>
		      emit(A.OPER{
				assem="lw `d0, 0(`s0)\n",
				src=[munchExp e1], dst=[r], jump=NONE
		  }))
	  | munchExp (T.CONST i) =
	    result(fn r =>
		      emit(A.OPER{
				assem="li `d0, " ^ intToString i ^"\n",
				src=[], dst=[r], jump=NONE
		  }))
	  | munchExp (T.ESEQ(stm,e)) =
	    (munchStm stm; munchExp e)
	  | munchExp (T.NAME n) =
	    result(fn r =>
		      emit(A.OPER{
				assem="la `d0, " ^ Symbol.name n ^ "\n",
				src=[], dst=[r], jump=NONE
		  }))
	  | munchExp (T.CALL(e, args))  =
	    let
		val calldefs = [Frame.RV, Frame.RA] @ (map (fn (_, t) => t) Frame.callersaves)
	    in
		(emit(A.OPER{
			   assem="jalr `s0\n",
			   src=munchExp(e)::munchArgs(0,args),
			   dst=calldefs, jump=NONE
		     });
		 Frame.RV)
	    end
    in
	(munchStm stm; rev(!ilist))
    end
end
