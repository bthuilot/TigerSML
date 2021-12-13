signature TRANSLATE = sig
    type level
    type exp
    type access
    val outermost : level
    val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
    val formals: level -> access list
    val allocLocal: level -> bool -> access
    val simpleVar: access * level -> exp
    val fieldVar: exp * int -> exp
    val subscriptVar: exp * exp -> exp
    val nilExp: unit -> exp
    val intExp: int -> exp
    val stringExp: string -> exp
    val callExp: Temp.label * level * level * exp list * Types.ty -> exp
    val opExp: Absyn.oper * exp * exp -> exp
    val strComp: Absyn.oper * exp * exp -> exp
    val recordExp: exp list -> exp
    val seqExp: exp list -> exp
    val assignExp: exp * exp -> exp
    val ifElseExp: exp * exp * exp -> exp
    val ifThenExp: exp * exp -> exp
    val whileExp: exp * exp * Temp.label -> exp
    val forExp: exp * exp * exp * exp * Temp.label -> exp
    val breakExp: Temp.label -> exp
    val arrayExp: exp * exp -> exp
    val letExp: exp list * exp -> exp
    val unEx: exp -> Tree.exp
    val unCx: exp -> (Temp.label * Temp.label -> Tree.stm)
    val unNx: exp -> Tree.stm
    val procEntryExit: {level: level, body: exp} -> unit
    val getResult : unit -> Frame.frag list
    val dummy: exp
end

structure Translate : TRANSLATE =
struct
datatype exp = Ex of Tree.exp
             | Nx of Tree.stm
             | Cx of Temp.label * Temp.label -> Tree.stm
                                                    
datatype level = Level of {parent: level, frame: Frame.frame, unique: unit ref}
       | Outermost
type access = level * Frame.access

val dummy = Ex (Tree.CONST(0))

val frags : Frame.frag list ref = ref []
val outermost = Outermost

fun newLevel {parent, name, formals} = Level {parent=parent, frame=Frame.newFrame({name=name, formals=true::formals}), unique=ref ()}

fun formals (level as Level {frame, ...}) = map (fn fa => (level, fa)) (Frame.formals(frame))
  | formals Outermost = []

fun allocLocal (level as Level {frame, ...}) escape = (level, Frame.allocLocal frame escape)
  | allocLocal Outermost escape = ErrorMsg.impossible "cannot allocate on top level"

fun seq(fst :: []) = fst
  | seq(fst :: rst) = Tree.SEQ(fst, seq(rst))
  | seq([]) = ErrorMsg.impossible "cannot build seq from empty list"
                                  
fun unEx(Ex e) = e
  | unEx(Cx genstm) =
    let val r = Temp.newtemp()
        val t = Temp.newlabel() and f = Temp.newlabel()
    in Tree.ESEQ(seq[
                      Tree.MOVE(Tree.TEMP r, Tree.CONST 1),
                      genstm(t, f),
                      Tree.LABEL f,
                      Tree.MOVE(Tree.TEMP r, Tree.CONST 0),
                      Tree.LABEL t],
                 Tree.TEMP r)
    end
  | unEx(Nx s) = Tree.ESEQ(s, Tree.CONST 0)

fun unNx(Ex e) = Tree.EXP(e)
  | unNx(Nx s) = s
  | unNx(Cx genstm) = let val z = Temp.newlabel()
                      in
                          seq[
                              genstm(z, z),
                              Tree.LABEL z
                          ]
                      end
                          
fun unCx(Ex (Tree.CONST(0))) = (fn(t, f) => Tree.JUMP(Tree.NAME f, [f]))
  | unCx(Ex (Tree.CONST(1))) = (fn(t, f) => Tree.JUMP(Tree.NAME t, [t]))
  | unCx(Ex e) = (fn(t, f) => Tree.CJUMP(Tree.EQ, e, Tree.CONST 0, f, t))
  | unCx (Cx genstm) = genstm
  | unCx (Nx stm) = (Printtree.printtree(TextIO.stdOut, stm); ErrorMsg.impossible "conditional expected, encountered statement")
                        
                        

fun computeStaticLink ((uselevel as Level {parent=useparent, unique=useunique, ...}), (deflevel as Level {unique=defunique, ...})) = 
    if useunique = defunique
    then Tree.TEMP(Frame.FP)
    else Tree.MEM(computeStaticLink(useparent, deflevel))
  | computeStaticLink(Outermost, Outermost) = Tree.TEMP(Frame.FP)
  | computeStaticLink(Outermost, Level {frame=frame,...}) = ErrorMsg.impossible ("static link not found: " ^ (Symbol.name (Frame.name(frame))))
  | computeStaticLink(uselevel as Level {...}, Outermost) = ErrorMsg.impossible "if function scope is outermost, we should be using external call"

fun nilExp() = Ex(Tree.CONST(0))
                 
fun simpleVar ((deflevel, defacc), uselevel) = 
    Ex (Frame.exp defacc (computeStaticLink(uselevel, deflevel)))

fun fieldVar (e, i) = Ex (Tree.MEM(Tree.BINOP(Tree.PLUS, unEx e, Tree.CONST(Frame.wordsize * i))))

fun subscriptVar (e, ie) = Ex (Tree.MEM(Tree.BINOP(Tree.PLUS, unEx e, Tree.BINOP(Tree.MUL, unEx ie, Tree.CONST(Frame.wordsize)))))


fun intExp(i) = Ex (Tree.CONST(i))

fun stringExp(str) = let val lab = Temp.newlabel()
                         val str' = Frame.STRING(lab, str)
                     in (frags := str' :: !frags;
                         Ex (Tree.NAME(lab)))
                     end

fun callExp(lab, _, Outermost, args, result) = 
    if result = Types.UNIT
    then Nx(Tree.EXP(Frame.externalCall(Symbol.name lab, map unEx args)))
    else Ex(Frame.externalCall(Symbol.name lab, map unEx args))
  | callExp(lab, callerLevel, calleeLevel, args, result) = 
    let val sl = computeStaticLink(callerLevel, calleeLevel)
    in
	Ex(Tree.CALL(Tree.NAME(lab), sl :: (map unEx args)))
    end

fun opExp(Absyn.PlusOp, e1, e2) = Ex(Tree.BINOP(Tree.PLUS, unEx e1, unEx e2))
  | opExp(Absyn.MinusOp, e1, e2) = Ex(Tree.BINOP(Tree.MINUS, unEx e1, unEx e2))
  | opExp(Absyn.TimesOp, e1, e2) = Ex(Tree.BINOP(Tree.MUL, unEx e1, unEx e2))
  | opExp(Absyn.DivideOp, e1, e2) = Ex(Tree.BINOP(Tree.DIV, unEx e1, unEx e2))
  | opExp(Absyn.EqOp, e1, e2) = Cx (fn(t, f) => Tree.CJUMP(Tree.EQ, unEx e1, unEx e2, t, f))
  | opExp(Absyn.NeqOp, e1, e2) = Cx (fn(t, f) => Tree.CJUMP(Tree.NE, unEx e1, unEx e2, t, f))
  | opExp(Absyn.LtOp, e1, e2) = Cx (fn(t, f) => Tree.CJUMP(Tree.LT, unEx e1, unEx e2, t, f))
  | opExp(Absyn.LeOp, e1, e2) = Cx (fn(t, f) => Tree.CJUMP(Tree.LE, unEx e1, unEx e2, t, f))
  | opExp(Absyn.GtOp, e1, e2) = Cx (fn(t, f) => Tree.CJUMP(Tree.GT, unEx e1, unEx e2, t, f))
  | opExp(Absyn.GeOp, e1, e2) = Cx (fn(t, f) => Tree.CJUMP(Tree.GE, unEx e1, unEx e2, t, f))

fun strComp(Absyn.EqOp, e1, e2) = Ex(Frame.externalCall("stringEqual", [unEx e1, unEx e2]))
  | strComp(Absyn.NeqOp, e1, e2) = Ex(Frame.externalCall("not", [Frame.externalCall("stringEqual", [unEx e1, unEx e2])]))
  | strComp(_, _, _) = ErrorMsg.impossible "string can only be compared for equality"



                                           
fun recordExp(exps) =
    let val addr = Temp.newtemp()
        fun allocFields(nil, idx) = []
          | allocFields(head :: rst, idx) = Tree.MOVE(Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.TEMP(addr), Tree.CONST(idx * Frame.wordsize))), unEx head) :: allocFields(rst, idx + 1)
    in
	Ex(Tree.ESEQ(Tree.SEQ(Tree.MOVE(Tree.TEMP(addr), Frame.externalCall("allocRecord", [Tree.CONST((length exps) * Frame.wordsize)])), seq(allocFields(exps, 0))), Tree.TEMP(addr)))
    end

fun seqExp([]) = Nx(Tree.EXP(Tree.CONST(0)))
  | seqExp(fst :: []) = Ex(unEx fst)
  | seqExp(fst :: rst) = Ex(Tree.ESEQ(unNx fst, unEx (seqExp(rst))))

                           
fun assignExp(exp1, exp2) = Nx(Tree.MOVE(unEx exp1, unEx exp2))

                              
(* val ifExp: exp * exp * exp option -> exp *)
fun ifElseExp(test, then', else') = let val testFn = unCx test
                                        val t = Temp.newlabel()
                                        val f = Temp.newlabel()
                                        val join = Temp.newlabel()
                                        val r = Temp.newtemp()
                                        fun genThen(Cx thenFn) =
                                            let val t' = Temp.newlabel()
                                            in
						[
                                                  thenFn(t', f),
                                                  Tree.LABEL(t'),
                                                  Tree.MOVE(Tree.TEMP r, Tree.CONST 1),
                                                  Tree.JUMP(Tree.NAME(join), [join])
						]
                                            end
                                          | genThen(Ex then') = [
                                              Tree.MOVE(Tree.TEMP r, then'),
                                              Tree.JUMP(Tree.NAME(join), [join])
                                          ]
                                          | genThen(Nx then') = [then']
                                        fun genElse(Cx elseFn) =
                                            let val t'' = Temp.newlabel()
                                                val f' = Temp.newlabel()
                                            in
						[
						  elseFn(t'', f'),
						  Tree.LABEL t'',
						  Tree.MOVE(Tree.TEMP r, Tree.CONST 1),
						  Tree.JUMP(Tree.NAME join, [join]),
						  Tree.LABEL f',
						  Tree.MOVE(Tree.TEMP r, Tree.CONST 0)
						]
                                            end
                                          | genElse(Ex else') = [Tree.MOVE(Tree.TEMP r, else')]
                                          | genElse(Nx else') = [else']
                                                                    
                                    in
					Ex(Tree.ESEQ(seq([testFn(t, f),
                                                          Tree.LABEL(t)]
							 @ genThen(then')
							 @ [Tree.LABEL f]
							 @ genElse(else')
							 @ [Tree.LABEL(join)]), Tree.TEMP(r)))
                                    end
                                        
fun ifThenExp(test, then') = let val testFn = unCx test
                                 val t = Temp.newlabel()
                                 val f = Temp.newlabel()
                             in
				 Nx(seq[testFn(t, f),
					Tree.LABEL(t),
					unNx then',
					Tree.LABEL(f)
                                   ])
                             end

fun whileExp(test, bodyExp, breakLabel) =
    let val testFn = unCx test
        val testLabel = Temp.newlabel()
        val bodyLabel = Temp.newlabel()

    in
	Nx(seq[Tree.LABEL testLabel,
               testFn(bodyLabel, breakLabel),
               Tree.LABEL bodyLabel,
               unNx bodyExp,
               Tree.JUMP(Tree.NAME testLabel, [testLabel]),
               Tree.LABEL breakLabel
          ])
    end

fun forExp(varExp, loExp, hiExp, bodyExp, breakLabel) = 
    let val testLabel = Temp.newlabel()
        val bodyLabel = Temp.newlabel()
        val incLabel = Temp.newlabel()
        val hiTemp = Temp.newtemp()
        val var = unEx varExp
    in
	Nx(seq[Tree.MOVE(var, unEx loExp),
               Tree.MOVE(Tree.TEMP hiTemp, unEx hiExp),
               Tree.LABEL testLabel,
               Tree.CJUMP(Tree.LE, var, Tree.TEMP hiTemp, bodyLabel, breakLabel),
               Tree.LABEL bodyLabel,
               unNx bodyExp,
               Tree.CJUMP(Tree.EQ, var, Tree.TEMP hiTemp, breakLabel, incLabel),
               Tree.LABEL incLabel,
               Tree.MOVE(var, Tree.BINOP(Tree.PLUS, var, Tree.CONST(1))),
               Tree.JUMP(Tree.NAME testLabel, [testLabel]),
               Tree.LABEL breakLabel])
    end


fun breakExp(breakLabel) = Nx(Tree.JUMP(Tree.NAME breakLabel, [breakLabel]))
                             
fun arrayExp(sizeExp, initExp) = Ex(Frame.externalCall("initArray", [unEx sizeExp, unEx initExp]))
                                   
fun letExp([], bodyExp) = bodyExp
  | letExp(decs, bodyExp) = Ex(Tree.ESEQ(seq(map unNx decs), unEx bodyExp))
                              
fun getResult() = !frags


fun procEntryExit({level = Level {parent, frame, ...}, body}) = 
    frags := Frame.PROC {body=Frame.procEntryExit1(frame, Tree.MOVE(Tree.TEMP Frame.RV, unEx body)), frame=frame} :: !frags
  | procEntryExit({level = Outermost, body}) = ()

end
