structure Semant =
struct
structure S = Symbol
structure T = Types
structure A = Absyn
structure E = Env
structure IR = Translate

type venv = Env.enventry S.table
type tenv = T.ty S.table
type expty = {exp: IR.exp, ty: Types.ty}
val err = ErrorMsg.error
val impossible = ErrorMsg.impossible
val dummy = {exp=IR.dummy, ty=T.UNIT}

fun actual_ty(T.NAME (_, ref(SOME(ty)))) = actual_ty ty
  | actual_ty(T.NAME (_, ref(NONE))) = impossible "uninitialized name type found"
  | actual_ty ty = ty
                       
fun expectType(expected, given, pos) =
    let val expected_actual = actual_ty expected
        val given_actual = actual_ty given
    in if expected_actual = given_actual
       then ()
       else case expected_actual
             of T.RECORD _ =>
                if given_actual = T.NIL
                then ()
                else err pos "record type does not match"
              | _ => err pos "types do not match"
    end

fun lookupTy(tenv, tySym, pos) =
    case S.look(tenv, tySym)
     of SOME ty => ty
      | NONE => (err pos ("type: " ^ (S.name tySym) ^ " not found"); T.NIL)


                    

fun enterParams(level, venv, params) =
    case params
     of [] => venv
      | ((name, ty, esc), access) :: rst => enterParams(level, S.enter(venv, name, E.VarEntry {ty=ty, access=access}), rst)

fun getFormal tenv {name, escape, typ, pos} = (name, lookupTy(tenv, typ, pos), !escape)

fun checkFundec(header_venv, tenv, {name, params, result, body, pos}, brklvl, brklabel) =
    let val formals_aug = map (getFormal tenv) params
        val {label, level, ...} = case S.look(header_venv, name)
                                   of SOME(E.FunEntry e) => e
                                    | _ => impossible "name must be bound to function"
        val new_level = IR.newLevel {parent=level, name=label, formals=map (#3 o (getFormal tenv)) params}
        val access = case IR.formals(new_level)
                      of (_ :: access) => access
                       | [] => impossible "first formal must be frame pointer"
        val new_venv = enterParams(new_level, header_venv, ListPair.zip(formals_aug, access))
        val {exp=irExp, ty=bodyTy} = transExp(new_level, new_venv, tenv, body, brklvl, brklabel)
    in
      case result
       of SOME (resSym, pos) =>
          (expectType(lookupTy(tenv, resSym, pos), bodyTy, pos);
           IR.procEntryExit({level=new_level, body=irExp}))
        | NONE =>
          (expectType(T.UNIT, bodyTy, pos);
           IR.procEntryExit({level=new_level, body=irExp}))
    end
        

and transFundecs(level, venv, tenv, decs, brklvl, brklabel) =
    let fun enterHeaders(venv, tenv, decs) =
            case decs
             of [] => venv
              | {name, params, result, body, pos} :: rst =>
                let val label = Temp.newlabel()
                in
                  enterHeaders(S.enter(venv, name, E.FunEntry{formals=map (#2 o (getFormal tenv)) params,
                                                              result=(case result
                                                                       of SOME (resSym, pos) => lookupTy(tenv, resSym, pos)
                                                                        | NONE => T.UNIT),
                                                              label=label,
                                                              level=level}), tenv, rst)
                end
        fun checkUnique (decs, seen) =
            case decs
             of [] => ()
              | {name, params, result, body, pos} :: rst =>
                if (List.exists (fn x => x = name) seen)
                then err pos "function names must be unique in the same block"
                else checkUnique(rst, name :: seen)
        val header_venv = enterHeaders(venv, tenv, decs)
    in
      (checkUnique(decs, []);
       case decs
        of [] => header_venv
         | fst :: rst => (checkFundec(header_venv, tenv, fst, brklvl, brklabel); 
                          transFundecs(level, header_venv, tenv, rst, brklvl, brklabel)))
          
    end

and transTy(tenv, A.NameTy(tySym, pos)) = lookupTy(tenv, tySym, pos)
  | transTy(tenv, A.RecordTy(fields)) = T.RECORD(map (fn {name, escape, typ, pos} => (name, lookupTy(tenv, typ, pos))) fields, ref ())
  | transTy(tenv, A.ArrayTy(tySym, pos)) = T.ARRAY(lookupTy(tenv, tySym, pos), ref ())


                                                  
and transTydecs(tenv, decs) =
    let fun enterHeaders(tenv, decs) =
            case decs
             of [] => tenv
              | {name, ty, pos} :: rst => enterHeaders(S.enter(tenv, name, T.NAME(name, ref NONE)), rst)
        val header_tenv = enterHeaders(tenv, decs)
        fun fillRef{name, ty, pos} =
            let val nameTy = lookupTy(header_tenv, name, pos)
            in
              case nameTy
               of T.NAME(sym, tyRef) => tyRef := SOME(transTy(header_tenv, ty))
                | _ => impossible "non-name found"
            end
        fun fillRefs(decs) =
            case decs
             of [] => ()
              | fst :: rst => (fillRef(fst); fillRefs(rst))
        fun checkTyCycle(ty, seen, pos) =
            case ty
             of T.NAME (sym, tyRef) =>
                if (List.exists (fn x => x = sym) seen)
                then (err pos "cycle detected"; raise ErrorMsg.Error)
                else (case !tyRef
                       of SOME ty => checkTyCycle(ty, sym :: seen, pos)
                        | NONE => impossible "uninitialized NAME found")
              | _ => ()
        fun checkCycles(decs) =
            case decs
             of [] => ()
              | {name, ty, pos} :: rst =>
                (checkTyCycle(lookupTy(header_tenv, name, pos), [], pos);
                 checkCycles(rst))
        fun checkUnique (decs, seen) =
            case decs
             of [] => ()
              | {name, ty, pos} :: rst =>
                if (List.exists (fn x => x = name) seen)
                then err pos "type names must be unique in the same block"
                else checkUnique(rst, name :: seen)
        fun checkInitialized ({name, ty, pos} :: rst) = (actual_ty (lookupTy(header_tenv, name, pos));
                                                         ())
          | checkInitialized [] = ()
    in
      (fillRefs(decs);
       checkCycles(decs);
       checkUnique(decs, []);
       header_tenv)
    end

and transDec(level, venv, tenv, dec, brklvl, brklabel) =
    case dec
     of A.VarDec {name, escape, typ, init, pos} =>
        let val {exp=initIR, ty=initTy} = transExp(level, venv, tenv, init, brklvl, brklabel)
            val access = IR.allocLocal level (!escape)
            val varIR = IR.simpleVar(access, level)
        in
          case typ
           of SOME (typ, pos) =>
              let val typeHint = lookupTy(tenv, typ, pos)
              in
                (expectType(typeHint, initTy, pos);
                 {exps=[IR.assignExp(varIR, initIR)], venv=S.enter(venv, name, E.VarEntry {ty=typeHint, access=access}), tenv=tenv})
              end

            | NONE => case initTy
                       of T.NIL => (err pos "can only assign explicit record types to nil"; {exps=[], venv=venv, tenv=tenv})
                        | _ => {exps=[IR.assignExp(varIR, initIR)], venv=S.enter(venv, name, E.VarEntry {ty=initTy, access=access}), tenv=tenv}
        end
      | A.FunctionDec fundecs => {exps=[], venv=transFundecs(level, venv, tenv, fundecs, brklvl, brklabel), tenv=tenv}
      | A.TypeDec tydecs => {exps=[], venv=venv, tenv=transTydecs(tenv, tydecs)}
                                
and transDecs(level, venv, tenv, decs, brklvl, brklabel, expIRs) =
    case decs
     of dec :: rst => let val {exps=exps, venv=new_venv, tenv=new_tenv} = transDec(level, venv, tenv, dec, brklvl, brklabel)
                      in
                        transDecs(level, new_venv, new_tenv, rst, brklvl, brklabel, exps @ expIRs)
                      end
      | [] => {exps=expIRs, venv=venv, tenv=tenv}

and lookupVar(venv, var, pos) =
    case S.look(venv, var)
     of SOME(E.VarEntry {ty, access}) => (access, ty)
      | SOME(E.CounterEntry access) => (access, T.INT)
      | SOME(E.FunEntry _) => (err pos "expected var found func"; raise ErrorMsg.Error)
      | NONE => (err pos "var undefined or not in scope"; raise ErrorMsg.Error)
                    
                    
and transVar(level, venv, tenv, var, brklvl, brklabel) =
    let fun getField(fields, field, pos) =
            case fields
             of [] => (err pos "field not found"; T.NIL)
              | (name, ty) :: rst => if name = field then ty else getField(rst, field, pos)
        fun getFieldIdx(fields, field) =
            case fields
             of [] => ~1
              | (name, ty) :: rst => if name = field then 0 else 1 + getFieldIdx(rst, field)
    in
      case var
       of A.SimpleVar(name, pos) => let val (access, varTy) = lookupVar(venv, name, pos)
                                    in
                                      {exp=IR.simpleVar(access, level), ty=varTy}
                                    end
        | A.FieldVar(var, field, pos) =>
          let val {exp=varIR, ty=varTy} = transVar(level, venv, tenv, var, brklvl, brklabel)
              val varTyA = actual_ty varTy
          in
            case varTyA
             of T.RECORD (fields, _) => {exp=IR.fieldVar(varIR, getFieldIdx(fields, field)), ty=getField(fields, field, pos)}
              | _ => (err pos "can only get fields of records"; dummy)
          end
        | A.SubscriptVar(var, exp, pos) =>
          let val {exp=varIR, ty=varTy} = transVar(level, venv, tenv, var, brklvl, brklabel)
              val varTy_actual = actual_ty varTy
              val {exp=idxIR, ty=idxTy} = transExp(level, venv, tenv, exp, brklvl, brklabel)
          in
            (checkInt(idxTy, pos);
             (case varTy_actual
               of T.ARRAY (ty, _) => {exp=IR.subscriptVar(varIR, idxIR), ty=ty}
                | _ => (err pos "can only subscript arrays"; dummy)))
          end
    end

and checkInt(ty, pos) =
    let val ty_actual = actual_ty ty
    in
      case ty_actual of
          T.INT => ()
        | _ => err pos "int type expected"
    end



        
and transExp(level, venv, tenv, exp, brklvl, brklabel) = 
    let fun trexp(level, A.VarExp var, brklvl) = transVar(level, venv, tenv, var, brklvl, brklabel)
          | trexp(level, A.NilExp, brklvl) = {exp=IR.nilExp(), ty=T.NIL}
          | trexp(level, A.IntExp i, brklvl) = {exp=IR.intExp(i), ty=T.INT}
          | trexp(level, A.StringExp(str, pos), brklvl) = {exp=IR.stringExp(str), ty=T.STRING}
          | trexp(level, A.CallExp{func, args, pos}, brklvl) =
            let fun checkType(expected, given) = let val {exp=irExp, ty=argTy} = trexp(level, given, brklvl)
                                                 in
                                                   (expectType(expected, argTy, pos);
                                                    irExp)
                                                 end
            in
              case S.look(venv, func)
               of SOME(E.VarEntry _) => (err pos "variable is not bound to a function"; dummy)
                | NONE => (err pos "variable undefined"; dummy)
                | SOME(E.CounterEntry _) => (err pos "variable is not bound to a function"; dummy)
                | SOME(E.FunEntry {formals, result, label, level=funlevel}) =>
                  if length formals = length args
                  then {exp=IR.callExp(label, level, funlevel, map checkType (ListPair.zip(formals, args)), actual_ty result), ty=actual_ty result}
                  else (err pos "number of arguments do not match"; dummy)
            end
          | trexp(level, A.OpExp{left,oper,right,pos}, brklvl) =
            let val {exp=leftIR, ty=leftTy} = trexp(level, left, brklvl)
                val {exp=rightIR, ty=rightTy} = trexp(level, right, brklvl)
                val leftTyA = actual_ty leftTy
                val rightTyA = actual_ty rightTy
                val resIR = IR.opExp(oper, leftIR, rightIR)
            in
              case oper
               of
                  (A.EqOp | A.NeqOp) =>
                  (case (leftTyA, rightTyA) of
                       (T.INT, T.INT) => {exp=resIR, ty=T.INT}
                     | (T.RECORD (_, lu), T.RECORD (_, ru)) =>
                       if lu = ru
                       then {exp=resIR, ty=T.INT}
                       else (err pos ("Cannot compare different record types"); dummy)
                     | (T.RECORD (_, lu), T.NIL) => {exp=resIR, ty=T.INT}
                     | (T.NIL, T.RECORD (_, ru)) => {exp=resIR, ty=T.INT}
                     | (T.STRING, T.STRING) => {exp=IR.strComp(oper, leftIR, rightIR), ty=T.INT}
                     | (T.ARRAY (_, lu), T.ARRAY (_, ru)) =>
                       if lu = ru
                       then {exp=resIR, ty=T.INT}
                       else (err pos ("Cannot compare different array types"); dummy)
                     | _ => (err pos "comparison is only available for integers, records, arrays, and strings"; dummy))


                | _ => (checkInt(leftTy, pos);
                        checkInt(rightTy, pos);
                        {exp=resIR, ty=T.INT})
            end
          | trexp(level, A.RecordExp{fields, typ, pos}, brklvl) =
            let fun checkField((expectedSym, expectedTy), (givenSym, givenExp, pos)) =
                    if expectedSym = givenSym
                    then expectType(expectedTy, #ty (trexp(level, givenExp, brklvl)), pos)
                    else err pos "field names do not match"
                             
                fun checkFields(expectedFields, givenFields) =
                    if length expectedFields = length givenFields
                    then app checkField (ListPair.zip(expectedFields, givenFields))
                    else err pos "length of fields does not match record type"
                val fieldExps = map (fn (_, exp, _) => trexp(level, exp, brklvl)) fields
                val ty_actual = actual_ty(lookupTy(tenv, typ, pos))
            in
              case ty_actual
               of T.RECORD (expectedFields, _) => (checkFields(expectedFields, fields); {exp=IR.recordExp(map #exp fieldExps), ty=ty_actual})
                | _ => (err pos "cannot create record with non-record type"; {exp=IR.dummy, ty=T.UNIT})
            end
          | trexp(level, A.SeqExp nil, brklvl) = {exp=IR.seqExp([]), ty=T.UNIT}
          | trexp(level, A.SeqExp((exp, pos)::nil), brklvl) = trexp(level, exp, brklvl)
          | trexp(level, A.SeqExp(exprs), brklvl) =
            let fun trexprs(nil, acc) = {exp=IR.dummy, ty=T.UNIT}
                  | trexprs((exp, pos)::nil, acc) = let val {exp=irExp, ty=resTy} = trexp(level, exp, brklvl)
                                                    in
                                                      {exp=IR.seqExp(List.rev(irExp::acc)), ty=actual_ty resTy}
                                                    end
                  | trexprs((exp, pos)::rst, acc) = let val {exp=irExp, ty=expTy} = trexp(level, exp, brklvl)
                                                    in
                                                      trexprs(rst, irExp::acc)
                                                    end
            in
              trexprs(exprs, [])
            end
          | trexp(level, A.AssignExp{var, exp, pos}, brklvl) =
            let val {exp=varIR, ty=varTy} =
                    (case var
                      of A.SimpleVar(name, pos) =>
                         (case S.look(venv, name)
                           of SOME (E.CounterEntry _) => (err pos "cannot assign to counter"; dummy)
                            | _ => transVar(level, venv, tenv, var, brklvl, brklabel)
                         )
                       | _ => transVar(level, venv, tenv, var, brklvl, brklabel))
                val {exp=expIR, ty=expTy} = trexp(level, exp, brklvl)
            in
              (expectType(varTy, expTy, pos);
               {exp=IR.assignExp(varIR, expIR), ty=T.UNIT})
            end
          | trexp(level, A.IfExp{test, then', else'=SOME(else'), pos}, brklvl) =
            let val {exp=testIR, ty=testTy} = trexp(level, test, brklvl)
                val {exp=thenIR, ty=thenTy} = trexp(level, then', brklvl)
                val {exp=elseIR, ty=elseTy} = trexp(level, else', brklvl)

            in
              (checkInt(testTy, pos);
               expectType(thenTy, elseTy, pos);
               {exp=IR.ifElseExp(testIR, thenIR, elseIR), ty=elseTy})
            end
          | trexp(level, A.IfExp{test, then', else'=NONE, pos}, brklvl) =
            let val {exp=testIR, ty=testTy} = trexp(level, test, brklvl)
                val {exp=thenIR, ty=thenTy} = trexp(level, then', brklvl)
            in
              (checkInt(testTy, pos);
               expectType(T.UNIT, thenTy, pos);
               {exp=IR.ifThenExp(testIR, thenIR), ty=T.UNIT})
            end
          | trexp(level, A.WhileExp{test, body, pos}, brklvl) =
            let val {exp=testIR, ty=testTy} = trexp(level, test, brklvl)
                val newbrklabel = Temp.newlabel()
                val {exp=bodyIR, ty=bodyTy} = transExp(level, venv, tenv, body, brklvl+1, newbrklabel)
            in
              (checkInt(testTy, pos);
               expectType(T.UNIT, bodyTy, pos);
               {exp=IR.whileExp(testIR, bodyIR, newbrklabel), ty=T.UNIT})
            end
          | trexp(level, A.ForExp{var, escape, lo, hi, body, pos}, brklvl) =
            let val {exp=loIR, ty=loTy} = trexp(level, lo, brklvl)
                val {exp=hiIR, ty=hiTy} = trexp(level, hi, brklvl)
                val access = IR.allocLocal level (!escape)
                val new_venv = S.enter(venv, var, E.CounterEntry access)
                val newbrklabel = Temp.newlabel()
                val {exp=bodyIR, ty=bodyTy} = transExp(level, new_venv, tenv, body, brklvl+1, newbrklabel)
            in
              (checkInt(loTy, pos);
               checkInt(hiTy, pos);
               expectType(T.UNIT, bodyTy, pos);
			   {exp=IR.forExp(IR.simpleVar(access, level), loIR, hiIR, bodyIR, newbrklabel), ty=T.UNIT})
            end
          | trexp(level, A.BreakExp pos, brklvl) =
            if brklvl > 0 then {exp=IR.breakExp(brklabel), ty=T.UNIT} else (err pos "cannot break outside of a loop"; dummy)
          | trexp(level, A.LetExp{decs, body, pos}, brklvl) =
            let val {exps=expIRs, venv=new_venv, tenv=new_tenv} = transDecs(level, venv, tenv, decs, brklvl, brklabel, [])
                val {exp=bodyIR, ty=bodyTy} = transExp(level, new_venv, new_tenv, body, brklvl, brklabel)
            in
              {exp=IR.letExp(List.rev expIRs, bodyIR), ty=bodyTy}
            end
          | trexp(level, A.ArrayExp{typ, size, init, pos}, brklvl) =
            let val {exp=initIR, ty=initTy} = trexp(level, init, brklvl)
                val ty_actual = actual_ty(lookupTy(tenv, typ, pos))
                val {exp=sizeIR, ty=sizeTy} = trexp(level, size, brklvl)
            in
              case ty_actual
               of T.ARRAY (arrayTy, _) =>
                  (expectType(arrayTy, initTy, pos);
                   {exp=IR.arrayExp(sizeIR, initIR), ty=ty_actual})
                | _ => (err pos "array type expected"; dummy)

            end
    in (trexp(level, exp, brklvl)) end

and transProg(exp) =
    let val lev = IR.newLevel {parent=IR.outermost, name=Temp.namedlabel "main", formals=nil}
        val {exp=irExp, ty=expTy} = transExp(lev, E.base_venv, E.base_tenv, exp, 0, Temp.namedlabel "main")
    in
      IR.procEntryExit({level=lev, body=irExp});
      IR.getResult()
    end

end
