structure FindEscape: sig val findEscape: Absyn.exp -> unit end =
struct
structure S = Symbol
structure A = Absyn
type depth = int
type escEnv = (depth * bool ref) S.table
																 
fun traverseVar(env:escEnv, d:depth, A.SimpleVar(sym, pos)) : unit =
		(case S.look(env, sym)
			of SOME (depth, esc) => if d > depth then (esc := true; ()) else ()
			 | NONE => ())
	| traverseVar(env:escEnv, d:depth, A.FieldVar(var, sym, pos)) : unit =
		traverseVar(env, d, var)
	| traverseVar(env:escEnv, d:depth, A.SubscriptVar(var, exp, pos)) : unit = 
		(traverseVar(env, d, var); traverseExp(env, d, exp))
				
and traverseExp(env:escEnv, d:depth, s:Absyn.exp) : unit =
		let fun trexp(A.VarExp var) = traverseVar(env, d, var)
					| trexp(A.CallExp {func, args, pos}) = (map trexp args; ())
					| trexp(A.OpExp {left, oper, right, pos}) = (trexp left; trexp right)
					| trexp(A.RecordExp {fields, typ, pos}) = (map (fn (_, exp, _) => trexp exp); ())
					| trexp(A.SeqExp []) = ()
					| trexp(A.SeqExp((exp, pos) :: rst)) = (trexp exp; trexp(A.SeqExp rst))
					| trexp(A.AssignExp {var, exp, pos}) = trexp exp
					| trexp(A.IfExp {test, then', else'=NONE, pos}) = (trexp test; trexp then')
					| trexp(A.IfExp {test, then', else'=SOME(elseExp), pos}) = (trexp test; trexp then'; trexp elseExp)
					| trexp(A.WhileExp {test, body, pos}) = (trexp test; trexp body)
					| trexp(A.ForExp {var, escape, lo, hi, body, pos}) = let val new_env = S.enter(env, var, (d, escape))
																															 in
																																 (traverseExp(new_env, d, lo);
																																	traverseExp(new_env, d, hi);
																																	traverseExp(new_env, d, body))
																															 end
					| trexp(A.LetExp {decs, body, pos}) = let val new_env = traverseDecs(env, d, decs)
																								in 
																									traverseExp(new_env, d, body)
																								end
					| trexp(A.ArrayExp{typ, size, init, pos}) = (trexp size; trexp init)
					| trexp(_) = ()
		in
			trexp(s)
		end
and traverseDecs(env, d, s: Absyn.dec list) : escEnv =
		let fun trdec(A.FunctionDec fundecs, env) = (map trfundec fundecs; env)
					| trdec(A.VarDec {name, escape, typ, init, pos}, env) = let val new_env = S.enter(env, name, (d, escape))
																																	in
																																		(traverseExp(env, d, init);
																																		 new_env)
																																	end
					| trdec(A.TypeDec tydecs, env) = env
				and trfundec({name, params, result, body, pos}) =
						let val new_env = foldl (fn ({name, escape, typ, pos}, env) => S.enter(env, name, (d+1, escape))) env params
						in
							traverseExp(new_env, d+1, body)
						end
		in
			foldl trdec env s
		end
				
				
fun findEscape(prog: Absyn.exp) : unit = traverseExp(S.empty, 0, prog)
end
