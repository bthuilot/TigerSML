signature ENV = sig
    datatype enventry = VarEntry of {access: Translate.access, ty: Types.ty}
		      | FunEntry of {level: Translate.level, label: Temp.label, formals: Types.ty list, result: Types.ty}
		      | CounterEntry of Translate.access
					    
    val base_tenv : Types.ty Symbol.table (* predefined types*)
    val base_venv : enventry Symbol.table (* predefinedfunctions*) end

structure Env : ENV =
struct

structure S = Symbol
structure T = Types

datatype enventry = VarEntry of {access: Translate.access, ty: T.ty}
                  | FunEntry of {level: Translate.level,
				 label: Temp.label,
				 formals: T.ty list, result: T.ty}
		  | CounterEntry of Translate.access


val base_tenv = S.empty
val base_tenv = S.enter (base_tenv, S.symbol "int", T.INT)
val base_tenv = S.enter (base_tenv, S.symbol "string", T.STRING)

val base_venv = S.empty
val base_venv = S.enter (base_venv, S.symbol "print", FunEntry {level = Translate.outermost, label = Temp.namedlabel "print", formals = [T.STRING], result = T.UNIT})
val base_venv = S.enter (base_venv, S.symbol "flush", FunEntry {level = Translate.outermost, label = Temp.namedlabel "flush", formals = [], result = T.UNIT})
val base_venv = S.enter (base_venv, S.symbol "getchar", FunEntry {level = Translate.outermost, label = Temp.namedlabel "getchar", formals = [], result = T.STRING})
val base_venv = S.enter (base_venv, S.symbol "ord", FunEntry {level = Translate.outermost,label = Temp.namedlabel "ord", formals = [T.STRING], result = T.INT})
val base_venv = S.enter (base_venv, S.symbol "chr", FunEntry {level = Translate.outermost, label = Temp.namedlabel "chr", formals = [T.INT], result = T.STRING})
val base_venv = S.enter (base_venv, S.symbol "size", FunEntry {level = Translate.outermost,label = Temp.namedlabel "size", formals = [T.STRING], result = T.INT})
val base_venv = S.enter (base_venv, S.symbol "substring", FunEntry {level = Translate.outermost, label = Temp.namedlabel "substring", formals = [T.STRING, T.INT, T.INT], result = T.STRING})
val base_venv = S.enter (base_venv, S.symbol "concat", FunEntry {level = Translate.outermost, label = Temp.namedlabel "concat", formals = [T.STRING, T.STRING], result = T.STRING})
val base_venv = S.enter (base_venv, S.symbol "not", FunEntry {level = Translate.outermost, label = Temp.namedlabel "not", formals = [T.INT], result = T.INT})
val base_venv = S.enter(base_venv, S.symbol "print_int", FunEntry{level = Translate.outermost, label = Temp.namedlabel "print_int", formals = [T.INT], result = T.UNIT})

end
