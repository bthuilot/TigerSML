signature FRAME = sig
  type frame
  type access
  type register
  val newFrame : {name: Temp.label, formals: bool list} -> frame
  val name : frame -> Temp.label
  val formals : frame -> access list
  val allocLocal : frame -> bool -> access
  val procEntryExit1: frame * Tree.stm -> Tree.stm
  val procEntryExit2: frame * Assem.instr list -> Assem.instr list
  val procEntryExit3 : frame * Assem.instr list -> {prolog: string, body: Assem.instr list, epilog: string}
  val FP : Temp.temp
  val RA : Temp.temp
  val RV : Temp.temp
  val SP : Temp.temp
  val wordsize : int
  val exp : access -> Tree.exp -> Tree.exp
  val externalCall : string * Tree.exp list -> Tree.exp
  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string
  val printFrags: frag list -> unit
  val tempMap: register Temp.Table.table
  val calleesaves: (string * Temp.temp) list
  val callersaves: (string * Temp.temp) list
  val argregs: (string * Temp.temp) list
  val tempToStr: Temp.temp -> string
end
