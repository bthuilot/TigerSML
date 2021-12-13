structure RegAlloc: REG_ALLOC =
struct
structure Frame = MipsFrame
type allocation = Frame.register Temp.Table.table
fun alloc(instrs, frame): Assem.instr list * allocation =
    let
        val (fgraph, _) = MakeGraph.instrs2graph instrs
        val (igraph, _) = Liveness.interferenceGraph fgraph
        val allocated = Color.color(
		{interference=igraph, 
		 initial=Frame.tempMap, 
		 registers=Frame.registers})
    in
        (instrs, allocated)
    end
        
end
