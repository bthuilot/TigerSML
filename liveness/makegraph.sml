structure MakeGraph: MAKEGRAPH =
struct

structure A = Assem
		  
fun instrs2graph (instrs): Flow.flowgraph * Flow.Graph.node list =
    let
	(* Create a new graph `g` *)
	val g = Graph.newGraph()
	(* Create mapping from label -> node, to be used later when making edges between labels*)
	val labelMap = LabelMap.mkTable()

	(* creates a node for each instruction, and if the instruction is a label, adds it to the LabelMap *)
	fun makeNode(A.LABEL{lab=label, ...}) = let val node' = Graph.newNode(g)
                                                in
                                                    (LabelMap.enter(labelMap, label, node');
                                                     node')
                                                end
          | makeNode(instr) = Graph.newNode(g)
	(* create nodes from instructions *)
	val nodes = map makeNode instrs
			
	(* zips together nodes and instructions
	   used mainly to ensure nodes and instructions are of same length 
	   to avoid redundent checks for length differences in functions
	 *)
	val nodeInstrs = ListPair.zip(nodes, instrs)


	(* createEdges: (node * instr) * (node * instr) -> unit
	   creates edgeds between nodes in succesion and between nodes that have JUMP instruction to the corresponding node
	 *)
	fun createEdges((n1, A.OPER{jump=SOME(jumpLabels), ...}), (n2, _)) = 
            (Graph.mk_edge({from=n1, to=n2});
             app (fn (lab) => Graph.mk_edge {from=n1, to=LabelMap.look(labelMap, lab)}) jumpLabels)
          | createEdges((n1, _), (n2, _)) = Graph.mk_edge ({from=n1, to=n2})
							  

	(* createGraphTables: (node * inst) * table * table * table -> (table * table * table)
	   creates the graph tables for use, def, ismove, for a given node and it's Assem instruction
	 *)
	fun createGraphTables((node, A.OPER{dst=dst, src=src, ...}), (def, use, ismove)) = 
	    let
		val def' = Graph.Table.enter(def, node, dst)
		val use' = Graph.Table.enter(use, node, src)
		val ismove' = Graph.Table.enter(ismove, node, false)
	    in
		(def', use', ismove')
	    end
	  | createGraphTables((node, A.MOVE{dst=dst, src=src, ...}), (def, use, ismove)) = 
	    let
		val def' = Graph.Table.enter(def, node, [dst])
		val use' = Graph.Table.enter(use, node, [src])
		val ismove' = Graph.Table.enter(ismove, node, true)
	    in
		(def', use', ismove')
	    end
	  | createGraphTables((node, A.LABEL{...}), (def, use, ismove)) = 
	    let
		val def' = Graph.Table.enter(def, node, nil)
		val use' = Graph.Table.enter(use, node, nil)
		val ismove' = Graph.Table.enter(ismove, node, false)
	    in
		(def', use', ismove')
	    end

	(* createGraph (node * inst) list *  table * table * table -> table * table * table 
	   createGraph takes a list of node instruction parings and a def, use and ismove table
	   and creates edges between nodes in the graph `g` and returns the filled the def, use and ismove table
	 *)
	fun createGraph(nodeInst1 :: nodeInst2 :: nodeInstrs, (def, use, ismove)) =
	    let 
		val (def', use', ismove') = createGraphTables(nodeInst1, (def, use, ismove))
	    in
		(createEdges(nodeInst1, nodeInst2); createGraph(nodeInst2::nodeInstrs, (def', use', ismove')))
	    end
	  | createGraph((nodeInst as (n1, A.OPER{jump=SOME(jumpLabels), ...})) :: nil, (def, use, ismove)) =
	    let
		val tables = createGraphTables(nodeInst, (def, use, ismove))
	    in 
		(app (fn (lab) => Graph.mk_edge {from=n1, to=LabelMap.look(labelMap, lab)}) jumpLabels;
		 tables
		)
	    end
	  | createGraph((nodeInst :: nil), (def, use, ismove)) = 
	    createGraphTables(nodeInst, (def, use, ismove))
	  (* Last element just return tables *)
	  | createGraph(nil, (def, use, ismove)) = 
	    (def,use,ismove)
		

	val (def, use, ismove) = createGraph(nodeInstrs, (Graph.Table.empty, Graph.Table.empty, Graph.Table.empty))
    in
	(* FlowGraph is the created graph and the `use`, `def` and `ismove` tables*)
	(Flow.FGRAPH{control=g, def=def, use=use, ismove=ismove}, nodes)
    end
end

