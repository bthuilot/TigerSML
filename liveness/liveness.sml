structure Liveness: LIVENESS =
struct



structure T = Flow.Graph.Table
datatype igraph =
	 IGRAPH of {graph: Graph.graph,
		    tnode: Temp.temp -> Graph.node,
		    gtemp: Graph.node -> Temp.temp,
		    move: (Graph.node * Graph.node) list
		   }

fun interferenceGraph (Flow.FGRAPH {control, def, use, ismove} : Flow.flowgraph): igraph * (Flow.Graph.node -> Temp.temp list) = 
    let
	val nodes = Graph.nodes control

	val initLiveOut = foldl (fn (node, table)  => T.enter(table, node, TempSet.empty)) T.empty nodes
	val initLiveIn = foldl (fn (node, table)  => T.enter(table, node, TempSet.empty)) T.empty nodes
	val moves : (Graph.node * Graph.node) list ref = ref []


	fun computeNodeLiveness(node, inSet, outSet) =
	    let
		val newNodeIn' = T.lookOrFail(inSet, node)
		val newNodeOut' = T.lookOrFail(outSet, node)
		val newNodeIn = TempSet.union(TempSet.fromList(T.lookOrFail(use, node)), TempSet.difference(newNodeOut', TempSet.fromList(T.lookOrFail(def, node))));
		val newIn = T.enter(inSet, node, newNodeIn)
		val newNodeOut = foldl (fn (node, set) => TempSet.union(T.lookOrFail(newIn, node), set)) TempSet.empty (Graph.succ node)
		val newOut = T.enter(outSet, node, newNodeOut)
	    in
		(newIn, newOut, TempSet.equal(newNodeIn', newNodeIn) andalso TempSet.equal(newNodeOut', newNodeOut))
	    end

	fun computeLiveness(nodes, liveIn, liveOut) = 
	    let
		val (newLiveIn, newLiveOut, unchanged)= foldl (fn (node, (liveIn, liveOut, unchanged)) => 
								  let 
								      val (newIn, newOut, nodeUnchanged) = computeNodeLiveness(node, liveIn, liveOut)
								  in
								      (newIn, newOut, nodeUnchanged andalso unchanged)
								  end
							      )
							      (liveIn, liveOut, true)
							      nodes
	    in
		if unchanged 
		then (newLiveIn, newLiveOut) 
		else computeLiveness(nodes, newLiveIn, newLiveOut)
	    end

	val (_, liveOutMap) = computeLiveness(nodes, initLiveIn, initLiveOut)
					     
	val ig = Graph.newGraph()
	val temps = foldl (fn (node, sofar) => (TempSet.addList(sofar, (T.lookOrFail(use, node)@T.lookOrFail(def, node))))) TempSet.empty nodes

	val (tnodemap, gtempmap) = TempSet.foldl (fn (tmp, (tnodeacc, gtempmapcc)) => 
						     let val newNode = Graph.newNode(ig)
						     in
							 (Temp.Table.enter(tnodeacc, tmp, newNode), T.enter(gtempmapcc, newNode, tmp))
						     end)
						 (Temp.Table.empty, T.empty)
						 temps

	fun createEdges(flownode) =
	    let
		val defTmps = T.lookOrFail(def, flownode)
		val useTmps = T.lookOrFail(use, flownode)
		val useigNodes = map (fn tmp => Temp.Table.lookOrFail(tnodemap, tmp)) useTmps
		val defigNodes = map (fn tmp => Temp.Table.lookOrFail(tnodemap, tmp)) defTmps
		val outTmps = T.lookOrFail(liveOutMap, flownode)
		(* Note that we can't use TempSet.map here since the result type will not be a set item *)
		val outigNodes = map (fn tmp => Temp.Table.lookOrFail(tnodemap, tmp)) (TempSet.toList(outTmps))
		val isNodeMove = T.lookOrFail(ismove, flownode)
					     
	    in
		app (fn (defNode) => app 
					 (fn (outNode) => 
					     if isNodeMove andalso (List.exists (fn (useNode) => Graph.eq(useNode, outNode)) useigNodes)
					     then moves := (defNode, outNode) :: !moves
   					     else Graph.mk_edge({from=defNode, to=outNode}))
					 outigNodes) defigNodes
	    end
	val _ = app createEdges nodes
    in
	(IGRAPH {graph= ig,
		 tnode= (fn (tmp) => Temp.Table.lookOrFail(tnodemap, tmp)),
		 gtemp= (fn (n) => T.lookOrFail(gtempmap, n)),
		 move= !moves},
	 (fn n => TempSet.toList(T.lookOrFail(liveOutMap, n))))
    end


fun show (outstream, IGRAPH {graph, tnode, gtemp, move}) =
    let fun say s =  TextIO.output(outstream,s)
        fun sayln s= (say s; say "\n")
        val nodes = Graph.nodes graph
        fun showNode n = Temp.makestring(gtemp n) ^ ": "
                         ^ (foldl (fn (neighbor, str) => Temp.makestring(gtemp neighbor) ^ ", " ^ str)
                                  ""
                                  (Graph.adj n))
    in
        app (fn (x) => sayln(showNode(x))) nodes
    end
end
