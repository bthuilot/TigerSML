structure Color: COLOR =
struct
structure Frame = MipsFrame : FRAME
structure NodeSet = Graph.NodeSet
exception SpillError

structure RegisterSet = ListSetFn(struct
                                    type ord_key = Frame.register
                                    val compare = String.compare
                                  end)

    type allocation = Frame.register Temp.Table.table

    fun color({interference=Liveness.IGRAPH{graph=g, gtemp=gtemp, ...}, initial=init, registers=registers}) = 
        let
            val nodes = Graph.nodes g
            val (precolored, uncolored) = List.partition (fn n => let val tmp = Temp.Table.look(init, gtemp(n)) in case tmp of SOME(x) => true | NONE => false end handle LibBase.NotFound => false) nodes
            val k = length registers

            val initDegreeMap = foldl (fn (node, table) => Graph.Table.enter(table, node, NodeSet.numItems(NodeSet.fromList(Graph.adj node)))) Graph.Table.empty nodes
            val simplifyWorklist = List.filter (fn (n) => (Graph.Table.lookOrFail(initDegreeMap, n) < k)) uncolored
            val initStack = NodeSet.empty

            fun adjacent(n, stack) = 
              let 
                val neighbors = NodeSet.fromList(Graph.adj n)
              in
                NodeSet.toList(NodeSet.difference(neighbors, stack))
              end

              fun decrementDegree(node, (swl, degrees)) =
                let
                  val d = Graph.Table.lookOrFail(degrees, node)
                  val degrees' = Graph.Table.enter(degrees, node, d - 1)
                in
                    if d = k
                    then (node :: swl, degrees')
                    else (swl, degrees')
                end


                fun simplify([], stack, degreeMap) = (stack, degreeMap)
                  | simplify(node :: wlrest, stack, degreeMap) = 
                  let
                    val (swl', degreeMap') = foldl decrementDegree (wlrest, degreeMap) (adjacent(node, stack))
                  in
                    simplify(swl', NodeSet.add(stack, node), degreeMap')
                  end                
                
            val (stack, degreeMap) = simplify(simplifyWorklist, initStack, initDegreeMap)

              fun assignColors([], colored) = colored
                | assignColors(node :: stack, colored) =
                  let val neighborColors = foldl (fn (neighbor, colors) => case Temp.Table.look(colored, gtemp neighbor) of
                     SOME(c) => c :: colors
                   | NONE => colors) [] (Graph.adj node)

                      val okColors = RegisterSet.difference(RegisterSet.fromList(registers), RegisterSet.fromList(neighborColors))
                      val nextColor = hd(RegisterSet.toList(okColors)) handle Empty => raise SpillError
                    in
                        assignColors(stack, Temp.Table.enter(colored, gtemp node, nextColor))
                    end
            val coloredNodes = assignColors(NodeSet.toList(stack), init)
        in
            (coloredNodes)
        end
end

