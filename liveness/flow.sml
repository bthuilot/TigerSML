structure Flow =
struct
structure Graph = Graph
datatype flowgraph =
	 FGRAPH of {control: Graph.graph,
		    def: Temp.temp list Graph.Table.table,
		    use: Temp.temp list Graph.Table.table,
		    ismove: bool Graph.Table.table
		   }
end
    
