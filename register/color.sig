signature COLOR =
sig
    structure Frame : FRAME
    type allocation = Frame.register Temp.Table.table

    val color: {interference: Liveness.igraph,
		initial: allocation,
		registers: Frame.register list}
	       -> allocation
         exception SpillError
end
