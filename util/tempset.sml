structure TempOrdKey =
struct 
type ord_key = Temp.temp
val compare = Int.compare
end

structure TempSet = ListSetFn(TempOrdKey)