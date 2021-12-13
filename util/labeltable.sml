structure LabelMap=
struct
fun mkTable() = HashTable.mkTable (HashString.hashString, op=) (128, Fail "Label not found")
fun enter(t,k,v) = HashTable.insert t (Symbol.name k, v)
fun look(t,k) = HashTable.lookup t (Symbol.name k)
end
