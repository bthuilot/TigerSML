CM.make "sources.cm";

let

	fun exitError(str) = (print("ERROR: " ^str); OS.Process.exit(OS.Process.failure))
	fun parseArgs (nil) = (NONE, NONE)
		| parseArgs ("-o"::arg::xs) = 
			let
				val (inFile, outFile) = parseArgs xs
				val newOut = case outFile of
							SOME _ => (exitError("Please supply onlt 1 output file"); "")
						  | NONE => arg
			in
				(inFile, SOME(newOut))
			end
		| parseArgs (arg::xs) = 
			let
				val (inFile, outFile) = parseArgs xs
				val newIn = case inFile of
							SOME _ => (exitError("Please supply only 1 input file to compile"); "")
						  | NONE => arg
			in
				(SOME(newIn), outFile)
			end
	    
    val args = CommandLine.arguments()
	val (inFile, outFile) = parseArgs(args)
	val inFilePath = case inFile of
					NONE => (exitError("No input file given"); "")
					| SOME str => str
	val outFilePath = case outFile of 
					NONE => "a.out"
					| SOME str => str
in
    (Main.main (inFilePath, outFilePath);
     OS.Process.exit(OS.Process.success))

end



