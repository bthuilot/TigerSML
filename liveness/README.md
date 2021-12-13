# Assignment 7 - Liveness Analysis

**Group:**

- Bryce Thuilot
- Khalil Haji

In this assignment, we implemented liveness analysis for our compiler

**New Files:**

- `makegraph.sml`
- `liveness.sml`
- `graph.sml` (provided to us)
- `flow.sml`
- `labeltable.sml`

# Summary

This assignment was very straightfoward to implement with the given signatures in the textbook. There were a few hints in the textbook where we decided to do things slightly differently.

# TempSet

We used the `smlnj` built-in `ListSetFn` for our live-in and live-out sets. This includes all the standard set operators including union, difference, adding items, and conversions to/from lists.

# LiveMap

Due to our use of the built-in set, we chose not to use the `liveMap` type provided in the textbook. Instead, we stored the liveMap as a table of flow graph nodes to `TempSet`s.

# LookOrFail

We added a function to `table.sml` called `lookOrFail` this function makes use of `lookup` instead of `find`. Instead of returning an option like the latter, `lookup` returns the value directly and raises an exception if the value does not exist in the map. We chose to use this rather than pattern matching everywhere since at this stage of the compiler, we have ruled out semantic and syntactical errors. Any issues that could cause a table entry to not be found are most likely compiler errors that do not require as detailed error messages.
