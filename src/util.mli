(* run_test: ('a -> string) -> ('b -> string) -> string -> 
 *           ('a -> 'b) -> 'a -> 'b -> unit
 * ENSURES: runs given function on given input, and compares to expected
 *          output, returning false and noting if there are any differences
 *)
val run_test : ('a -> string) -> ('b -> string) -> string ->
               ('a -> 'b) -> 'a -> 'b -> unit
