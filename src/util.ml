(* run_test: ('a -> string) -> ('b -> string) -> string -> 
 *           ('a -> 'b) -> 'a -> 'b -> unit
 * ENSURES: runs given function on given input, and compares to expected
 *          output, returning false and noting if there are any differences
 *)
let run_test (a2s : 'a -> string) (b2s : 'b -> string) (func_name : string)
             (func : 'a -> 'b) (input : 'a) (expected : 'b) : unit =
  let result = func input in
  if (result = expected)
  then ()
  else
    (print_string (func_name ^ " test failed:\n" ^
                   "input: " ^ (a2s input) ^ "\n" ^
                   "output:   " ^ (b2s result) ^ "\n" ^
                   "expected: " ^ (b2s expected) ^ "\n\n"))
