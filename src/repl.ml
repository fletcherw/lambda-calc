open Parse

(* pow : int -> int -> int
 * REQUIRES: true
 * ENSURES: pow a == f, where f b = a to the bth power
 *)
let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

(* compile : string -> expr option
 * ENSURES: compile s yields the operator precedence parsing of the token
 *          stream that results when s is lexed. If no valid parsing exists
 *          compile s raises an exception.
 *)
let compile (s : string) : expr option = parse (try Lex.lex s with _ -> [])

(* evaluate : expr -> int
 * REQUIRES: true
 * ENSURES: evaluate e evaluates the computation tree e and returns
 *          the resulting value
 *)
let rec evaluate (e : expr) : int =
    match e with
        | Int(i)        -> i
        | Neg(e1)       -> (evaluate e1) * -1
        | Plus(e1, e2)  -> (evaluate e1) + (evaluate e2)
        | Minus(e1, e2) -> (evaluate e1) - (evaluate e2)
        | Times(e1, e2) -> (evaluate e1) * (evaluate e2)
        | Div(e1, e2)   -> (evaluate e1) / (evaluate e2)
        | Exp(e1, e2)   -> pow (evaluate e1) (evaluate e2)
        | Mod(e1, e2)   -> (evaluate e1) mod (evaluate e2)

(* repl : unit -> unit
 * REQUIRES: true
 * ENSURES: true
 *) 
let rec repl () =
  let _ = print_string "> " in
  let input = read_line () in
  match compile(input) with
      | None -> let _ = 
        print_string "Error!\n\n" in repl ()
      | Some(e) -> let _ =
        print_string (string_of_int (evaluate e) ^ "\n\n") in repl ()

let () = repl ()
