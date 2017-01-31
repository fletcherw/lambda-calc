open Parse
open Lex

module StringMap = Map.Make(String) 

exception UndefinedVariable of string

(* pow : int -> int -> int
 * ENSURES: pow a == f, where f b = a to the bth power
 *)
let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

(* evaluate : int StringMap.t -> expr -> int
 * ENSURES: evaluate e evaluates the computation tree e and returns
 *          the resulting value
 *)
let rec evaluate (env : int StringMap.t) (e : expr) : int =
  let evaluate = evaluate env in
  match e with
    | Int(i)        -> i
    | Var(i)        ->
      if StringMap.mem i env
      then StringMap.find i env
      else raise (UndefinedVariable i)
    | Neg(e1)       -> (evaluate e1) * -1
    | Plus(e1, e2)  -> (evaluate e1) + (evaluate e2)
    | Minus(e1, e2) -> (evaluate e1) - (evaluate e2)
    | Times(e1, e2) -> (evaluate e1) * (evaluate e2)
    | Div(e1, e2)   -> (evaluate e1) / (evaluate e2)
    | Exp(e1, e2)   -> pow (evaluate e1) (evaluate e2)
    | Mod(e1, e2)   -> (evaluate e1) mod (evaluate e2)

(* run_repl : int StringMap.t -> 'a *) 
let rec run_repl env =
  let _ = print_string "> " in
  let input = read_line () in
  (try 
    (let tokens = lex input in
     let (result_name, tokens) = 
       match tokens with
       | LetTok::VarTok(i)::EqTok::rest -> (i, rest)
       | _ -> ("_", tokens) in
     let expression = parse tokens in 
     let result = evaluate env expression in
     let env = StringMap.add result_name result env in
     print_string ("val " ^ result_name ^ " = " ^ string_of_int result ^ "\n\n");
     run_repl env)
  with 
    | LexError(s) -> print_string ("Lex Error: " ^ s ^ "\n\n")
    | ParseError(s) -> print_string ("Parse Error: " ^ s ^ "\n\n")
    | UndefinedVariable(i) -> print_string ("Undefined Variable: " ^ i ^ "\n\n"));
  run_repl env

let repl () = run_repl StringMap.empty
