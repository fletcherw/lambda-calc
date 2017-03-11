open Lex

type expr =
  | Int   of int
  | Var   of string
  | Neg   of expr
  | Plus  of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Div   of expr * expr
  | Exp   of expr * expr
  | Mod   of expr * expr

exception ParseError of string

(* parse helper functions *)

(* exp_to_string : expr -> string
 * exp_to_string e yields a human readable representation of e
 *)
let rec exp_to_string exp =
  match exp with
    | Int   i -> "Int " ^ string_of_int i
    | Var   i -> "Var " ^ i
    | Neg   e -> "Neg( " ^ exp_to_string e ^ " )"
    | Plus  (e1, e2) -> "Plus( " ^ exp_to_string e1 ^ " , " ^
                                   exp_to_string e2 ^ " )"
    | Minus (e1, e2) -> "Minus( " ^ exp_to_string e1 ^ " , " ^
                                    exp_to_string e2 ^ " )"
    | Times (e1, e2) -> "Times( " ^ exp_to_string e1 ^ " , " ^
                                    exp_to_string e2 ^ " )"
    | Div   (e1, e2) -> "Div( " ^ exp_to_string e1 ^ " , " ^
                                  exp_to_string e2 ^ " )"
    | Exp   (e1, e2) -> "Exp( " ^ exp_to_string e1 ^ " , " ^
                                  exp_to_string e2 ^ " )"
    | Mod   (e1, e2) -> "Mod( " ^ exp_to_string e1 ^ " , " ^
                                  exp_to_string e2 ^ " )"

(* precedence : token -> int
 * t is an operator token
 * precedence t yields the precedence ranking of t
 *)
let precedence (t : token) : int =
    match t with
      | PlusTok
      | MinusTok  -> 3
      | TimesTok
      | DivTok
      | ModTok    -> 4
      | ExpTok    -> 5
      | NegTok    -> 6
      | _         -> raise (Failure "precedence called on non-operator token")

type assoc = Left | Right

(* associativity : token -> assoc
 * t is an operator token
 * associativity t == Right if t is right associative and Left if
 * t is left associative
 *)
let associativity (t : token) : assoc =
    match t with
      | PlusTok
      | MinusTok
      | NegTok
      | TimesTok
      | DivTok
      | ModTok    -> Left
      | ExpTok    -> Right
      | _         -> raise (Failure "associativity called on non-operator token")

(* binop2expr : token -> (expr * expr) -> expr
 * REQUIRES: t is a binary operator token
 * binop2expr t yields a function which takes in an expression tuple
 * and returns the result of applying the operator t to those
 * two expressions.
 *)
let binop2expr (t : token) : ((expr * expr) -> expr) =
    match t with
      | PlusTok   -> fun (e1, e2) -> (Plus (e1, e2))
      | MinusTok  -> fun (e1, e2) -> (Minus (e1, e2))
      | TimesTok  -> fun (e1, e2) -> (Times (e1, e2))
      | DivTok    -> fun (e1, e2) -> (Div (e1, e2))
      | ExpTok    -> fun (e1, e2) -> (Exp (e1, e2))
      | ModTok    -> fun (e1, e2) -> (Mod (e1, e2))
      | _         -> raise (Failure "binop2expr called on non-binary operator")

(* is_binop : token -> bool
 * Returns true if t is a binary operator token.
 *)
let is_binop (t : token) : bool =
    match t with
      | PlusTok
      | MinusTok
      | TimesTok
      | DivTok
      | ExpTok
      | ModTok    -> true
      | _         -> false

(* postfix : token list -> expr list -> token list -> bool -> expr list
 * (postfix t [] [] false) yields the reverse polish notation token list that
 * results from the token list t after parsing using the shunting-yard
 * algorithm, according to the precedence and associativity values assigned
 * in the functions precedence and associativity. If no such parsing
 * exists, raises an exception.
 *)
let rec postfix (t : token list)
                (out : token list)
                (opstack : token list)
                (prev_is_operand : bool) : token list =
    (*
    print_string ("t: " ^ Lex.token_list_to_string t ^ "\n");
    print_string ("opstack: " ^ Lex.token_list_to_string opstack ^ "\n");
    print_string ("out: " ^ Lex.token_list_to_string out ^ "\n\n");
    *)
    match t with
      | LetTok::_ -> raise (ParseError "Unexpected keyword 'let'")
      | EqTok::_ -> raise (ParseError "Unexpected token '='")
      | LparTok::RparTok::_ -> raise (ParseError "Consecutive parens")
      | (IntTok i)::t' -> postfix t' ((IntTok i)::out) opstack true
      | (VarTok i)::t' -> postfix t' ((VarTok i)::out) opstack true
      | LparTok::t' -> postfix t' out (LparTok::opstack) false
      | RparTok::t' ->
         (match opstack with
            | [] -> raise (ParseError "Mismatched parentheses 1")
            | LparTok::opstack' -> postfix t' out opstack' true
            | op::opstack' -> postfix t (op::out) opstack' prev_is_operand)
      | MinusTok::t' when not prev_is_operand ->
         (match opstack with
           | NegTok::opstack' ->
               postfix t (NegTok::out) opstack' false
           | _ ->
               postfix t' out (NegTok::opstack) prev_is_operand)
      | op1::t' ->
        (match opstack with
           | [] ->         postfix t' out (op1::opstack) false
           | LparTok::_ -> postfix t' out (op1::opstack) false
           | RparTok::_ -> postfix t' out (op1::opstack) false
           | op2::opstack' ->
              (match (associativity op1,
                      compare (precedence op1) (precedence op2)) with
                 | (Right, 0) -> postfix t' out (op1::opstack) false
                 | (Right, 1) -> postfix t' out (op1::opstack) false
                 | (Left,  1) -> postfix t' out (op1::opstack) false
                 | (_, _) -> postfix t (op2::out) opstack' prev_is_operand))
      | [] ->
        (match opstack with
           | [] -> List.rev out
           | LparTok::_ ->
               raise (ParseError "Mismatched parentheses 2")
           | RparTok::_ ->
               raise (ParseError "Mismatched parentheses 3")
           | op::opstack' ->
               postfix t (op::out) opstack' prev_is_operand)

(* build_tree : token list -> expr list -> expr list
 * Attempts to construct an expression tree from the given token list
 * (in reverse polish notation). If no valid expression can be created,
 * raises an exception.
 *)
let rec build_tree (input : token list)
                   (stack : expr list) : expr list =
  match input with
    | [] -> stack
    | LetTok::_ -> raise (ParseError "Unexpected keyword 'let'")
    | EqTok::_ -> raise (ParseError "Unexpected token '='")
    | (IntTok i)::input' -> build_tree input' ((Int i)::stack)
    | (VarTok i)::input' -> build_tree input' ((Var i)::stack)
    | NegTok::input' ->
      (match stack with
         | operand::stack' ->
           build_tree input' (Neg(operand)::stack')
         | _ -> raise (ParseError "Missing operands for unary operator '-'"))
    | op::input' when is_binop op ->
      (match stack with
         | out2::out1::stack' ->
           build_tree input' (((binop2expr op) (out1, out2))::stack')
         | _ -> raise (ParseError "Missing operands for binary operator"))
    | _ -> raise (ParseError "broken")

(* parse : token list -> expr option
 * parse t gives the unique operator precedence parsing of the
 * token list t using the shunting-yard algorithm, according to the
 * precedence and associativity values assigned in the functions
 * precedence and associativity. If no such parsing exists, raises an
 * exception.
 *)
let parse (t : token list) : expr =
  let tokens = postfix t [] [] false in
  match build_tree tokens [] with
    | [e] -> e
    | res ->
      List.iter (fun e -> print_string (exp_to_string e ^ " ")) res;
      print_string "\n";
      raise (ParseError "Invalid parse")

(* run_tests: unit -> unit
 * ENSURES: if all tests pass, run_tests () does nothing, otherwise it
 *          notes which tests failed
 *)
let run_tests () =
  let test_parse = Util.run_test (fun a -> a)
                                 exp_to_string
                                 "parse"
                                 (fun e -> parse (lex e)) in

  test_parse "2+3"
             (Plus(Int 2, Int 3));
  test_parse "3 * 10 ^ 8 * x5"
             (Times(Times(Int 3, Exp(Int 10, Int 8)), Var "x5"));
  test_parse "2--3"
             (Minus(Int 2, Neg(Int 3)));
  test_parse "-(1+2)"
             (Neg(Plus(Int 1, Int 2)));
  test_parse "-3^5"
             (Exp(Neg(Int 3), Int 5));
  test_parse "2+3*-5"
             (Plus(Int 2, Times(Int 3, Neg(Int 5))))
