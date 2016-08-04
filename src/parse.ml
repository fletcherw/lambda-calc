open Lex

type expr =
    | Int   of int
    | Neg   of expr
    | Plus  of expr * expr
    | Minus of expr * expr
    | Times of expr * expr
    | Div   of expr * expr
    | Exp   of expr * expr
    | Mod   of expr * expr

exception ParseError of string

(* parse helper functions *)

(* precedence : token -> int
 * REQUIRES: t is an operator token
 * ENSURES: precedence t yields the precedence ranking of t
 *)
let precedence (t : token) : int =
    match t with
    | PlusTok   -> 3
    | MinusTok  -> 3
    | NegTok    -> 6
    | TimesTok  -> 4
    | DivTok    -> 4
    | ExpTok    -> 5
    | ModTok    -> 4
    | _         -> raise (Failure "precedence called on non-operator token")

type assoc = Left | Right

(* associativity : token -> assoc
 * REQUIRES: t is an operator token
 * ENSURES: associativity t == Right if t is right associative and Left if
 *          t is left associative
 *)
let associativity (t : token) : assoc =
    match t with
    | PlusTok   -> Left
    | MinusTok  -> Left
    | NegTok    -> Left
    | TimesTok  -> Left
    | DivTok    -> Left
    | ExpTok    -> Right
    | ModTok    -> Left
    | _         -> raise (Failure "associativity called on non-operator token")

(* binop2expr : token -> (expr * expr) -> expr
 * REQUIRES: t is a binary operator token
 * ENSURES: binop2expr t yields a function which takes in an expression tuple
 *          and returns the result of applying the operator t to those
 *          two expressions.
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

(* parse' : token list -> expr list -> token list -> expr list
 * REQUIRES: true
 * ENSURES: parse' t [] [] yields the output stack that results from the
 *          token list t after parsing using the shunting yard algorithm,
 *          according to the precedence and associativity values assigned
 *          in the functions precedence and associativity. If no such parsing
 *          exists, raises an exception.
 *)
let rec parse' (t : token list)
               (out : expr list)
               (opstack : token list) : expr list =
    (match t with
           | (IntTok i)::t' -> parse' t' ((Int i)::out) opstack
           | LparTok::t' -> parse' t' out ((LparTok)::opstack)
           | RparTok::t' ->
                   (match (out, opstack) with
                   | (_, []) -> raise (ParseError "Mismatched parentheses 1")
                   | (_, LparTok::opstack') -> parse' t' out opstack'
                   | (out1::out', NegTok::opstack') ->
                           parse' t (Neg(out1)::out') opstack'
                   | (out2::out1::out', op::opstack') ->
                           parse' t
                                  (((binop2expr op) (out1, out2))::out')
                                  opstack'
                   | (_, _) -> raise (ParseError "Missing operators 1"))
           | op1::t' ->
                   (match opstack with
                      | [] ->         parse' t' out (op1::opstack)
                      | LparTok::_ -> parse' t' out (op1::opstack)
                      | RparTok::_ -> parse' t' out (op1::opstack)
                      | op2::opstack' ->
                         (match (associativity op1,
                                 compare (precedence op1) (precedence op2)) with
                               | (Right, 0) -> parse' t' out (op1::opstack)
                               | (Right, 1) -> parse' t' out (op1::opstack)
                               | (Left,  1) -> parse' t' out (op1::opstack)
                               | (_, _) ->
                                   (match (op2, out) with
                                       | (NegTok, o1::out') ->
                                               parse' t ((Neg o1)::out') opstack'
                                       | (_, o2::o1::out') ->
                                               parse' t (((binop2expr op2)
                                               (o1, o2))::out') opstack'
                                       | (_, _) -> raise
                                              (ParseError "Missing operators 2")
                                               )))
           | [] ->
                   (match (out, opstack) with
                       | (_, []) -> out
                       | (_, LparTok::opstack') ->
                               raise (ParseError "Mismatched parentheses 2")
                       | (_, RparTok::opstack') ->
                               raise (ParseError "Mismatched parentheses 3")
                       | (out1::out', NegTok::opstack') ->
                               parse' t (Neg(out1)::out') opstack'
                       | (out2::out1::out', op::opstack') ->
                               parse' t
                                      ( ((binop2expr op) (out1, out2))::out')
                                      opstack'
                       | (_, _) -> raise (ParseError "Missing operators 3")))

    (* parse : token list -> expr option
     * REQUIRES: true
     * ENSURES: parse t yields the unique operator precedence parsing of the
     *          token list t using the shunting yard algorithm, according to the
     *          precedence and associativity values assigned in the functions
     *          precedence and associativity. If no such parsing exists, raises an
     *          exception.
     *)
    let parse (t : token list) : expr =
      match parse' t [] [] with
            | [e] -> e
            | _   -> raise (ParseError "Invalid expression") 

