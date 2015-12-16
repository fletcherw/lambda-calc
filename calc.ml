type token =
    | IntTok of int
    | LparTok
    | RparTok
    | PlusTok
    | MinusTok
    | NegTok
    | TimesTok
    | DivTok
    | ExpTok
    | ModTok

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
exception LexError

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

(* lexing helper functions *)

(* explode : string -> char list
 * REQUIRES: true
 * ENSURES: explode s = s', where s' is the character list representation
 *          of s as a string
 *)
let explode (s : string) : char list =
    let rec exp i l =
        if i < 0 then l else exp (i-1) (s.[i] :: l) in
    exp (String.length s - 1) []

(* isdigit : char -> bool
 * REQUIRES: true
 * ENSURES: isdigit c == true iff c is a digit
 *)
let isdigit (c : char) : bool =
    match c with
    | '0' -> true
    | '1' -> true
    | '2' -> true
    | '3' -> true
    | '4' -> true
    | '5' -> true
    | '6' -> true
    | '7' -> true
    | '8' -> true
    | '9' -> true
    | _   -> false

(* digit_to_int : char -> int
 * REQUIRES: c is a digit
 * ENSURES: (digit_to_int c) is the integer corresponding to c
 *)
let digit_to_int (c : char) : int =
    match c with
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | _   -> raise (Failure "c was not a digit")

(* charlist_to_int : char list -> int -> int option
 * REQUIRES: true
 * ENSURES: if c is a string of digits (optionally preceded with a minus sign),
 *          then charlist_to_int c 0 yields i, where i is the decimal
 *          representation of c.
 *)
let rec charlist_to_int (c : char list) (acc : int) : int option =
    match c with
    | [] -> None
    | [ch] ->
            if isdigit ch
            then Some (10 * acc + (digit_to_int ch))
            else None
    | ch1::ch2::chs ->
            (match (ch1, ch2, isdigit ch1) with
            | (_, '-', _) -> None
            | (_, _, true) -> charlist_to_int (ch2::chs)
                                              (10 * acc + digit_to_int ch1)
            | ('-', _, _) ->
                    (match charlist_to_int (ch2::chs) acc with
                     | None -> None
                     | Some(n) -> Some(-1 * n))
            | (_, _, _) -> None)

(* peelint : char list -> (char list * char list) option
 * REQUIRES: true
 * ENSURES: if there is some string of digits i (possibly prefixed
 *          with a minus sign) and some char list c' such that i @ c' == c,
 *          peelint c == Some(i, c'). Otherwise, peelint c == None
 *)
let rec peelint' (c : char list) : (char list * char list) option =
    match c with
    | []    -> None
    | [c1]  -> if isdigit c1 then Some ([c1], []) else None
    | c1::c2::ds ->
            (match (c1, c2, isdigit c1) with
            | ('-', '-', _)  -> None
            | (_, '-', true) -> Some ([c1], c2::ds)
            | (_, '-', false) -> None
            | ('-', _, _) ->
                    (match peelint' (c2::ds) with
                     | None -> None
                     | Some(n, r) -> Some(c1::n, r))
                    | (_, _, true) ->
                            (match peelint' (c2::ds) with
                     | None -> Some([c1], c2::ds)
                     | Some(n, r) -> Some(c1::n, r))
                            | (_, _, _) -> None)

(* peelint : char list -> (int * char list) option
 * REQUIRES: true
 * ENSURES: if there is some string of digits i (possibly prefixed
 *          with a minus sign) with numerical representation i' and some
 *          char list c' such that i @ c' == c, peelint c == Some(i', c').
 *          Otherwise, peelint c == None
 *)
let peelint (c : char list) : (int * char list) option =
    match peelint' c with
    | None -> None
    | Some(n, r) ->
            (match charlist_to_int n 0 with
            | None -> None
            | Some(i) -> Some(i, r))

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

(* lex' : char list -> token list -> token list
 * REQUIRES: true
 * ENSURES: lex' c t == (lex' c []) @ t, and lex' c [] is the token stream that
 *          results from lexing c, in reverse order.
 *)
let rec lex' (c : char list) (t : token list) : token list =
    match c with
       | [] -> t
       | ch :: chs ->
               (match ch with
               | ' ' -> lex' chs t
               | '+' -> lex' chs (PlusTok::t)
               | '*' -> lex' chs (TimesTok::t)
               | '/' -> lex' chs (DivTok::t)
               | '^' -> lex' chs (ExpTok::t)
               | '%' -> lex' chs (ModTok::t)
               | '(' -> lex' chs (LparTok::t)
               | ')' -> lex' chs (RparTok::t)
               | '-' ->
                       (match t with
                        | IntTok(_)::ts -> lex' chs (MinusTok::t)
                        | RparTok::ts -> lex' chs (MinusTok::t)
                        | _ ->
                            (match peelint chs with
                             | None -> lex' chs (MinusTok::t)
                             | Some(i, r) -> lex' r (IntTok(i)::NegTok::t)
                        ))
               | _ ->
                        (match peelint c with
                        | None -> raise LexError
                        | Some(i, r) -> lex' r (IntTok(i)::t)))

(* lex : string -> token list
 * REQUIRES: true
 * ENSURES: lex s converts s into a stream of tokens.
 *)
let rec lex (s : string) : token list = List.rev (lex' (explode s) [])

(* lextests: unit -> unit
 * REQUIRES: true
 * ENSURES: if all tests pass, lextests () prints nothing, otherwise it
 *          notes which tests failed.
 *)
let lextests () : unit =
    let explode_tests =
        (explode "" = []) &&
        (explode "123" = ['1';'2';'3']) in
    if not explode_tests then print_endline "explode tests failed";

    let charlist_to_int_tests =
        (charlist_to_int [] 0 = None) &&
        (charlist_to_int ['#'] 0 = None) &&
        (charlist_to_int ['3'] 0 = Some 3) &&
        (charlist_to_int ['-';'3'] 0 = Some (-3)) &&
        (charlist_to_int ['3';'1';'4';'0';'5'] 0 = Some 31405) &&
        (charlist_to_int ['3';'1';'-';'0';'5'] 0 = None) in
    if not charlist_to_int_tests
    then print_endline "charlist_to_int tests failed";

    let peelint_tests =
        (peelint [] = None) &&
        (peelint ['-'] = None) &&
        (peelint ['x'] = None) &&
        (peelint ['2'] = Some (2, [])) &&
        (peelint ['-';'2'] = Some (-2, [])) &&
        (peelint ['2';'-'] = Some (2, ['-'])) &&
        (peelint ['2';'-';'3'] = Some (2, ['-';'3'])) &&
        (peelint ['2';'-';'a'] = Some (2, ['-';'a'])) &&
        (peelint ['2';'5';'1';'x'] = Some (251, ['x'])) &&
        (peelint ['-';'2';'5';'1';'x'] = Some (-251, ['x'])) in
    if not peelint_tests then print_endline "peelint tests failed";

    let lex_tests =
        (lex "" = []) &&
        (lex "2" = [IntTok 2]) &&
        (lex "-2" = [NegTok;IntTok (2)]) &&
        (lex "2+2" = [IntTok 2;PlusTok;IntTok 2]) &&
        (lex "2--3" = [IntTok 2;MinusTok;NegTok;IntTok (3)]) &&
        (lex "(1+2)*3/4^5%6" =
            [LparTok;IntTok 1;PlusTok;IntTok 2;RparTok;
             TimesTok;IntTok 3;DivTok;IntTok 4;ExpTok;
             IntTok 5;ModTok;IntTok 6]) in
    if not lex_tests then print_endline "lex tests failed";;

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
    let parse (t : token list) : expr option =
      match (try parse' t [] [] with | _ -> []) with
            | [] -> None 
            | [e] -> Some(e)
            | _   -> None 

    (* compile : string -> expr option
     * REQUIRES: true
     * ENSURES: compile s yields the operator precedence parsing of the token
     *          stream that results when s is lexed. If no valid parsing exists
     *          compile s raises an exception.
     *)
    let compile (s : string) : expr option = parse (try lex s with _ -> [])

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

    
