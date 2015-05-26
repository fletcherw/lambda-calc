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
exception DigitError


(* lexing helper functions *)

let explode (s : string) : char list =
    let rec exp i l =
        if i < 0 then l else exp (i-1) (s.[i] :: l) in
    exp (String.length s - 1) []

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
    | _   -> raise DigitError

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
            | (_, _, true) -> charlist_to_int (ch2::chs) (10 * acc + digit_to_int ch1)
            | ('-', _, _) ->
                    (match charlist_to_int (ch2::chs) acc with
                     | None -> None
                     | Some(n) -> Some(-1 * n))
                    | (_, _, _) -> None)

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

let peelint (c : char list) : (int * char list) option =
    match peelint' c with
    | None -> None
    | Some(n, r) ->
            (match charlist_to_int n 0 with
            | None -> None
            | Some(i) -> Some(i, r))

            (* parse helper functions *)

let precedence (t : token) : int =
    match t with
    | IntTok(_) -> 0
    | LparTok   -> 0
    | RparTok   -> 0
    | PlusTok   -> 3
    | MinusTok  -> 3
    | NegTok    -> 6
    | TimesTok  -> 4
    | DivTok    -> 4
    | ExpTok    -> 5
    | ModTok    -> 4

type assoc = Left | Right | Neither

let associativity (t : token) : assoc =
    match t with
    | IntTok(_) -> Neither
    | LparTok   -> Neither
    | RparTok   -> Neither
    | PlusTok   -> Left
    | MinusTok  -> Left
    | NegTok    -> Left
    | TimesTok  -> Left
    | DivTok    -> Left
    | ExpTok    -> Right
    | ModTok    -> Left

let binop2expr (t : token) : ((expr * expr) -> expr) =
    match t with
    | PlusTok   -> fun (e1, e2) -> (Plus (e1, e2))
    | MinusTok  -> fun (e1, e2) -> (Minus (e1, e2))
    | TimesTok  -> fun (e1, e2) -> (Times (e1, e2))
    | DivTok    -> fun (e1, e2) -> (Div (e1, e2))
    | ExpTok    -> fun (e1, e2) -> (Exp (e1, e2))
    | ModTok    -> fun (e1, e2) -> (Mod (e1, e2))
    | _         -> raise (ParseError "binop2expr called on non-binary op")


let rec lex' (c : char list) (t : token list) : token list =
    match c with
       | [] -> t
       | ch :: chs ->
               (match ch with
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

let rec lex (s : string) : token list = List.rev (lex' (explode s) [])

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
    if not charlist_to_int_tests then print_endline "charlist_to_int tests failed";

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

let rec parse' (t : token list)
               (operands : expr list)
               (operators : token list) : expr list =
    (match t with
           | (IntTok i)::t' -> parse' t' ((Int i)::operands) operators
           | LparTok::t' -> parse' t' operands ((LparTok)::operators)
           | RparTok::t' ->
                   (match (operators, operands) with
                   | ([], _) -> raise (ParseError "Mismatched parentheses 1")
                   | (LparTok::ops, _) -> parse' t' operands ops
                   | (NegTok::ops, oper::opers) ->
                           parse' t' (Neg(oper)::opers) ops
                   | (op::ops, op1::op2::opers) ->
                           parse' t' (((binop2expr op) (op2, op1))::opers) ops
                   | (_, _) -> raise (ParseError "Missing operators 1"))
           | op1::t' ->
                   (match operators with
                      | [] -> parse' t' operands (op1::operators)
                      | op2::ops ->
                          (match (associativity op1,
                                  compare (precedence op1) (precedence op2)) with
                               | (Right, 0) -> parse' t' operands (op1::operators)
                               | (Right, 1) -> parse' t' operands (op1::operators)
                               | (Left,  1) -> parse' t' operands (op1::operators)
                               | (_, _) ->
                                   (match (op2, operands) with
                                   | (NegTok, o1::opers) ->
                                           parse' t ((Neg o1)::opers) operators
                                   | (_, o1::o2::opers) ->
                                           parse' t (((binop2expr op2)
                                           (o1, o2))::opers) operators
                                   | (_, _) -> raise
                                               (ParseError "Missing operators 2")
                                               )))
           | [] ->
                   (match (operators, operands) with
                       | ([], _) -> operands
                       | (LparTok::ops, _) ->
                               raise (ParseError "Mismatched parentheses 2")
                       | (RparTok::ops, _) ->
                               raise (ParseError "Mismatched parentheses 3")
                       | (NegTok::ops, oper::opers) ->
                               parse' t (Neg(oper)::opers) ops
                       | (op::ops, op1::op2::opers) ->
                               parse' t (((binop2expr op) (op2, op1))::opers) ops
                       | (_, _) -> raise (ParseError "Missing operators 3")))

    let parse (t : token list) : expr =
        match (parse' t [] []) with
            | [] -> raise (ParseError "No expression")
            | [e] -> e
            | _   -> raise (ParseError "Insufficient operators")


