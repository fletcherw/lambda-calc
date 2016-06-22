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

exception LexError

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
let run_tests () : unit =
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
