type token =
    | IntTok of int
    | LparTok
    | RparTok
    | PlusTok
    | MinusTok
    | TimesTok
    | DivTok
    | ExpTok
    | ModTok

exception LexError
exception DigitError

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
    | ch::chs ->
            (match (ch, isdigit ch) with
            | (_, true) -> charlist_to_int chs (10 * acc + digit_to_int ch)
            | ('-' , _) ->
                    (match charlist_to_int chs acc with
                     | None -> None
                     | Some(n) -> Some(-1 * n))
            | (_, _) -> None)

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
                  (match peelint c with
                    | None -> MinusTok::t
                    | Some(i, r) -> lex' r (IntTok(i)::t)
                  ))
               | _ ->
                (match peelint c with
                    | None -> raise LexError
                    | Some(i, r) -> lex' r (IntTok(i)::t)))

let rec lex (s : string) : token list = List.rev (lex' (explode s) [])

let lextests () : bool = true

