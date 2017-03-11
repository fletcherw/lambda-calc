type token =
  | IntTok of int
  | VarTok of string
  | LetTok
  | LparTok
  | RparTok
  | PlusTok
  | MinusTok
  | NegTok
  | TimesTok
  | DivTok
  | ExpTok
  | ModTok
  | EqTok

exception LexError of string

(* explode : string -> char list
 * explode s = s', where s' is the character list representation
 * of s as a string
 *)
let explode (s : string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i-1) (s.[i] :: l) in
  exp (String.length s - 1) []

(* is_digit : char -> bool
 * is_digit c == true iff c is a digit
 *)
let is_digit (c : char) : bool =
  let code = Char.code c in (48 <= code && code <= 57)

(* digit_to_int : char -> int
 * REQUIRES: c is a digit
 * (digit_to_int c) is the integer corresponding to c
 *)
let digit_to_int (c : char) : int =
  let code = Char.code c in
  if (48 <= code && code <= 57)
  then (code - 48)
  else raise (Failure "c was not a digit")

(* charlist_to_int : char list -> int -> int option
 * if c is a string of digits (optionally preceded with a minus sign),
 * then charlist_to_int c 0 yields i, where i is the decimal
 * representation of c.
 *)
let rec charlist_to_int (c : char list) (acc : int) : int option =
  match c with
    | [] -> None
    | [ch] ->
            if is_digit ch
            then Some (10 * acc + (digit_to_int ch))
            else None
    | ch1::ch2::chs ->
            (match (ch1, ch2, is_digit ch1) with
               | (_, '-', _) -> None
               | (_, _, true) -> charlist_to_int (ch2::chs)
                                                 (10 * acc + digit_to_int ch1)
               | ('-', _, _) ->
                        (match charlist_to_int (ch2::chs) acc with
                         | None -> None
                         | Some(n) -> Some(-1 * n))
               | _ -> None)

(* peel_int' : char list -> (char list * char list)
 * Returns the longest prefix of c that is made up of the characters '0' to '9',
 * as well as the rest of c
 *)
let rec peel_int' (c : char list) : (char list * char list) =
  match c with
    | c::cs when is_digit c ->
        let (digits, rest) = peel_int' cs in (c::digits, rest)
    | _ -> ([], c)

(* peel_int : char list -> (int * char list) option
 * If there is a prefix of c that represents a valid integer,
 * returns Some(c', r), where c' is the integer representation of c.
 * If no such prefix of c exists, returns None
 *)
let peel_int (c : char list) : (int * char list) option =
  match peel_int' c with
    | ([], _) -> None
    | (digits, rest) ->
            (match charlist_to_int digits 0 with
             | None -> None
             | Some(i) -> Some(i, rest))

(* can_begin_ident : char -> bool
 * returns true iff c can begin a variable name
 *)
let can_begin_ident (c : char) : bool =
  let code = Char.code c in
  (65 <= code && code <= 90) ||
  (97 <= code && code <= 122) ||
  (code = 95)

(* charlist_to_string : char list -> string
 * Converts the given list of characters into a string
 *)
let charlist_to_string (cl : char list) : string =
  String.concat "" (List.map (String.make 1) cl)

(* peel_ident : char list -> string * char list
 * Finds the longest prefix of chs that is a valid variable name
 * and returns that as a string, as well as the remainder of chs
 *)
let peel_ident (chs : char list) : string * char list =
  let rec peel_ident' chs acc =
    (match chs with
     | ch :: r when can_begin_ident ch || is_digit ch ->
       peel_ident' r (ch::acc)
     | _ -> (List.rev acc, chs))
  in
  let (ident_chars, rest) = peel_ident' chs [] in
  let ident = charlist_to_string ident_chars in
  (ident, rest)

(* lex' : char list -> token list -> token list
 * lex' c [] is the token stream that results from lexing c
 *)
let rec lex' (c : char list) (t : token list) : token list =
  match c with
    | [] -> List.rev t
    | ch :: chs when can_begin_ident ch ->
      let (s, r) = peel_ident (ch::chs) in
      if s = "let"
      then lex' r (LetTok::t)
      else lex' r (VarTok(s)::t)
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
               | '=' -> lex' chs (EqTok::t)
               | '-' -> lex' chs (MinusTok::t)
               | '.' ->
                       (match chs with
                          | next::_ when is_digit next ->
                                  raise (LexError "Decimal numbers not permitted")
                          | _ -> raise (LexError "Unidentified token '.'"))
               | _ ->
                        (match peel_int c with
                           | None ->
                              raise (LexError ("Unidentified token '" ^
                                                Char.escaped ch ^ "'"))
                           | Some(i, r) -> lex' r (IntTok(i)::t)))

(* lex : string -> token list
 * lex s converts s into a list of lexed tokens.
 *)
let rec lex (s : string) : token list = lex' (explode s) []

let token_list_to_string (tokens : token list) : string =
  let name (tok : token) : string =
    match tok with
      | IntTok i -> "IntTok " ^ (string_of_int i)
      | VarTok i -> "VarTok " ^ i
      | LetTok   -> "LetTok"
      | LparTok  -> "LparTok"
      | RparTok  -> "RparTok"
      | PlusTok  -> "PlusTok"
      | MinusTok -> "MinusTok"
      | NegTok   -> "NegTok"
      | TimesTok -> "TimesTok"
      | DivTok   -> "DivTok"
      | ExpTok   -> "ExpTok"
      | ModTok   -> "ModTok"
      | EqTok    -> "EqTok"
  in
    "[" ^ (String.concat ", " (List.map name tokens)) ^ "]"

(* run_tests: unit -> unit
 * if all tests pass, run_tests () does nothing, otherwise it
 * prints which tests failed
 *)
let run_tests (() : unit) : unit =
  let test_c2i = Util.run_test (fun (a, b) ->
                                charlist_to_string a ^ ", " ^ string_of_int b)
                               (fun x -> match x with
                                           | None -> "None"
                                           | Some i -> string_of_int i)
                               "charlist_to_int"
                               (fun (a, b) -> charlist_to_int a b) in

  test_c2i ([], 0) None;
  test_c2i (['#'], 0) None;
  test_c2i (['3'], 0) (Some 3);
  test_c2i (['-';'3'], 0) (Some (-3));
  test_c2i (['3';'1';'4';'0';'5'], 0) (Some 31405);
  test_c2i (['3';'1';'-';'0';'5'], 0) None;

  let test_pi = Util.run_test charlist_to_string
                              (fun x -> match x with
                                          | None -> "None"
                                          | Some (i, r) ->
                                              string_of_int i ^ ", " ^
                                              charlist_to_string r)
                              "peel_int"
                              peel_int in

  test_pi [] None;
  test_pi ['-'] None;
  test_pi ['x'] None;
  test_pi ['2'] (Some (2, []));
  test_pi ['2';'-'] (Some (2, ['-']));
  test_pi ['2';'-';'3'] (Some (2, ['-';'3']));
  test_pi ['2';'-';'a'] (Some (2, ['-';'a']));
  test_pi ['2';'5';'1';'x'] (Some (251, ['x']));

  let test_lex = Util.run_test (fun x -> x)
                               token_list_to_string
                               "lex"
                               lex in

  test_lex "" [];
  test_lex "2" [IntTok 2];
  test_lex "-2" [MinusTok;IntTok (2)];
  test_lex "2+2" [IntTok 2;PlusTok;IntTok 2];
  test_lex "2--3" [IntTok 2;MinusTok;MinusTok;IntTok (3)];
  test_lex "(1+2)*3/4^5%6"
    [LparTok;IntTok 1;PlusTok;IntTok 2;RparTok;
     TimesTok;IntTok 3;DivTok;IntTok 4;ExpTok;
     IntTok 5;ModTok;IntTok 6];
  test_lex "let x2yz = 2x + 7"
    [LetTok;VarTok "x2yz";EqTok;IntTok 2;
     VarTok "x";PlusTok;IntTok 7];
