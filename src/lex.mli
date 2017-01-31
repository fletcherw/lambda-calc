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

(* lex : string -> token list
 * ENSURES: lex s converts s into a stream of tokens.
 *)
val lex : string -> token list 

(* token_list_to_string : token list -> string
 * ENSURES: token_list_to_string t is a human-readable representation of t
 *)
val token_list_to_string : token list -> string

(* run_tests : unit -> unit
 * ENSURES: if all tests pass, run_tests () does nothing, otherwise it
 *          trips an assertion failure on the first failing test.
 *)
val run_tests : unit -> unit
