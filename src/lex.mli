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

(* lex : string -> token list
 * ENSURES: lex s converts s into a stream of tokens.
 *)
val lex : string -> token list 

(* lextests: unit -> unit
 * ENSURES: if all tests pass, lextests () prints nothing, otherwise it
 *          notes which tests failed.
 *)
val run_tests : unit -> unit
