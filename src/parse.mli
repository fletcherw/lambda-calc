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

(* throws ParseError when parsing fails *)
val parse : Lex.token list -> expr

val run_tests : unit -> unit
