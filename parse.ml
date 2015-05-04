type expr =
    | Int   of int
    | Plus  of expr * expr
    | Minus of expr * expr
    | Times of expr * expr
    | Div   of expr * expr
    | Exp   of expr * expr
    | Mod   of expr * expr

