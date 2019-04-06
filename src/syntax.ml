open Core

module Syntax =
struct

  type pos = int * int

  type exp = NUM of int * pos
           | ID of string * pos
           | EMPTY
           | CONC of exp * exp * pos
           | FROMTO of exp * exp * pos
           | CHOOSE of exp * pos
           | DIFFERENT of exp * pos
           | PLUS of exp * exp * pos
           | MINUS of exp * exp * pos
           | TIMES of exp * exp * pos
           | DIVIDE of exp * exp * pos
           | MOD of exp * exp * pos
           | UMINUS of exp * pos
           | SIGN of exp * pos
           | D of exp * pos
           | Z of exp * pos
           | SUM of exp * pos
           | COUNT of exp * pos
           | LEAST of exp * exp * pos
           | LARGEST of exp * exp * pos
           | MINIMAL of exp * pos
           | MAXIMAL of exp * pos
           | HASH of exp * exp * pos
           | AND of exp * exp * pos
           | EQ of exp * exp * pos
           | NEQ of exp * exp * pos
           | LT of exp * exp * pos
           | GT of exp * exp * pos
           | LE of exp * exp * pos
           | GE of exp * exp * pos
           | DROP of exp * exp * pos
           | KEEP of exp * exp * pos
           | PICK of exp * exp * pos
           | SETMINUS of exp * exp * pos
           | MEDIAN of exp * pos
           | LET of string * exp * exp * pos
           | REPEAT of string * exp * exp * bool * pos
           | ACCUM of string * exp * exp * bool * pos
           | FOREACH of string * exp * exp * pos
           | IF of exp * exp * exp * pos
           | CALL of string * exp list * pos
           | STRING of string * pos
           | SAMPLE of exp * pos
           | SAMPLES of exp * exp * pos
           | HCONC of exp * exp * pos
           | VCONCL of exp * exp * pos
           | VCONCR of exp * exp * pos
           | VCONCC of exp * exp * pos
           | QUESTION of float * pos
           | PAIR of exp * exp * pos
           | FIRST of exp * pos
           | SECOND of exp * pos
           | DEFAULT of string * exp * pos
  and
    declaration = Func of (string list * exp * pos)
                | Comp of (exp * string * string * pos)

  and program = (string * declaration) list * exp

  let rec showExp exp =
    match exp with
      NUM (i, _) -> string_of_int i
    | EMPTY -> "{}"
    | ID (x, _) -> x
    | CONC (e1, e2, _) -> "{" ^ showExp e1 ^ ", " ^ showExp e2 ^ "}"
    | FROMTO (e1, e2, _) -> "(" ^ showExp e1 ^ ".." ^ showExp e2 ^ ")"
    | CHOOSE (e1, _) -> "(choose " ^ showExp e1 ^ ")"
    | DIFFERENT (e1, _) -> "(different " ^ showExp e1 ^ ")"
    | PLUS (e1, e2, _) -> "(" ^ showExp e1 ^ "+" ^ showExp e2 ^ ")"
    | MINUS (e1, e2, _) -> "(" ^ showExp e1 ^ "-" ^ showExp e2 ^ ")"
    | TIMES (e1, e2, _) -> "(" ^ showExp e1 ^ "*" ^ showExp e2 ^ ")"
    | DIVIDE (e1, e2, _) -> "(" ^ showExp e1 ^ "/" ^ showExp e2 ^ ")"
    | MOD (e1, e2, _) -> "(" ^ showExp e1 ^ " mod " ^ showExp e2 ^ ")"
    | UMINUS (e1, _) -> "(- " ^ showExp e1 ^ ")"
    | D (e1, _) -> "(D " ^ showExp e1 ^ ")"
    | Z (e1, _) -> "(Z " ^ showExp e1 ^ ")"
    | SIGN (e1, _) -> "(sgn " ^ showExp e1 ^ ")"
    | SUM (e1, _) -> "(sum " ^ showExp e1 ^ ")"
    | COUNT (e1, _) -> "(count " ^ showExp e1 ^ ")"
    | LEAST (e1, e2, _) -> "(least " ^ showExp e1 ^ " " ^ showExp e2 ^ ")"
    | LARGEST (e1, e2, _) -> "(largest " ^ showExp e1 ^ " " ^ showExp e2 ^ ")"
    | MINIMAL (e1, _) -> "(min " ^ showExp e1 ^ ")"
    | MAXIMAL (e1, _) -> "(max " ^ showExp e1 ^ ")"
    | HASH (e1, e2, _) -> "(" ^ showExp e1 ^ " # " ^ showExp e2 ^ ")"
    | AND (e1, e2, _) -> "(" ^ showExp e1 ^ " & " ^ showExp e2 ^ ")"
    | EQ (e1, e2, _) -> "(" ^ showExp e1 ^ " = " ^ showExp e2 ^ ")"
    | NEQ (e1, e2, _) -> "(" ^ showExp e1 ^ " =/= " ^ showExp e2 ^ ")"
    | LT (e1, e2, _) -> "(" ^ showExp e1 ^ " < " ^ showExp e2 ^ ")"
    | GT (e1, e2, _) -> "(" ^ showExp e1 ^ " > " ^ showExp e2 ^ ")"
    | LE (e1, e2, _) -> "(" ^ showExp e1 ^ " <= " ^ showExp e2 ^ ")"
    | GE (e1, e2, _) -> "(" ^ showExp e1 ^ " >= " ^ showExp e2 ^ ")"
    | DROP (e1, e2, _) -> "(" ^ showExp e1 ^ " drop " ^ showExp e2 ^ ")"
    | KEEP (e1, e2, _) -> "(" ^ showExp e1 ^ " keep " ^ showExp e2 ^ ")"
    | PICK (e1, e2, _) -> "(" ^ showExp e1 ^ " pick " ^ showExp e2 ^ ")"
    | SETMINUS (e1, e2, _) -> "(" ^ showExp e1 ^ " -- " ^ showExp e2 ^ ")"
    | MEDIAN (e1, _) -> "(median " ^ showExp e1 ^ ")"
    | LET (x, e1, e2, _) ->
      "(" ^ x ^ " := " ^ showExp e1 ^ ";\n" ^ showExp e2 ^ ")"
    | REPEAT (x, e1, e2, b, _) ->
      "(repeat " ^ x ^ " := " ^ showExp e1 ^
      (if b then " while " else " until ") ^ showExp e2 ^ ")"
    | ACCUM (x, e1, e2, b, _) ->
      "(accumulate " ^ x ^ " := " ^ showExp e1 ^
      (if b then " while " else " until ") ^ showExp e2 ^ ")"
    | FOREACH (x, e1, e2, _) ->
      "(foreach " ^ x ^ " in" ^ showExp e1 ^ " do " ^ showExp e2 ^ ")"
    | IF (e1, e2, e3, _) ->
      "(if " ^ showExp e1 ^ "\nthen " ^ showExp e2 ^ "\nelse " ^ showExp e3 ^ ")"
    | CALL (f, es, _) ->
      "call " ^ f ^ "(" ^
      String.concat ~sep:"" (List.map es ~f:(fun e -> showExp e ^ ",")) ^ ")"
    | STRING (s, _) -> "\"" ^ s ^ "\""
    | SAMPLE (e1, _) -> "'( " ^ showExp e1 ^ ")"
    | SAMPLES (e1, e2, _) -> "(" ^ showExp e1 ^ " ' " ^ showExp e2 ^ ")"
    | HCONC (e1, e2, _) -> "(" ^ showExp e1 ^ " || " ^ showExp e2 ^ ")"
    | VCONCL (e1, e2, _) -> "(" ^ showExp e1 ^ " |> " ^ showExp e2 ^ ")"
    | VCONCR (e1, e2, _) -> "(" ^ showExp e1 ^ " <| " ^ showExp e2 ^ ")"
    | VCONCC (e1, e2, _) -> "(" ^ showExp e1 ^ " <> " ^ showExp e2 ^ ")"
    | QUESTION (q, _) -> "?" ^ string_of_float q
    | PAIR (e1, e2, _) -> "[" ^ showExp e1 ^ " , " ^ showExp e2 ^ "]"
    | FIRST (e1, _) -> "%1( " ^ showExp e1 ^ ")"
    | SECOND (e1, _) -> "%2( " ^ showExp e1 ^ ")"
    | DEFAULT (x,e1, _) -> "(" ^ x ^" ~ " ^ showExp e1 ^ ")"
end
