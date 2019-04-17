open Base

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
           | LOOP of exp list * exp * exp * string list * pos
           | DEFAULT of string * exp * pos
  and
    declaration = Func of (string list * exp * pos)
                | Comp of (exp * string * string * pos)

  and program = (string * declaration) list * exp

  let rec string_of_exp exp =
    match exp with
      NUM (i, _) -> string_of_int i
    | EMPTY -> "{}"
    | ID (x, _) -> x
    | CONC (e1, e2, _) -> "{" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ "}"
    | FROMTO (e1, e2, _) -> "(" ^ string_of_exp e1 ^ ".." ^ string_of_exp e2 ^ ")"
    | CHOOSE (e1, _) -> "(choose " ^ string_of_exp e1 ^ ")"
    | DIFFERENT (e1, _) -> "(different " ^ string_of_exp e1 ^ ")"
    | PLUS (e1, e2, _) -> "(" ^ string_of_exp e1 ^ "+" ^ string_of_exp e2 ^ ")"
    | MINUS (e1, e2, _) -> "(" ^ string_of_exp e1 ^ "-" ^ string_of_exp e2 ^ ")"
    | TIMES (e1, e2, _) -> "(" ^ string_of_exp e1 ^ "*" ^ string_of_exp e2 ^ ")"
    | DIVIDE (e1, e2, _) -> "(" ^ string_of_exp e1 ^ "/" ^ string_of_exp e2 ^ ")"
    | MOD (e1, e2, _) -> "(" ^ string_of_exp e1 ^ " mod " ^ string_of_exp e2 ^ ")"
    | UMINUS (e1, _) -> "(- " ^ string_of_exp e1 ^ ")"
    | D (e1, _) -> "(D " ^ string_of_exp e1 ^ ")"
    | Z (e1, _) -> "(Z " ^ string_of_exp e1 ^ ")"
    | SIGN (e1, _) -> "(sgn " ^ string_of_exp e1 ^ ")"
    | SUM (e1, _) -> "(sum " ^ string_of_exp e1 ^ ")"
    | COUNT (e1, _) -> "(count " ^ string_of_exp e1 ^ ")"
    | LEAST (e1, e2, _) -> "(least " ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ ")"
    | LARGEST (e1, e2, _) -> "(largest " ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ ")"
    | MINIMAL (e1, _) -> "(min " ^ string_of_exp e1 ^ ")"
    | MAXIMAL (e1, _) -> "(max " ^ string_of_exp e1 ^ ")"
    | HASH (e1, e2, _) -> "(" ^ string_of_exp e1 ^ " # " ^ string_of_exp e2 ^ ")"
    | AND (e1, e2, _) -> "(" ^ string_of_exp e1 ^ " & " ^ string_of_exp e2 ^ ")"
    | EQ (e1, e2, _) -> "(" ^ string_of_exp e1 ^ " = " ^ string_of_exp e2 ^ ")"
    | NEQ (e1, e2, _) -> "(" ^ string_of_exp e1 ^ " =/= " ^ string_of_exp e2 ^ ")"
    | LT (e1, e2, _) -> "(" ^ string_of_exp e1 ^ " < " ^ string_of_exp e2 ^ ")"
    | GT (e1, e2, _) -> "(" ^ string_of_exp e1 ^ " > " ^ string_of_exp e2 ^ ")"
    | LE (e1, e2, _) -> "(" ^ string_of_exp e1 ^ " <= " ^ string_of_exp e2 ^ ")"
    | GE (e1, e2, _) -> "(" ^ string_of_exp e1 ^ " >= " ^ string_of_exp e2 ^ ")"
    | DROP (e1, e2, _) -> "(" ^ string_of_exp e1 ^ " drop " ^ string_of_exp e2 ^ ")"
    | KEEP (e1, e2, _) -> "(" ^ string_of_exp e1 ^ " keep " ^ string_of_exp e2 ^ ")"
    | PICK (e1, e2, _) -> "(" ^ string_of_exp e1 ^ " pick " ^ string_of_exp e2 ^ ")"
    | SETMINUS (e1, e2, _) -> "(" ^ string_of_exp e1 ^ " -- " ^ string_of_exp e2 ^ ")"
    | MEDIAN (e1, _) -> "(median " ^ string_of_exp e1 ^ ")"
    | LET (x, e1, e2, _) ->
      "(" ^ x ^ " := " ^ string_of_exp e1 ^ ";\n" ^ string_of_exp e2 ^ ")"
    | REPEAT (x, e1, e2, b, _) ->
      "(repeat " ^ x ^ " := " ^ string_of_exp e1 ^
      (if b then " while " else " until ") ^ string_of_exp e2 ^ ")"
    | ACCUM (x, e1, e2, b, _) ->
      "(accumulate " ^ x ^ " := " ^ string_of_exp e1 ^
      (if b then " while " else " until ") ^ string_of_exp e2 ^ ")"
    | FOREACH (x, e1, e2, _) ->
      "(foreach " ^ x ^ " in" ^ string_of_exp e1 ^ " do " ^ string_of_exp e2 ^ ")"
    | IF (e1, e2, e3, _) ->
      "(if " ^ string_of_exp e1 ^ "\nthen " ^ string_of_exp e2 ^ "\nelse " ^ string_of_exp e3 ^ ")"
    | CALL (f, es, _) ->
      "call " ^ f ^ "(" ^
      String.concat ~sep:"" (List.map es ~f:(fun e -> string_of_exp e ^ ",")) ^ ")"
    | STRING (s, _) -> "\"" ^ s ^ "\""
    | SAMPLE (e1, _) -> "'( " ^ string_of_exp e1 ^ ")"
    | SAMPLES (e1, e2, _) -> "(" ^ string_of_exp e1 ^ " ' " ^ string_of_exp e2 ^ ")"
    | HCONC (e1, e2, _) -> "(" ^ string_of_exp e1 ^ " || " ^ string_of_exp e2 ^ ")"
    | VCONCL (e1, e2, _) -> "(" ^ string_of_exp e1 ^ " |> " ^ string_of_exp e2 ^ ")"
    | VCONCR (e1, e2, _) -> "(" ^ string_of_exp e1 ^ " <| " ^ string_of_exp e2 ^ ")"
    | VCONCC (e1, e2, _) -> "(" ^ string_of_exp e1 ^ " <> " ^ string_of_exp e2 ^ ")"
    | QUESTION (q, _) -> "?" ^ string_of_float q
    | PAIR (e1, e2, _) -> "[" ^ string_of_exp e1 ^ " , " ^ string_of_exp e2 ^ "]"
    | FIRST (e1, _) -> "%1( " ^ string_of_exp e1 ^ ")"
    | SECOND (e1, _) -> "%2( " ^ string_of_exp e1 ^ ")"
    | LOOP (_) -> "Tailrec loop"
    | DEFAULT (x,e1, _) -> "(" ^ x ^" ~ " ^ string_of_exp e1 ^ ")"

  let string_of_decl = function
      Func (sl, exp, pos) -> string_of_exp exp
    | Comp (exp, s1, s2, pos) -> string_of_exp exp

  let optimize_tco (prog: program): program =
    let names_equal s1 s2 = Int.equal 0 (String.compare s1 s2) in
    let decls, e = prog in
    let decls' = List.map decls ~f:(fun (name, decl) ->
        match decl with
          Func (params, IF (condition_exp, CALL (fn_name, arg_exps, _), else_exp, p1), p2) when names_equal name fn_name ->
          (name, Func (params, LOOP (arg_exps, condition_exp, else_exp, params, p1), p2))
        | Func (params, IF (condition_exp, then_exp, CALL (fn_name, arg_exps, _), p1), p2) when names_equal name fn_name ->
          let inverse_condition = IF (condition_exp, EMPTY, NUM (1, p2), p2) in
          (name, Func (params, LOOP (arg_exps, inverse_condition, then_exp, params, p1), p2))
        | d -> (name, d)
      ) in
    (decls', e)
end
