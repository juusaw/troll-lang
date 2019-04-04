module Interpreter :
sig
  exception RunError of string * Syntax.Syntax.pos

  type value =
      VAL of int list
    | TEXT of string list
    | PAIR of value * value

  val rollDice :
    (string * Syntax.Syntax.declaration) list * Syntax.Syntax.exp -> value
end
