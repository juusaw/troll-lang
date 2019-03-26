module Interpreter :
sig
  exception RunError of string * Syntax.Syntax.pos
  type value =
      VAL of int list
    | TEXT of string list
    | PAIR of value * value
  val sign : int -> int
  val take : int -> 'a list -> 'a list
  val drop' : int -> 'a list -> 'a list
  val last : 'a list -> 'a
  val is_empty : 'a list -> bool
  val zip : 'a list -> 'b list -> ('a * 'b) list
  val tabulate : int -> (int -> 'a) -> 'a list
  val rand : int -> int
  val tryFn : float -> bool
  val tryFn' : float -> int -> bool
  val merge : 'a list -> 'a list -> 'a list
  val merge1 : 'a -> 'a list -> 'a list -> 'a list
  val member : 'a -> 'a list -> bool
  val drop : 'a list -> 'a list -> 'a list
  val keep : 'a list -> 'a list -> 'a list
  val drop1 : 'a -> 'a list -> 'a list
  val setminus : 'a list -> 'a list -> 'a list
  val pick : int -> int -> 'a list -> 'a list
  val lookup : 'a -> ('a * 'b) list -> 'b option
  val rollDice :
    (string * Syntax.Syntax.declaration) list * Syntax.Syntax.exp -> value
  val evalExp0 :
    Syntax.Syntax.exp ->
    (string * value) list ->
    (string * Syntax.Syntax.declaration) list -> value
  val iterate :
    int list ->
    string ->
    Syntax.Syntax.exp ->
    Syntax.Syntax.exp ->
    bool ->
    (string * value) list ->
    (string * Syntax.Syntax.declaration) list ->
    Syntax.Syntax.pos -> int list list
  val callFun :
    string * value list * (string * Syntax.Syntax.declaration) list *
    Syntax.Syntax.pos -> value
  val compositional :
    int list * Syntax.Syntax.exp * string * string *
    (string * Syntax.Syntax.declaration) list * Syntax.Syntax.pos -> 
    value
  val makeText : value -> value
  val concatenate : string list -> string
  val spaces : int -> string
  val hconc : value * value -> value
  val vconcl : value * value -> value
  val vconcr : value * value -> value
  val vconcc : value * value -> value
end
