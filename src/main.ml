open Base
open Lexing
open Interpreter
open Parser

module Main =
struct

  let print s = Caml.Pervasives.print_string s

  let times n f =  List.init n ~f:(fun _ -> f())

  let stringVal l =
    String.concat
      ~sep:" "
      (List.map
         ~f:(fun n -> if n >= 0 then Int.to_string n
              else "-" ^ Int.to_string (~-n))
         l)

  let rec stringIVal = function
    | Interpreter.VAL l -> stringVal l
    | Interpreter.TEXT ts ->
      String.concat ~sep:"" (List.map ~f:(fun t -> t ^"\n") ts)
    | Interpreter.PAIR (v,w) ->
      "[" ^ stringIVal v ^ ", " ^ stringIVal w ^ "]"

  let printVal v = print (stringIVal v ^"\n")

  let run lb n defs =
    let dice =
      let (decls,exp) = Parser.dice Lexer.token lb in
      (decls, defs exp) in
    let roll = fun _ ->  Interpreter.rollDice (Syntax.Syntax.optimize_tco dice) in
    List.hd (times n roll)

  let print_error = print (* TODO: To stderr? *)

  let findDef str =
    match String.split_on_chars ~on:['='] str with
      [name;valString] -> (match Caml.int_of_string_opt valString with
          None -> None
        | Some (value) -> Some (name,value))
    | _ -> None

  let main source count seed =
    let () = match seed with
        Some s -> Random.init s
      | None -> Random.self_init () in
    try
      match run source count (fun d -> d) with
        Some x -> printVal x
      | None -> ()
    with Caml.Parsing.YYexit _ -> print_error "Parser-exit\n"
       | Parser.Error ->
         let (lin,col)
           = Lexer.getLineCol 0
             (!Lexer.currentLine)
             (!Lexer.lineStartPos) in
         print_error ("Parse-error at line "
                      ^ Int.to_string lin ^ ", column " ^ Int.to_string col)
       | Lexer.LexicalError (mess,(lin,col)) ->
         print_error ("Lexical error: " ^mess^ " at line "
                      ^ Int.to_string lin ^ ", column " ^ Int.to_string col)
       | Interpreter.RunError (mess,(lin,col)) ->
         print_error ("Runtime error: " ^mess^ " at line "
                      ^ Int.to_string lin ^ ", column " ^ Int.to_string col)
       | Sys_error s -> print_error ("Exception: " ^ s)

end
