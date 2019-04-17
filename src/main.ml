open Base
open Lexing
open Interpreter
open Parser

module Main =
struct

  let print s = Pervasives.print_string s

  let times n f =  List.init n ~f:(fun _ -> f())

  let stringVal l =
    String.concat
      ~sep:" "
      (List.map
         ~f:(fun n -> if n >= 0 then string_of_int n
              else "-" ^ string_of_int (~-n))
         l)

  let rec stringIVal = function
    | Interpreter.VAL l -> stringVal l
    | Interpreter.TEXT ts ->
      String.concat ~sep:"" (List.map ~f:(fun t -> t ^"\n") ts)
    | Interpreter.PAIR (v,w) ->
      "[" ^ stringIVal v ^ ", " ^ stringIVal w ^ "]"

  let printVal v = print (stringIVal v ^"\n")

  let run filename n defs =
    let lb = Lexing.from_channel
        (match filename with
           Some (filename) -> (In_channel.create filename)
         | None -> In_channel.stdin) in
    let dice =
      let (decls,exp) = Parser.dice Lexer.token lb in
      (decls, defs exp) in
    let roll = fun _ -> printVal (Interpreter.rollDice (Syntax.Syntax.optimize_tco dice)) in
    List.hd (times n roll)

  let errorMess s = print s (* TODO: To stderr? *)

  let findDef str =
    match String.split_on_chars ~on:['='] str with
      [name;valString] -> (match int_of_string_opt valString with
          None -> None
        | Some (value) -> Some (name,value))
    | _ -> None

  let main filename count seed =
    let () = match seed with
        Some s -> Random.init s
      | None -> Random.self_init () in
    try
      match run filename count (fun d -> d) with
        _ -> ()
    with Parsing.YYexit _ -> errorMess "Parser-exit\n"
       | Parser.Error ->
         let (lin,col)
           = Lexer.getLineCol 0
             (!Lexer.currentLine)
             (!Lexer.lineStartPos) in
         errorMess ("Parse-error at line "
                    ^ string_of_int lin ^ ", column " ^ string_of_int col)
       | Lexer.LexicalError (mess,(lin,col)) ->
         errorMess ("Lexical error: " ^mess^ " at line "
                    ^ string_of_int lin ^ ", column " ^ string_of_int col)
       | Interpreter.RunError (mess,(lin,col)) ->
         errorMess ("Runtime error: " ^mess^ " at line "
                    ^ string_of_int lin ^ ", column " ^ string_of_int col)
       | Sys_error s -> errorMess ("Exception: " ^ s)

end
