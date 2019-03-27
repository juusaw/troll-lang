open Lexing
open Interpreter
open Parser

module Main =
struct

  exception ParamErr of string

  let percent = ref true

  let pmax = ref 1.0

  let graph = ref 0.0

  let col2 = ref "ge"

  let createLexerStream ( is (* : BasicIO.instream *) ) =
    Lexing.from_channel is

  let print s = Pervasives.print_string s

  let tupleSnd x = let _, y = x in y

  let tabulate n f =  List.init n (fun x -> f(x + 1))

  let implode l = String.concat "" (List.map (fun x -> String.make 1 x) l)

  let rec stringVal l =
    concatenate
      (List.map
         (fun n -> if n>=0 then string_of_int n
           else "-" ^ string_of_int (~-n))
         l)

  and concatenate = function
    | [] -> ""
    | [x] -> x
    | (x :: xs) -> x ^ " " ^ concatenate xs

  let realtostring n = if n<0.0 then "-" ^ string_of_float (~-.n)
    else string_of_float n

  let padrealtostring p =
    let padding = "                 " in 
    let ps = if p < 1000000000000000.0
      then "  0.0"
      else if p<9.999999
      then "  "^realtostring p
      else if p<100.0
      then " "^realtostring p
      else realtostring p
    in
    ps ^ String.sub padding 0 (20-String.length ps)

  let rec stringIVal = function 
    | Interpreter.VAL l -> stringVal l
    | Interpreter.TEXT ts ->
      String.concat "" (List.map (fun t -> t ^"\n") ts)
    | Interpreter.PAIR (v,w) ->
      "[" ^ stringIVal v ^ ", " ^ stringIVal w ^ "]"

  let printVal v = print (stringIVal v ^"\n")

  let rec gtList a b = match a,b with 
    | [], _ -> false
    | (a::l1), [] -> true
    | (a::l1), (b::l2) -> a>b || a=b && gtList l1 l2

  let rec printDist1 a b = 
    match (a, b) with
    | [], pad -> ()
    | ((a,p)::l), pad ->
      let s1 = stringIVal a in
      let s2 = match a with
          Interpreter.VAL _ ->
          String.sub pad 0 (String.length pad - String.length s1)
        | _ -> "" in
      let s2' = match a with
          Interpreter.VAL _ -> ""
        | _ -> pad in
      let s3 = padrealtostring p in
      let sumTwo x y = x +. y in
      let pgeq: float = match (!col2) with
          "ge" -> p +. List.fold_right sumTwo (List.map tupleSnd l) 0.0
        | "le" -> !pmax -. List.fold_right sumTwo (List.map tupleSnd l) 0.0
        | "gt" -> List.fold_right sumTwo (List.map tupleSnd l) 0.0
        | _    -> !pmax -. p -. List.fold_right sumTwo (List.map tupleSnd l) 0.0
      in
      let s4 = padrealtostring pgeq in
      let rec bar p = if (!graph)*.p<0.5 then []
        else '|' :: bar (p-.1.0/.(!graph))
      in (if !graph=0.0
          then print (s2 ^ s1 ^ s2' ^ ":  " ^ s3 ^ s4 ^ "\n")
          else  print (s2 ^ s1 ^ s2' ^ ":  " ^ (String.concat "" (List.map (fun x -> String.make 1 x) (bar p))) ^ "\n");
          printDist1 l pad)

  let printDist l =
    let myAdd = function
        ((Interpreter.VAL (n::_),p),s) -> p*.(float_of_int n)+.s
      | (_,s) -> s in
    let myAdd2 = function
        ((Interpreter.VAL (n::_),p),s) ->
        p*.(float_of_int n)*.(float_of_int n)+.s
      | (_,s) -> s in
    let myAdd3 a b = match a,b with
        m, ((Interpreter.VAL (n::_),p),s) ->
        p*.(abs_float (float_of_int n -. m))+.s
      | m, (_,s) -> s in
    let maximum x y = if x > y then x else y in
    let maxLen = List.fold_right maximum
        (List.map (fun x -> match x with
               (Interpreter.VAL a,_) ->
               String.length (stringVal a)
             | (Interpreter.TEXT ts,_) ->
               List.fold_right maximum
                 (List.map String.length ts) 0
             | (pair,_) -> String.length (stringIVal pair))
            l) 0 in
    let pad = implode (tabulate (maxLen+5) (fun x -> ' ')) in
    let s1 = "Value" in
    let s2 = String.sub pad 0 (String.length pad - String.length s1) in
    let s3 = if !percent
      then "    % =              "
      else "  Probability for =  " in
    let s4 = match (!percent,!col2) with
        (true,"ge")  -> "   % >=              "
      | (false,"ge") -> " Probability for >=  "
      | (true,"le")  -> "   % <=              "
      | (false,"le") -> " Probability for <=  "
      | (true,"lt")  -> "   % <               "
      | (false,"lt") -> " Probability for <   "
      | (true,_)     -> "   % >               "
      | (false,_)    -> " Probability for >   " in
    let mean = (if List.for_all (fun x ->
        (match x with
           (Interpreter.VAL x, p) -> List.length x <= 1
         | _ -> false)) l
       then
         let m = List.fold_right (fun x y -> myAdd (x, y)) l 0.0 in
         let m2 = List.fold_right (fun x y -> myAdd2 (x, y)) l 0.0 in
         let md = List.fold_right (fun x y -> myAdd3 m (x, y)) l 0.0 in
         Some (m, sqrt(m2-.m*.m), md)
       else None) in
    let s5 = match mean with
        None -> ""
      | Some (m,sp,md) ->
        "\nAverage = " ^ realtostring m ^
        "    Spread = " ^ realtostring sp ^
        "  Mean deviation = " ^ realtostring md ^ "\n" in
    let l1 = if !percent
      then (pmax := 100.0; List.map (fun (a,p) -> (a,100.0*.p)) l)
      else l
    in
    (if !graph=0.0
     then print (s2 ^ s1 ^ s3 ^ s4 ^ "\n")
     else print (s2 ^ s1 ^"   "^ string_of_float (!graph) ^" bars per %\n");
     printDist1 l1 pad;
     print s5)

  let run filename n defs =
    let lb = createLexerStream
        (match filename with
           Some (filename) -> (open_in filename)
         | None -> stdin) in
    let dice = 
      let (decls,exp) = Parser.dice Lexer.token lb in
      (decls, defs exp) in
    let roll = fun n -> printVal (Interpreter.rollDice dice) in
    List.hd (tabulate n roll)

  let errorMess s = print s (* TODO: To stderr? *)

  let findDef str =
    match String.split_on_char '=' str with
      [name;valString] -> (match int_of_string_opt valString with
          None -> None
        | Some (value) -> Some (name,value))
    | _ -> None

  let rec run0 a b c d = match a,b,c,d with
      [], filename, n, defs -> run filename n defs
    | (arg::args), filename, n, defs ->
      match int_of_string_opt arg with
        None -> (match findDef arg with
            None ->
            if arg = "-p"
            then (percent := not (!percent);
                  run0 args filename n defs)
            else if "-g" <= arg && arg < "-h"
            then (match float_of_string_opt (String.sub arg 0 2 ) with
                  None -> raise (ParamErr arg)
                | Some s ->
                  (graph := s;
                   run0 args filename n defs))
            else if arg = "ge" || arg = "le" ||
                    arg = "gt" || arg = "lt"
            then (col2 := arg;
                  run0 args filename n defs)
            else run0 args (Some arg) n defs
          | Some (name,value) ->
            run0 args filename n
              (fun d -> Syntax.Syntax.LET
                  (name, Syntax.Syntax.NUM (value,(0,0)),
                   defs d,(0,0))))
      | Some n -> run0 args filename n defs

  let main filename = try
      run0 [] filename 1 (fun d -> d)
    with Parsing.YYexit ob -> errorMess "Parser-exit\n"
       | Parsing.Parse_error ->
         let (p1,p2) = (0, 0) in
         let (lin,col)
           = Lexer.getLineCol p2
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
       | ParamErr s -> errorMess ("Bad command-line parameter: " ^ s)

  let spec =
    let open Core.Command.Spec in
    empty
    +> anon (maybe ("filename" %: string))

  let command =
    Core.Command.basic
      ~summary:"Simulate dice rolling based on a domain-specific syntax"
      ~readme:(fun () -> "Command-line options")
      spec
      (fun filename () -> main filename)

  let _ =
    Random.self_init();
    Core.Command.run ~version:"0.0.1" command
end

