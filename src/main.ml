open Core
open Lexing
open Interpreter
open Parser

module Main =
struct

  let percent = ref true

  let pmax = ref 1.0

  let graph = ref 0.0

  let col2 = ref "ge"

  let createLexerStream ( is (* : BasicIO.instream *) ) =
    Lexing.from_channel is

  let print s = Pervasives.print_string s

  let tupleSnd x = let _, y = x in y

  let tabulate n f =  List.init n ~f:(fun x -> f(x + 1))

  let implode l = String.concat ~sep:"" (List.map ~f:(fun x -> String.make 1 x) l)

  let stringVal l =
    String.concat
      ~sep:" "
      (List.map
         ~f:(fun n -> if n >= 0 then string_of_int n
              else "-" ^ string_of_int (~-n))
         l)

  let realtostring n = if n < 0.0 then "-" ^ string_of_float (~-.n)
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
    ps ^ String.sub padding ~pos:0 ~len:(20-String.length ps)

  let rec stringIVal = function
    | Interpreter.VAL l -> stringVal l
    | Interpreter.TEXT ts ->
      String.concat ~sep:"" (List.map ~f:(fun t -> t ^"\n") ts)
    | Interpreter.PAIR (v,w) ->
      "[" ^ stringIVal v ^ ", " ^ stringIVal w ^ "]"

  let printVal v = print (stringIVal v ^"\n")

  let rec gtList l1 l2 = match l1, l2 with
    | [], _ -> false
    | (_::_), [] -> true
    | (a::l1), (b::l2) -> a>b || a=b && gtList l1 l2

  let rec printDist1 a b =
    match (a, b) with
    | [], _ -> ()
    | ((a,p)::l), pad ->
      let s1 = stringIVal a in
      let s2 = match a with
          Interpreter.VAL _ ->
          String.sub pad ~pos:0 ~len:(String.length pad - String.length s1)
        | _ -> "" in
      let s2' = match a with
          Interpreter.VAL _ -> ""
        | _ -> pad in
      let s3 = padrealtostring p in
      let sumTwo x y = x +. y in
      let pgeq: float = match (!col2) with
          "ge" -> p +. List.fold_right ~f:sumTwo (List.map ~f:tupleSnd l) ~init:0.0
        | "le" -> !pmax -. List.fold_right ~f:sumTwo (List.map ~f:tupleSnd l) ~init:0.0
        | "gt" -> List.fold_right ~f:sumTwo (List.map ~f:tupleSnd l) ~init:0.0
        | _    -> !pmax -. p -. List.fold_right ~f:sumTwo (List.map ~f:tupleSnd l) ~init:0.0
      in
      let s4 = padrealtostring pgeq in
      let rec bar p = if (!graph)*.p<0.5 then []
        else '|' :: bar (p-.1.0/.(!graph))
      in (if !graph=0.0
          then print (s2 ^ s1 ^ s2' ^ ":  " ^ s3 ^ s4 ^ "\n")
          else  print (s2 ^ s1 ^ s2' ^ ":  " ^ (String.concat ~sep:"" (List.map ~f:(fun x -> String.make 1 x) (bar p))) ^ "\n");
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
        p*.(Float.abs (float_of_int n -. m))+.s
      | _, (_,s) -> s in
    let maximum x y = if x > y then x else y in
    let maxLen = List.fold_right ~f:maximum
        (List.map
           ~f:(fun x -> match x with
                 (Interpreter.VAL a,_) ->
                 String.length (stringVal a)
               | (Interpreter.TEXT ts,_) ->
                 List.fold_right ~f:maximum
                   (List.map ~f:String.length ts) ~init:0
               | (pair,_) -> String.length (stringIVal pair))
           l) ~init:0 in
    let pad = implode (tabulate (maxLen+5) (fun _ -> ' ')) in
    let s1 = "Value" in
    let s2 = String.sub pad ~pos:0 ~len:(String.length pad - String.length s1) in
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
    let mean = (if List.for_all ~f:(fun x ->
        (match x with
           (Interpreter.VAL x, _) -> List.length x <= 1
         | _ -> false)) l
       then
         let m = List.fold_right ~f:(fun x y -> myAdd (x, y)) l ~init:0.0 in
         let m2 = List.fold_right ~f:(fun x y -> myAdd2 (x, y)) l ~init:0.0 in
         let md = List.fold_right ~f:(fun x y -> myAdd3 m (x, y)) l ~init:0.0 in
         Some (m, sqrt(m2-.m*.m), md)
       else None) in
    let s5 = match mean with
        None -> ""
      | Some (m,sp,md) ->
        "\nAverage = " ^ realtostring m ^
        "    Spread = " ^ realtostring sp ^
        "  Mean deviation = " ^ realtostring md ^ "\n" in
    let l1 = if !percent
      then (pmax := 100.0; List.map ~f:(fun (a,p) -> (a,100.0*.p)) l)
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
           Some (filename) -> (In_channel.create filename)
         | None -> In_channel.stdin) in
    let dice =
      let (decls,exp) = Parser.dice Lexer.token lb in
      (decls, defs exp) in
    let roll = fun _ -> printVal (Interpreter.rollDice dice) in
    List.hd (tabulate n roll)

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

  let spec =
    let open Core.Command.Spec in
    empty
    +> anon (maybe ("filename" %: string))
    +> flag "--times" (optional_with_default 1 int) ~doc:"int Number of rolls"
    +> flag "--seed" (optional int) ~doc:"int Seed value for dice"

  let command =
    Core.Command.basic
      ~summary:"Simulate dice rolling based on a domain-specific syntax"
      ~readme:(fun () -> "Command-line options")
      spec
      (fun filename count seed () -> main filename count seed)

  let () =
    Core.Command.run ~version:"0.0.1" command
end

