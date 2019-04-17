open Base
open Syntax

module Interpreter =
struct

  exception RunError of string * Syntax.pos

  type value = VAL of int list
             | TEXT of string list
             | PAIR of value * value

  (* TODO move elsewhere *)
  let sign n =
    match n with
    | 0 -> 0
    | _ -> (n / abs(n))

  let rec drop' n h =
    if Int.equal n 0
    then h
    else (drop' (n-1)
            (match h with
               _::b -> b
             | [] -> failwith "drop"))

  let is_empty = function
      [] -> true
    | _ -> false

  let tabulate n f =  List.init n ~f:(fun x -> f(x + 1))

  let rand n = 1 + Random.int n

  (* let rec tryFn p = tryFn' p 30 (* true with probability p *)

     and tryFn' p n = (* scale p until integral *)
     if n=0 then true
     else
      let p1 = Float.round_down (10.0 *. p) in
      let r = float_of_int (rand 10 - 1)
      in
      if r < p1 then true
      else if r > p1 then false
      else tryFn' (10.0 *. p -. p1) (n-1) *)

  let rec merge a b = match a, b with
    | [], l2 -> l2
    | (a::l1), l2 -> merge1 a l1 l2

  and merge1 a b c = match a, b, c with
    | a, l1, [] -> a :: l1
    | a, l1, (b::l2) ->
      if a<=b then a :: merge1 b l2 l1
      else b :: merge1 a l1 l2

  let rec member a b = match a, b with
    | _, [] -> false
    | a, (b::l2) -> a = b || member a l2

  let rec drop a b = match a, b with
    | [], _ -> []
    | (a::l1), l2 -> if member a l2 then drop l1 l2 else a :: drop l1 l2

  let rec keep a b = match a, b with
    | [], _ -> []
    | (a::l1), l2 ->
      if member a l2 then a :: keep l1 l2 else keep l1 l2

  let rec drop1 a b = match a, b with
    | _, [] -> []
    | a, (b::bs) -> if a=b then bs else b :: drop1 a bs

  let rec setminus a b = match a, b with
    | [], _ -> []
    | (a::l1), l2 ->
      if member a l2 then setminus l1 (drop1 a l2)
      else a :: setminus l1 l2

  (* randomly pick m out of n elements *)
  let rec pick a b c = match a, b, c with
    | 0, _, _ -> []
    | m, n, (x::xs) ->
      if n<=m then x::xs
      else if rand n <= m then x :: pick (m-1) (n-1) xs
      else pick m (n-1) xs
    | _, _, _ -> raise (RunError ("Bad call to pick",(0,0)))

  let rec lookup a b = match a, b with
    | _, [] -> None
    | x, ((y,v)::table) ->
      if (0 = String.compare x y) then Some v else lookup x table

  let rec rollDice (decs, dice) = evalExp0 dice [] decs 0

  and evalExp0 exp table decs d =
    let rec evalExp exp table d =
      (* TODO: Make max stack size adjustable *)
      if d > 1000 then raise (RunError ("Max stack size exceeded", (0,0))) else
        let depth = d + 1 in
        match exp with
          Syntax.NUM (n, _) -> VAL [n]
        | Syntax.ID (x, p) ->
          (match lookup x table with
             Some v -> v
           | None -> raise (RunError ("unknown variable: "^x, p)))
        | Syntax.EMPTY -> VAL []
        | Syntax.CONC (e1, e2 ,p) ->
          (match (evalExp e1 table depth, evalExp e2 table depth) with
             (VAL v1, VAL v2) -> VAL (merge v1 v2)
           | _ -> raise (RunError ("Args to @ must be collections", p)))
        | Syntax.DROP (e1, e2, p) ->
          (match (evalExp e1 table depth, evalExp e2 table depth) with
             (VAL v1, VAL v2) -> VAL (drop v1 v2)
           | _ -> raise (RunError ("Args to drop must be collections", p)))
        | Syntax.KEEP (e1,e2, p) ->
          (match (evalExp e1 table depth, evalExp e2 table depth) with
             (VAL v1, VAL v2) -> VAL (keep v1 v2)
           | _ -> raise (RunError ("Args to keep must be collections", p)))
        | Syntax.SETMINUS (e1,e2, p) ->
          (match (evalExp e1 table depth, evalExp e2 table depth) with
             (VAL v1, VAL v2) -> VAL (setminus v1 v2)
           | _ -> raise (RunError ("Args to setminus must be collections", p)))
        | Syntax.CHOOSE (e1, p) ->
          (match (evalExp e1 table depth) with
             VAL [] -> raise (RunError ("Arg to choose most be non-empty", p))
           | VAL ns -> VAL [List.nth_exn ns (rand (List.length ns) - 1)]
           | _  -> raise (RunError ("Arg to choose must be a collection", p)))
        | Syntax.PICK (e1,e2, p) ->
          (match (evalExp e1 table depth, evalExp e2 table depth) with
             (VAL ns, VAL [n]) -> VAL (pick n (List.length ns) ns)
           | _ -> raise (RunError ("The first arg to pick must be a collection, and the second a number", p)))
        | Syntax.DIFFERENT (e1, p) ->
          let rec noDups = function
              [] -> []
            | (x::xs) ->
              let ys = noDups xs in (drop [x] ys)@ys
          in
          (match (evalExp e1 table depth) with
             VAL v -> VAL (noDups v)
           | _ -> raise (RunError ("Arg to different must be a collection", p)))

        | Syntax.PLUS (e1,e2, p) ->
          (match (evalExp e1 table depth, evalExp e2 table depth) with
             (VAL [n1], VAL [n2]) -> VAL [n1 + n2]
           | _ -> raise (RunError ("illegal arg to +", p)))
        | Syntax.MINUS (e1,e2, p) ->
          (match (evalExp e1 table depth, evalExp e2 table depth) with
             (VAL [n1], VAL [n2]) -> VAL [n1-n2]
           | _ -> raise (RunError ("illegal arg to -", p)))
        | Syntax.UMINUS (e1, p) ->
          (match (evalExp e1 table depth) with
             VAL [n1] -> VAL [~-n1]
           | _ -> raise (RunError ("illegal arg to -", p)))
        | Syntax.TIMES (e1,e2, p) ->
          (match (evalExp e1 table depth, evalExp e2 table depth) with
             (VAL [n1], VAL [n2]) -> VAL [n1*n2]
           | _ -> raise (RunError ("illegal arg to *", p)))
        | Syntax.DIVIDE (e1,e2, p) ->
          (match (evalExp e1 table depth, evalExp e2 table depth) with
             (VAL [], VAL [_]) -> VAL [0]
           | (VAL [n1], VAL [n2]) ->
             if n2=0 then raise (RunError ("division by 0", p))
             else VAL [n1 / n2]
           | _ -> raise (RunError ("illegal arg to /", p)))
        | Syntax.MOD (e1,e2, p) ->
          (match (evalExp e1 table depth, evalExp e2 table depth) with
             (VAL [], VAL [_]) -> VAL [0]
           | (VAL [n1], VAL [n2]) ->
             if n2=0 then raise (RunError ("modulo by 0", p))
             else VAL [n1 mod n2]
           | _ -> raise (RunError ("illegal arg to mod", p)))
        | Syntax.D (e1, p) ->
          (match (evalExp e1 table depth) with
             VAL [n]   ->
             if n<=0 then raise (RunError ("Arg to d or D most be >0", p))
             else VAL [rand n]
           | _ -> raise (RunError ("illegal arg to d or D", p)))
        | Syntax.Z (e1, p) ->
          (match (evalExp e1 table depth) with
             VAL [n]   ->
             if n<0 then raise (RunError ("Arg to z or Z most be >=0", p))
             else VAL [rand (n+1) - 1]
           | _ -> raise (RunError ("illegal arg to z or Z", p)))
        | Syntax.SIGN (e1, p) ->
          (match (evalExp e1 table depth) with
             VAL [n]   -> VAL [sign n]
           | _ -> raise (RunError ("illegal arg to sgn", p)))
        | Syntax.SUM (e1, p) ->
          (match (evalExp e1 table depth) with
             VAL v -> VAL [List.fold_left ~f:(fun x y ->x + y) ~init:0 v]
           | _ -> raise (RunError ("illegal arg to sum", p)))
        | Syntax.COUNT (e1, p) ->
          (match (evalExp e1 table depth) with
             VAL v -> VAL [List.length v]
           | _ -> raise (RunError ("illegal arg to count", p)))
        | Syntax.LEAST (e1,e2, p) ->
          (match (evalExp e1 table depth,evalExp e2 table depth) with
             (VAL [n], VAL l) ->
             if n<0 then raise (RunError ("Negative arg to least", p))
             else if List.length l <= n then VAL l
             else VAL (List.take l n)
           | _ -> raise (RunError ("illegal arg to least", p)))
        | Syntax.LARGEST (e1,e2, p) ->
          (match (evalExp e1 table depth,evalExp e2 table depth) with
             (VAL [n], VAL l) ->
             if n<0 then raise (RunError ("Negative arg to largest", p))
             else if List.length l <= n then VAL l
             else VAL (drop' ((List.length l)- n) l)
           | _ -> raise (RunError ("illegal arg to largest", p)))
        | Syntax.MEDIAN (e1, p) ->
          (match (evalExp e1 table depth) with
             VAL [] -> raise (RunError ("Can't take median of empty collection", p))
           | VAL vs -> VAL [List.nth_exn vs (List.length vs / 2)]
           | _ -> raise (RunError  ("Can't take median of text", p)))
        | Syntax.MINIMAL (e, p) ->
          (match (evalExp e table depth) with
             VAL [] -> VAL []
           | VAL (a::v) -> let rec g = function
                 [] -> [a]
               | (b::bs) -> if a=b then b:: g bs else [a]
             in VAL (g v)
           | _ -> raise (RunError ("illegal arg to minimal", p)))
        | Syntax.MAXIMAL (e, p) ->
          (match (evalExp e table depth) with
             VAL [] -> VAL []
           | VAL (a::v) -> let rec g a b c = match a, b, c with
                 [], x, xs -> x::xs
               | (b::bs), x, xs ->
                 if b=x then g bs x (b::xs) else g bs b []
             in VAL (g v a [])
           | _ -> raise (RunError ("illegal arg to maximal", p)))
        | Syntax.HASH (e1,e2, p) ->
          (match (evalExp e1 table depth) with
             VAL [n] ->
             if n<0 then raise (RunError ("Negative arg to #", p))
             else
               VAL (List.fold_right
                      ~f:(fun a b -> merge a b)
                      (tabulate n (fun _ ->
                           match evalExp e2 table depth with
                             VAL v -> v
                           | _ -> raise (RunError ("illegal arg2 to #",p)))) ~init:[])
           | _ -> raise (RunError ("illegal arg1 to #",p)))
        | Syntax.AND (e1, e2, _) ->
          (match (evalExp e1 table depth) with
             VAL [] -> VAL []
           | _      -> evalExp e2 table depth)
        | Syntax.EQ (e1,e2,p) ->
          (match (evalExp e1 table depth, evalExp e2 table depth) with
             (VAL [n1], VAL l) -> VAL (List.filter ~f:(fun x -> n1=x) l)
           | _ -> raise (RunError ("illegal arg to =",p)))
        | Syntax.NEQ (e1,e2,p) ->
          (match (evalExp e1 table depth, evalExp e2 table depth) with
             (VAL [n1], VAL l) -> VAL (List.filter ~f:(fun x -> n1<>x) l)
           | _ -> raise (RunError ("illegal arg to =/=",p)))
        | Syntax.LT (e1,e2,p) ->
          (match (evalExp e1 table depth, evalExp e2 table depth) with
             (VAL [n1], VAL l) -> VAL (List.filter ~f:(fun x -> n1<x) l)
           | _ -> raise (RunError ("illegal arg to <",p)))
        | Syntax.GT (e1,e2,p) ->
          (match (evalExp e1 table depth, evalExp e2 table depth) with
             (VAL [n1], VAL l) -> VAL (List.filter ~f:(fun x -> n1>x) l)
           | _ -> raise (RunError ("illegal arg to >",p)))
        | Syntax.LE (e1,e2,p) ->
          (match (evalExp e1 table depth, evalExp e2 table depth) with
             (VAL [n1], VAL l) -> VAL (List.filter ~f:(fun x -> n1<=x) l)
           | _ -> raise (RunError ("illegal arg to <=",p)))
        | Syntax.GE (e1,e2,p) ->
          (match (evalExp e1 table depth, evalExp e2 table depth) with
             (VAL [n1], VAL l) -> VAL (List.filter ~f:(fun x -> n1>=x) l)
           | _ -> raise (RunError ("illegal arg to >=",p)))
        | Syntax.FROMTO (e1,e2,p) ->
          (match (evalExp e1 table depth, evalExp e2 table depth) with
             (VAL [n1], VAL [n2]) -> VAL (tabulate (n2-n1+1) (fun x -> x+n1))
           | _ -> raise (RunError ("illegal arg to ..",p)))
        | Syntax.LET (x, e1, e2, _) ->
          evalExp e2 ((x,evalExp e1 table depth)::table) depth
        | Syntax.ACCUM (x,e1,e2,continue,p) ->
          (match evalExp e1 table depth with
             VAL v ->
             VAL (List.fold_right ~f:(fun a b -> merge a b)
                    (iterate v x e1 e2 continue table decs p depth) ~init:[])
           | _ -> raise (RunError ("illegal arg to accumulate",p)))
        | Syntax.REPEAT (x,e1,e2,continue,p) ->
          (match evalExp e1 table depth with
             VAL v ->
             VAL (List.last_exn (iterate v x e1 e2 continue table decs p depth))
           | _ -> raise (RunError ("illegal arg to repeat",p)))
        | Syntax.FOREACH (x,e1,e2,p) ->
          (match evalExp e1 table depth with
             VAL v ->
             VAL (List.fold_right ~f:(fun a b -> merge a b)
                    (List.map ~f:(fun w ->
                         (match evalExp e2 ((x,VAL [w])::table) depth with
                            VAL v1 -> v1
                          | _ -> raise (RunError ("illegal arg2 to foreach",p))))
                        v) ~init:[])
           | _ -> raise (RunError ("illegal arg1 to foreach",p)))
        | Syntax.IF (e1,e2,e3,p) ->
          (match evalExp e1 table depth with
             VAL [] -> evalExp e3 table depth (* false *)
           | VAL _ -> evalExp e2 table depth  (* true *)
           | _ -> raise (RunError ("illegal arg to if",p)))
        | Syntax.CALL (f,args,p) ->
          callFun (f, List.map ~f:(fun e -> evalExp e table depth) args, decs, p, depth)
        | Syntax.STRING (s, _) -> TEXT [s]
        | Syntax.SAMPLE (e, _) ->
          makeText (evalExp e table depth)
        | Syntax.SAMPLES (e1, e2, p) ->
          (match (evalExp e1 table depth) with
             VAL [n] ->
             if n<0 then raise (RunError ("Negative arg1 to '", p))
             else
               let rec samples = function
                   0 -> TEXT []
                 | 1 -> makeText (evalExp e2 table depth)
                 | n -> vconcr (makeText (evalExp e2 table depth),
                                samples (n-1))
               in
               samples n
           | _ -> raise (RunError ("illegal arg1 to '", p)))
        | Syntax.HCONC (e1, e2, _) ->
          hconc (evalExp e1 table depth, evalExp e2 table depth)
        | Syntax.VCONCL (e1, e2, _) ->
          vconcl (evalExp e1 table depth, evalExp e2 table depth)
        | Syntax.VCONCR (e1, e2, _) ->
          vconcr (evalExp e1 table depth, evalExp e2 table depth)
        | Syntax.VCONCC (e1, e2, _) ->
          vconcc (evalExp e1 table depth, evalExp e2 table depth)
        (* | Syntax.QUESTION (prob, _) ->
           if tryFn prob then VAL [1] else VAL [] *)
        | Syntax.PAIR (e1, e2, _) ->
          PAIR (evalExp e1 table depth, evalExp e2 table depth)
        | Syntax.FIRST (e1, p) ->
          (match evalExp e1 table depth with
             PAIR (v, _) -> v
           | _ -> raise (RunError ("Argument to %1 must be a pair\n", p)))
        | Syntax.SECOND (e1, p) ->
          (match evalExp e1 table depth with
             PAIR (_, w) -> w
           | _ -> raise (RunError ("Argument to %2 must be a pair\n", p)))
        | Syntax.LOOP (arg_exps, cond_exp, else_exp, params, p) ->
          let table' = ref table in
          let b t = match (evalExp cond_exp t depth) with
              VAL [] -> false
            | VAL _ -> true
            | _ -> raise (RunError ("illegal arg to if", p)) in
          let () = while b !table' do
              table' := (List.fold (List.zip_exn params arg_exps) ~init:[] ~f:(fun l e ->
                  let (param, arg) = e in
                  (param, evalExp arg !table' depth)::l)) @ table 
            done in
          evalExp else_exp !table' depth
        | Syntax.DEFAULT (x, e1, _) ->
          (match lookup x table with
             Some v -> v
           | None ->  evalExp e1 table depth)
    in
    evalExp exp table d


  and iterate v x e1 e2 continue table decs pos depth =
    match evalExp0 e2 ((x,VAL v)::table) decs depth with
      VAL test ->
      v::(if 0 = Bool.compare (not (is_empty test)) continue
          then
            (match evalExp0 e1 table decs depth with
               VAL v1 -> iterate v1 x e1 e2 continue table decs pos depth
             | _ -> raise (RunError ("illegal arg to iterator",pos)))
          else [])
    | _ -> raise (RunError ("illegal arg to iterator",pos))

  and callFun (f, vs, decs, p, depth) =
    match lookup f decs with
      None -> raise (RunError ("Unknown function: "^f,p))
    | Some (Syntax.Func(pars, body, _)) ->
      evalExp0 body (List.zip_exn pars vs) decs depth
    | Some (Syntax.Comp(empty, single, union, pos)) ->
      (match vs with
         [VAL v] -> compositional (v, empty, single, union, decs, pos, depth)
       | _ -> raise (RunError ("Wrong number of args to "^f,p)))

  and compositional = function
      ([], empty, _, _, decs, _, depth) ->
      evalExp0 empty [] decs depth
    | ((x::xs), empty, single, union, decs, p, depth) ->
      let v1 = callFun (single, [VAL [x]], decs, p, depth) in
      let v2 = compositional (xs, empty, single, union, decs, p, depth) in
      callFun (union, v1::v2::[], decs, p, depth)

  (* text functions *)

  (* converts non-text values into text values *)
  and makeText = function
      (VAL v) ->
      TEXT [concatenate
              (List.map
                 ~f:(fun n -> if n>=0 then string_of_int n
                      else "-" ^ string_of_int (~-n))
                 v)]
    | (PAIR (v,w)) ->
      (match (makeText v, makeText w) with
         (TEXT [s1], TEXT [s2]) -> TEXT ["[" ^ s1 ^ " , " ^ s2 ^ "]"]
       | (TEXT ss1, TEXT ss2) -> TEXT (["["] @ ss1 @ [" , "] @ ss2 @ [ "]"])
       | _  -> raise (RunError ("Can not convert to text\n", (0,0))))
    | text -> text

  and printVal s = match makeText s with
      TEXT s -> String.concat s ^ " "
    | _ -> failwith("should not happen")

  and concatenate = function
      [] -> ""
    | [x] -> x
    | (x :: xs) -> x ^ " " ^ concatenate xs

  (* make string of n spaces *)
  and spaces = function
    | 0 -> ""
    | n -> " " ^ spaces (n-1)

  (* Horisontal concatenation (top aligned) *)
  and hconc = function
      (TEXT ss1,TEXT ss2) ->
      let l1 = String.length (List.hd_exn ss1) in
      let l2 = String.length (List.hd_exn ss2) in
      let pad1 = spaces l1 in
      let pad2 = spaces l2 in
      let rec hc a b = match a, b with
          [], [] -> []
        | [], (s2::ss2) -> (pad1^s2) :: hc [] ss2
        | (s1::ss1), [] -> (s1^pad2) :: hc ss1 []
        | (s1::ss1), (s2::ss2) -> (s1^s2) :: hc ss1 ss2
      in TEXT (hc ss1 ss2)
    | (v1, v2) -> hconc (makeText v1, makeText v2)


  (* left-aligned vertical concatenation *)
  and vconcl = function
      (TEXT ss1,TEXT ss2) ->
      let l1 = String.length (List.hd_exn ss1) in
      let l2 = String.length (List.hd_exn ss2) in
      let pad = spaces (abs (l1-l2)) in
      if l1 = l2 then TEXT (ss1 @ ss2)
      else if l1<l2 then
        TEXT (List.map ~f:(fun s -> s^pad) ss1 @ ss2)
      else
        TEXT (ss1 @ List.map ~f:(fun s -> s^pad) ss2)
    | (v1, v2) -> hconc (makeText v1, makeText v2)

  (* right-aligned vertical concatenation *)
  and vconcr = function
      (TEXT ss1,TEXT ss2) ->
      let l1 = String.length (List.hd_exn ss1) in
      let l2 = String.length (List.hd_exn ss2) in
      let pad = spaces (abs (l1-l2)) in
      if l1 = l2 then TEXT (ss1 @ ss2)
      else if l1<l2 then
        TEXT (List.map ~f:(fun s -> pad^s) ss1 @ ss2)
      else
        TEXT (ss1 @ List.map ~f:(fun s -> pad^s) ss2)
    | (v1, v2) -> hconc (makeText v1, makeText v2)

  (* center-aligned vertical concatenation *)
  and vconcc = function
      (TEXT ss1,TEXT ss2) ->
      let l1 = String.length (List.hd_exn ss1) in
      let l2 = String.length (List.hd_exn ss2) in
      let l3 = abs (l1-l2) in
      let padl = spaces (l3 / 2) in
      let padr = spaces (l3 - l3 / 2) in
      if l1 = l2 then TEXT (ss1 @ ss2)
      else if l1<l2 then
        TEXT (List.map ~f:(fun s -> padl^s^padr) ss1 @ ss2)
      else
        TEXT (ss1 @ List.map ~f:(fun s -> padl^s^padr) ss2)
    | (v1, v2) -> hconc (makeText v1, makeText v2)

end
