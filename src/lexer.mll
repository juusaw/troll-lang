{

open Lexing
open Parser
let currentLine = ref 1
let lineStartPos = ref [0]

let rec getPos lexbuf = getLineCol (lexeme_start lexbuf)
                              (!currentLine)
                              (!lineStartPos)

and getLineCol p l s = match p, l, s with
  pos, line, (p1::ps) ->
      if pos>=p1 then (line, pos-p1)
      else getLineCol pos (line - 1) ps
  | _, _, [] -> (0,0) (* should not happen *)


exception LexicalError of string * (int * int) (* (message, (line, column)) *)

let lexerError lexbuf s =
    raise (LexicalError (s, getPos lexbuf))

let keyword (s, pos) =
    match s with
        "D"            -> Parser.D pos
      | "d"            -> Parser.D pos
      | "Z"            -> Parser.Z pos
      | "z"            -> Parser.Z pos
      | "U"            -> Parser.CONC pos
      | "sum"          -> Parser.SUM pos
      | "sgn"          -> Parser.SIGN pos
      | "mod"          -> Parser.MOD pos
      | "least"        -> Parser.LEAST pos
      | "largest"      -> Parser.LARGEST pos
      | "count"        -> Parser.COUNT pos
      | "drop"         -> Parser.DROP pos
      | "keep"         -> Parser.KEEP pos
      | "pick"         -> Parser.PICK pos
      | "median"       -> Parser.MEDIAN pos
      | "let"          -> Parser.LET pos
      | "in"           -> Parser.IN pos
      | "repeat"       -> Parser.REPEAT pos
      | "accumulate"   -> Parser.ACCUM pos
      | "while"        -> Parser.WHILE pos
      | "until"        -> Parser.UNTIL pos
      | "foreach"      -> Parser.FOREACH pos
      | "do"           -> Parser.DO pos
      | "if"           -> Parser.IF pos
      | "then"         -> Parser.THEN pos
      | "else"         -> Parser.ELSE pos
      | "min"          -> Parser.MIN pos
      | "max"          -> Parser.MAX pos
      | "minimal"      -> Parser.MINIMAL pos
      | "maximal"      -> Parser.MAXIMAL pos
      | "choose"       -> Parser.CHOOSE pos
      | "different"    -> Parser.DIFFERENT pos
      | "function"     -> Parser.FUNCTION pos
      | "call"         -> Parser.CALL pos
      | "compositional"
                -> Parser.COMPOSITIONAL pos
      | _              -> Parser.ID (s,pos)

 }

rule token = parse
    [' ' '\t' '\r']     { token lexbuf } (* whitespace *)
  | ['\n' '\012']       { currentLine := !currentLine+1;
                          lineStartPos :=  lexeme_start lexbuf
                                           :: !lineStartPos;
                          token lexbuf } (* newlines *)
  | "\\" [^ '\n' '\012']*
                        { token lexbuf } (* comment *)
  | ['0'-'9']+          { match int_of_string_opt (lexeme lexbuf) with
                               None   -> lexerError lexbuf "Bad integer"
                             | Some i -> Parser.NUM (i, getPos lexbuf)
                        }
  | "0."['0'-'9']+ 	{ match float_of_string_opt (lexeme lexbuf) with
                               None   -> lexerError lexbuf "Bad number"
                             | Some p -> Parser.FLOAT (p, getPos lexbuf)
                        }
  | '"'                 { Parser.STRINGS (string_token lexbuf, getPos lexbuf) }
  | ['a'-'z' 'A'-'Z']+  { keyword (lexeme lexbuf, getPos lexbuf) }
  | '+'                 { Parser.PLUS (getPos lexbuf) }
  | '-'                 { Parser.MINUS (getPos lexbuf) }
  | "--"                { Parser.SETMINUS (getPos lexbuf) }
  | '*'                 { Parser.TIMES (getPos lexbuf) }
  | '/'                 { Parser.DIVIDE (getPos lexbuf) }
  | '('                 { Parser.LPAR (getPos lexbuf) }
  | ')'                 { Parser.RPAR (getPos lexbuf) }
  | ','                 { Parser.COMMA (getPos lexbuf) }
  | ';'                 { Parser.SEMI (getPos lexbuf) }
  | '{'                 { Parser.LBRACE (getPos lexbuf) }
  | '}'                 { Parser.RBRACE (getPos lexbuf) }
  | ":="                { Parser.ASSGN (getPos lexbuf) }
  | '='                 { Parser.EQ (getPos lexbuf) }
  | "=/="               { Parser.NEQ (getPos lexbuf) }
  | '<'                 { Parser.LT (getPos lexbuf) }
  | '>'                 { Parser.GT (getPos lexbuf) }
  | "<="                { Parser.LE (getPos lexbuf) }
  | ">="                { Parser.GE (getPos lexbuf) }
  | ".."                { Parser.DOTDOT (getPos lexbuf) }
  | '@'                 { Parser.CONC (getPos lexbuf) }
  | '&'                 { Parser.AND (getPos lexbuf) }
  | '#'                 { Parser.HASH (getPos lexbuf) }
  | '?'                 { Parser.QUESTION (getPos lexbuf) }
  | "||"                { Parser.HCONC (getPos lexbuf) }
  | "|>"                { Parser.VCONCL (getPos lexbuf) }
  | "<|"                { Parser.VCONCR (getPos lexbuf) }
  | "<>"                { Parser.VCONCC (getPos lexbuf) }
  | "'"                 { Parser.SAMPLE (getPos lexbuf) }
  | '['                 { Parser.LBRACK (getPos lexbuf) }
  | ']'                 { Parser.RBRACK (getPos lexbuf) }
  | "%1"                { Parser.FIRST (getPos lexbuf) }
  | "%2"                { Parser.SECOND (getPos lexbuf) }
  | '~'                 { Parser.TILDE (getPos lexbuf) }
  | '!'                 { Parser.BANG (getPos lexbuf) }
  | eof                 { Parser.EOF (getPos lexbuf) }
  | _                   { lexerError lexbuf "Illegal symbol in input" }

and string_token = parse
    '"'                 { [] }
  | "|>" | "<|" | "<>" | "||"
                        { lexeme lexbuf :: string_token lexbuf }
  | (  [^ '\000'-'\031' '"' '|' '<' '\127'-'\159']
     | '|' [^ '\000'-'\031' '"' '|' '>' '\127'-'\159']
     | '<' [^ '\000'-'\031' '"' '|' '>' '\127'-'\159']) *
                        { lexeme lexbuf :: string_token lexbuf }

