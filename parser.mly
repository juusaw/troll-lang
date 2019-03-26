%{

open Syntax

let p0 = (0,0)

let fst x = let y, _ = x in y

let snd x = let _, y = x in y

let makeUnaryFunction (name, constr) =
      (name, Syntax.Func (["x"],
			  constr (Syntax.ID ("x", p0), p0),
			  p0))

let makeBinaryFunction (name, constr) =
      (name, Syntax.Func (["x"; "y"],
			  constr (Syntax.ID ("x", p0),
				  Syntax.ID ("y", p0),
				  p0),
			  p0))

let predef = List.map makeUnaryFunction
		      [("-", (fun (x, y) -> Syntax.UMINUS (x, y)));
		       ("d", (fun (x, y) -> Syntax.D (x, y)));
		       ("z", (fun (x, y) -> Syntax.Z (x, y)));
		       ("sum",(fun (x, y) -> Syntax.SUM (x, y)));
		       ("sign",(fun (x, y) -> Syntax.SIGN (x, y)));
		       ("count",(fun (x, y) -> Syntax.COUNT (x, y)));
		       ("min",fun (e,p) -> Syntax.LEAST (Syntax.NUM (1,p0),e,p));
		       ("max",fun (e,p) -> Syntax.LARGEST (Syntax.NUM (1,p0),e,p));
		       ("minimal", (fun (x, y) -> Syntax.MINIMAL (x, y)));
		       ("maximal", (fun (x, y) -> Syntax.MAXIMAL (x, y)));
		       ("choose", (fun (x, y) -> Syntax.CHOOSE (x, y)));
		       ("different", (fun (x, y) -> Syntax.DIFFERENT (x, y))) ]
	     @
	     List.map makeBinaryFunction
		      [("+", (fun (x, y, z) -> Syntax.PLUS (x, y, z)));
		       ("*", (fun (x, y, z) -> Syntax.TIMES (x, y, z)));
		       ("U", (fun (x, y, z) -> Syntax.CONC (x, y, z)))]
%}

%token <int*(int*int)> NUM
%token <string*(int*int)> ID
%token <string list *(int*int)> STRINGS
%token <(int*int)> D Z SUM LEAST LARGEST MIN MAX CONC HASH COUNT AND
%token <(int*int)> PLUS MINUS TIMES DIVIDE MOD LPAR RPAR LBRACE RBRACE COMMA
%token <(int*int)> ASSGN EQ NEQ LT GT LE GE DOTDOT SEMI LET IN
%token <(int*int)> FOREACH DO IF THEN ELSE CHOOSE DROP KEEP PICK DIFFERENT MEDIAN
%token <(int*int)> ACCUM REPEAT WHILE UNTIL FUNCTION CALL COMPOSITIONAL EOF
%token <(int*int)> SAMPLE HCONC VCONCL VCONCR VCONCC
%token <(int*int)> QUESTION MINIMAL MAXIMAL SETMINUS
%token <(int*int)> LBRACK RBRACK FIRST SECOND SIGN TILDE BANG
%token <float*(int*int)> FLOAT

%nonassoc FUNCTION
%right SEMI
%nonassoc DO ELSE WHILE UNTIL
%right VCONCL VCONCR VCONCC HCONC
%nonassoc DOTDOT
%left DROP KEEP PICK SETMINUS
%right CONC AND
%left PLUS MINUS
%left TIMES DIVIDE MOD
%nonassoc UMINUS
%nonassoc SUM COUNT LEAST LARGEST MIN MAX CHOOSE DIFFERENT SAMPLE MINIMAL MAXIMAL MEDIAN FIRST SECOND SIGN BANG
%right NEQ EQ LT GT LE GE
%right HASH TILDE
%left D Z

%start dice
%type <Syntax.program> dice
%type <string * Syntax.declaration> Decl
%type <(string * Syntax.declaration) list> Decls
%type <Syntax.exp> Exp ExpList ExpList1
%type <Syntax.exp list> ExpList2
%type <string list> Ids
%type <string> IDorUnop IDorBinop

%%

dice:
	  Decls Exp Decls EOF	{ ($1 @ $3 @ predef,$2) }
;

Decls:
	  Decl Decls	{ $1 :: $2 }
	|		{ [] }
;

Decl:
	  FUNCTION ID LPAR Ids RPAR EQ Exp %prec FUNCTION
			{ (fst $2, Syntax.Func ($4, $7, $1)) }
	| COMPOSITIONAL ID LPAR Exp COMMA IDorUnop COMMA IDorBinop RPAR
			{ (fst $2, Syntax.Comp ($4, $6, $8, $1)) }
;

IDorUnop:
	  ID		{ fst $1 }
	| MINUS		{ "-" }
	| D		{ "d" }
	| Z		{ "z" }
	| SUM		{ "sum" }
	| SIGN		{ "sgn" }
	| COUNT		{ "count" }
	| MIN		{ "min" }
	| MAX		{ "max" }
	| MINIMAL	{ "minimal" }
	| MAXIMAL	{ "maximal" }
	| CHOOSE	{ "choose" }
	| DIFFERENT	{ "different" }
;

IDorBinop:
	  ID		{ fst $1 }
	| PLUS		{ "+" }
	| TIMES		{ "*" }
	| CONC		{ "U" }
Ids:
	  ID		{ [fst $1] }
	| ID COMMA Ids	{ fst $1 :: $3 }
;

Exp:
	  NUM           { Syntax.NUM (fst $1, snd $1) }
        | ID		{ Syntax.ID (fst $1, snd $1) }
	| Exp CONC Exp	{ Syntax.CONC ($1,$3, $2) }
	| CHOOSE Exp	{ Syntax.CHOOSE ($2, $1) }
	| DIFFERENT Exp	{ Syntax.DIFFERENT ($2, $1) }
	| LBRACE ExpList RBRACE
			{ $2 }
	| Exp PLUS Exp	{ Syntax.PLUS ($1,$3, $2) }
	| Exp MINUS Exp	{ Syntax.MINUS ($1,$3, $2) }
	| Exp TIMES Exp	{ Syntax.TIMES ($1,$3, $2) }
	| Exp DIVIDE Exp
		     	{ Syntax.DIVIDE ($1,$3, $2) }
	| Exp MOD Exp	{ Syntax.MOD ($1,$3, $2) }
	| MINUS Exp %prec UMINUS
			{ Syntax.UMINUS ($2, $1) }
	| D Exp		{ Syntax.D ($2, $1) }
	| Z Exp		{ Syntax.Z ($2, $1) }
	| SUM Exp	{ Syntax.SUM ($2, $1) }
	| SIGN Exp	{ Syntax.SIGN ($2, $1) }
	| COUNT Exp	{ Syntax.COUNT ($2, $1) }
	| LEAST Exp Exp	{ Syntax.LEAST ($2,$3, $1) }
	| MIN Exp	{ Syntax.LEAST (Syntax.NUM (1,$1),$2, $1) }
	| LARGEST Exp Exp
		  	{ Syntax.LARGEST ($2,$3, $1) }
	| MAX Exp	{ Syntax.LARGEST (Syntax.NUM (1,$1),$2, $1) }
        | MEDIAN Exp    { Syntax.MEDIAN ($2, $1) }
	| MINIMAL Exp	{ Syntax.MINIMAL ($2, $1) }
	| MAXIMAL Exp	{ Syntax.MAXIMAL ($2, $1) }
	| Exp HASH Exp	{ Syntax.HASH ($1,$3, $2) }
	| Exp AND Exp	{ Syntax.AND ($1,$3, $2) }
	| Exp D Exp %prec HASH
			{ Syntax.HASH ($1,Syntax.D ($3, $2), $2) }
	| Exp Z Exp %prec HASH
			{ Syntax.HASH ($1,Syntax.Z ($3, $2), $2) }
	| Exp EQ Exp	{ Syntax.EQ ($1,$3, $2) }
	| Exp NEQ Exp	{ Syntax.NEQ ($1,$3, $2) }
	| Exp LT Exp	{ Syntax.LT ($1,$3, $2) }
	| Exp GT Exp	{ Syntax.GT ($1,$3, $2) }
	| Exp LE Exp	{ Syntax.LE ($1,$3, $2) }
	| Exp GE Exp	{ Syntax.GE ($1,$3, $2) }
	| Exp DROP Exp	{ Syntax.DROP ($1,$3, $2) }
	| Exp KEEP Exp	{ Syntax.KEEP ($1,$3, $2) }
	| Exp PICK Exp	{ Syntax.PICK ($1,$3, $2) }
	| Exp SETMINUS Exp
			{ Syntax.SETMINUS ($1,$3, $2) }
	| Exp DOTDOT Exp
			{ Syntax.FROMTO ($1,$3, $2) }
	| ID ASSGN Exp SEMI Exp
		 	{ Syntax.LET (fst $1,$3,$5, $2) }
	| ACCUM ID ASSGN Exp WHILE Exp
			{ Syntax.ACCUM (fst $2,$4,$6, true, $1) }
	| ACCUM ID ASSGN Exp UNTIL Exp
			{ Syntax.ACCUM (fst $2,$4,$6, false, $1) }
	| REPEAT ID ASSGN Exp WHILE Exp
			{ Syntax.REPEAT (fst $2,$4,$6, true, $1) }
	| REPEAT ID ASSGN Exp UNTIL Exp
			{ Syntax.REPEAT (fst $2,$4,$6, false, $1) }
	| FOREACH ID IN Exp DO Exp
		 	{ Syntax.FOREACH (fst $2,$4,$6, $1) }
	| IF Exp THEN Exp ELSE Exp
		 	{ Syntax.IF ($2,$4,$6, $1) }
	| CALL ID LPAR ExpList2 RPAR
			{ Syntax.CALL (fst $2, $4, $1) }
	| STRINGS       { let
                            pos = snd $1 in
                            let rec build = function
															[] -> Syntax.STRING ("" ,pos)
                              | [s] -> Syntax.STRING (s,pos)
                              | ("|>" :: ss) ->
                                  Syntax.VCONCL(Syntax.STRING ("",pos),
                                                build ss,pos)
                              | ("<|" :: ss) ->
                                  Syntax.VCONCR(Syntax.STRING ("",pos),
                                                build ss,pos)
                              | ("<>" :: ss) ->
                                  Syntax.VCONCC(Syntax.STRING ("",pos),
                                                build ss,pos)
                              | ("||" :: ss) ->
                                  Syntax.HCONC(Syntax.STRING ("",pos),
                                               build ss,pos)
                              | (s :: "|>" :: ss) ->
                                  Syntax.VCONCL(Syntax.STRING (s,pos),
                                                build ss,pos)
                              | (s :: "<|" :: ss) ->
                                  Syntax.VCONCR(Syntax.STRING (s,pos),
                                                build ss,pos)
                              | (s :: "<>" :: ss) ->
                                  Syntax.VCONCC(Syntax.STRING (s,pos),
                                                build ss,pos)
                              | (s :: "||" :: ss) ->
                                  Syntax.HCONC(Syntax.STRING (s,pos),
                                               build ss,pos)
                              | (s :: ss) ->
                                  Syntax.HCONC(Syntax.STRING (s,pos),
                                               build ss,pos)
                          in
                            build (fst $1)
                          
                        }
        | SAMPLE Exp    { Syntax.SAMPLE ($2, $1) }
        | Exp SAMPLE Exp %prec HASH
	                { Syntax.SAMPLES ($1, $3, $2) }
        | Exp HCONC Exp { Syntax.HCONC ($1,$3,$2) }
        | Exp VCONCL Exp { Syntax.VCONCL ($1,$3,$2) }
        | Exp VCONCR Exp { Syntax.VCONCR ($1,$3,$2) }
        | Exp VCONCC Exp { Syntax.VCONCC ($1,$3,$2) }
	| QUESTION FLOAT	{ Syntax.QUESTION (fst $2, $1) }
	| FIRST Exp		{ Syntax.FIRST ($2, $1) }
	| SECOND Exp	{ Syntax.SECOND ($2, $1) }
	| LBRACK Exp COMMA Exp RBRACK
					{ Syntax.PAIR ($2, $4, $3) }
	| BANG Exp	{ Syntax.IF ($2, Syntax.EMPTY, Syntax.NUM (1,$1), $1) }
	| ID TILDE Exp	{ Syntax.DEFAULT (fst $1, $3, $2) }
	| LPAR Exp RPAR	{ $2 }
;

ExpList:
	  		{ Syntax.EMPTY }
	| ExpList1	{ $1 }
;

ExpList1:
	  Exp		{ $1 }
	| Exp COMMA ExpList1
			{ Syntax.CONC ($1,$3, $2) }
;

ExpList2:
	  Exp		{ [$1] }
	| Exp COMMA ExpList2
			{ $1 :: $3 }
;

%%