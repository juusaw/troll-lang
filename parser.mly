%{

module S = Syntax

let p0 = (0,0)

let fst x = let y, _ = x in y

let snd x = let _, y = x in y

let makeUnaryFunction (name, constr) =
      (name, S.Syntax.Func (["x"],
			  constr (S.Syntax.ID ("x", p0), p0),
			  p0))

let makeBinaryFunction (name, constr) =
      (name, S.Syntax.Func (["x"; "y"],
			  constr (S.Syntax.ID ("x", p0),
				  S.Syntax.ID ("y", p0),
				  p0),
			  p0))

let predef = List.map makeUnaryFunction
		      [("-", (fun (x, y) -> S.Syntax.UMINUS (x, y)));
		       ("d", (fun (x, y) -> S.Syntax.D (x, y)));
		       ("z", (fun (x, y) -> S.Syntax.Z (x, y)));
		       ("sum",(fun (x, y) -> S.Syntax.SUM (x, y)));
		       ("sign",(fun (x, y) -> S.Syntax.SIGN (x, y)));
		       ("count",(fun (x, y) -> S.Syntax.COUNT (x, y)));
		       ("min",fun (e,p) -> S.Syntax.LEAST (S.Syntax.NUM (1,p0),e,p));
		       ("max",fun (e,p) -> S.Syntax.LARGEST (S.Syntax.NUM (1,p0),e,p));
		       ("minimal", (fun (x, y) -> S.Syntax.MINIMAL (x, y)));
		       ("maximal", (fun (x, y) -> S.Syntax.MAXIMAL (x, y)));
		       ("choose", (fun (x, y) -> S.Syntax.CHOOSE (x, y)));
		       ("different", (fun (x, y) -> S.Syntax.DIFFERENT (x, y))) ]
	     @
	     List.map makeBinaryFunction
		      [("+", (fun (x, y, z) -> S.Syntax.PLUS (x, y, z)));
		       ("*", (fun (x, y, z) -> S.Syntax.TIMES (x, y, z)));
		       ("U", (fun (x, y, z) -> S.Syntax.CONC (x, y, z)))]
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
%type <Syntax.Syntax.program> dice
%type <string * S.Syntax.declaration> Decl
%type <(string * S.Syntax.declaration) list> Decls
%type <S.Syntax.exp> Exp ExpList ExpList1
%type <S.Syntax.exp list> ExpList2
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
			{ (fst $2, S.Syntax.Func ($4, $7, $1)) }
	| COMPOSITIONAL ID LPAR Exp COMMA IDorUnop COMMA IDorBinop RPAR
			{ (fst $2, S.Syntax.Comp ($4, $6, $8, $1)) }
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
	  NUM           { S.Syntax.NUM (fst $1, snd $1) }
        | ID		{ S.Syntax.ID (fst $1, snd $1) }
	| Exp CONC Exp	{ S.Syntax.CONC ($1,$3, $2) }
	| CHOOSE Exp	{ S.Syntax.CHOOSE ($2, $1) }
	| DIFFERENT Exp	{ S.Syntax.DIFFERENT ($2, $1) }
	| LBRACE ExpList RBRACE
			{ $2 }
	| Exp PLUS Exp	{ S.Syntax.PLUS ($1,$3, $2) }
	| Exp MINUS Exp	{ S.Syntax.MINUS ($1,$3, $2) }
	| Exp TIMES Exp	{ S.Syntax.TIMES ($1,$3, $2) }
	| Exp DIVIDE Exp
		     	{ S.Syntax.DIVIDE ($1,$3, $2) }
	| Exp MOD Exp	{ S.Syntax.MOD ($1,$3, $2) }
	| MINUS Exp %prec UMINUS
			{ S.Syntax.UMINUS ($2, $1) }
	| D Exp		{ S.Syntax.D ($2, $1) }
	| Z Exp		{ S.Syntax.Z ($2, $1) }
	| SUM Exp	{ S.Syntax.SUM ($2, $1) }
	| SIGN Exp	{ S.Syntax.SIGN ($2, $1) }
	| COUNT Exp	{ S.Syntax.COUNT ($2, $1) }
	| LEAST Exp Exp	{ S.Syntax.LEAST ($2,$3, $1) }
	| MIN Exp	{ S.Syntax.LEAST (S.Syntax.NUM (1,$1),$2, $1) }
	| LARGEST Exp Exp
		  	{ S.Syntax.LARGEST ($2,$3, $1) }
	| MAX Exp	{ S.Syntax.LARGEST (S.Syntax.NUM (1,$1),$2, $1) }
        | MEDIAN Exp    { S.Syntax.MEDIAN ($2, $1) }
	| MINIMAL Exp	{ S.Syntax.MINIMAL ($2, $1) }
	| MAXIMAL Exp	{ S.Syntax.MAXIMAL ($2, $1) }
	| Exp HASH Exp	{ S.Syntax.HASH ($1,$3, $2) }
	| Exp AND Exp	{ S.Syntax.AND ($1,$3, $2) }
	| Exp D Exp %prec HASH
			{ S.Syntax.HASH ($1,S.Syntax.D ($3, $2), $2) }
	| Exp Z Exp %prec HASH
			{ S.Syntax.HASH ($1,S.Syntax.Z ($3, $2), $2) }
	| Exp EQ Exp	{ S.Syntax.EQ ($1,$3, $2) }
	| Exp NEQ Exp	{ S.Syntax.NEQ ($1,$3, $2) }
	| Exp LT Exp	{ S.Syntax.LT ($1,$3, $2) }
	| Exp GT Exp	{ S.Syntax.GT ($1,$3, $2) }
	| Exp LE Exp	{ S.Syntax.LE ($1,$3, $2) }
	| Exp GE Exp	{ S.Syntax.GE ($1,$3, $2) }
	| Exp DROP Exp	{ S.Syntax.DROP ($1,$3, $2) }
	| Exp KEEP Exp	{ S.Syntax.KEEP ($1,$3, $2) }
	| Exp PICK Exp	{ S.Syntax.PICK ($1,$3, $2) }
	| Exp SETMINUS Exp
			{ S.Syntax.SETMINUS ($1,$3, $2) }
	| Exp DOTDOT Exp
			{ S.Syntax.FROMTO ($1,$3, $2) }
	| ID ASSGN Exp SEMI Exp
		 	{ S.Syntax.LET (fst $1,$3,$5, $2) }
	| ACCUM ID ASSGN Exp WHILE Exp
			{ S.Syntax.ACCUM (fst $2,$4,$6, true, $1) }
	| ACCUM ID ASSGN Exp UNTIL Exp
			{ S.Syntax.ACCUM (fst $2,$4,$6, false, $1) }
	| REPEAT ID ASSGN Exp WHILE Exp
			{ S.Syntax.REPEAT (fst $2,$4,$6, true, $1) }
	| REPEAT ID ASSGN Exp UNTIL Exp
			{ S.Syntax.REPEAT (fst $2,$4,$6, false, $1) }
	| FOREACH ID IN Exp DO Exp
		 	{ S.Syntax.FOREACH (fst $2,$4,$6, $1) }
	| IF Exp THEN Exp ELSE Exp
		 	{ S.Syntax.IF ($2,$4,$6, $1) }
	| CALL ID LPAR ExpList2 RPAR
			{ S.Syntax.CALL (fst $2, $4, $1) }
	| STRINGS       { let
                            pos = snd $1 in
                            let rec build = function
															[] -> S.Syntax.STRING ("" ,pos)
                              | [s] -> S.Syntax.STRING (s,pos)
                              | ("|>" :: ss) ->
                                  S.Syntax.VCONCL(S.Syntax.STRING ("",pos),
                                                build ss,pos)
                              | ("<|" :: ss) ->
                                  S.Syntax.VCONCR(S.Syntax.STRING ("",pos),
                                                build ss,pos)
                              | ("<>" :: ss) ->
                                  S.Syntax.VCONCC(S.Syntax.STRING ("",pos),
                                                build ss,pos)
                              | ("||" :: ss) ->
                                  S.Syntax.HCONC(S.Syntax.STRING ("",pos),
                                               build ss,pos)
                              | (s :: "|>" :: ss) ->
                                  S.Syntax.VCONCL(S.Syntax.STRING (s,pos),
                                                build ss,pos)
                              | (s :: "<|" :: ss) ->
                                  S.Syntax.VCONCR(S.Syntax.STRING (s,pos),
                                                build ss,pos)
                              | (s :: "<>" :: ss) ->
                                  S.Syntax.VCONCC(S.Syntax.STRING (s,pos),
                                                build ss,pos)
                              | (s :: "||" :: ss) ->
                                  S.Syntax.HCONC(S.Syntax.STRING (s,pos),
                                               build ss,pos)
                              | (s :: ss) ->
                                  S.Syntax.HCONC(S.Syntax.STRING (s,pos),
                                               build ss,pos)
                          in
                            build (fst $1)
                          
                        }
        | SAMPLE Exp    { S.Syntax.SAMPLE ($2, $1) }
        | Exp SAMPLE Exp %prec HASH
	                { S.Syntax.SAMPLES ($1, $3, $2) }
        | Exp HCONC Exp { S.Syntax.HCONC ($1,$3,$2) }
        | Exp VCONCL Exp { S.Syntax.VCONCL ($1,$3,$2) }
        | Exp VCONCR Exp { S.Syntax.VCONCR ($1,$3,$2) }
        | Exp VCONCC Exp { S.Syntax.VCONCC ($1,$3,$2) }
	| QUESTION FLOAT	{ S.Syntax.QUESTION (fst $2, $1) }
	| FIRST Exp		{ S.Syntax.FIRST ($2, $1) }
	| SECOND Exp	{ S.Syntax.SECOND ($2, $1) }
	| LBRACK Exp COMMA Exp RBRACK
					{ S.Syntax.PAIR ($2, $4, $3) }
	| BANG Exp	{ S.Syntax.IF ($2, S.Syntax.EMPTY, S.Syntax.NUM (1,$1), $1) }
	| ID TILDE Exp	{ S.Syntax.DEFAULT (fst $1, $3, $2) }
	| LPAR Exp RPAR	{ $2 }
;

ExpList:
	  		{ S.Syntax.EMPTY }
	| ExpList1	{ $1 }
;

ExpList1:
	  Exp		{ $1 }
	| Exp COMMA ExpList1
			{ S.Syntax.CONC ($1,$3, $2) }
;

ExpList2:
	  Exp		{ [$1] }
	| Exp COMMA ExpList2
			{ $1 :: $3 }
;

%%