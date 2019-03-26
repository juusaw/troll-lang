ocamlyacc parser.mly
echo "open Syntax\n\n$(cat parser.mli)" > parser.mli
ocamllex Lexer.mll
ocamlc -o troll \
 syntax.ml\
 parser.mli parser.ml \
 lexer.ml \
 interpreter.mli interpreter.ml \
 main.ml
