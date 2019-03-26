ocamlyacc parser.mly
ocamllex Lexer.mll
ocamlc -o troll \
 syntax.ml\
 parser.mli parser.ml \
 lexer.ml \
 interpreter.mli interpreter.ml \
 main.ml
