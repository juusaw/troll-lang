# troll-lang

An implementation of the Troll programming language in Objective Caml.

Troll is a domain-specific programming language for describing dice rolls.

This project is based on the original implementation of Troll in Standard ML (MoscowML) and the paper describing the language (see [resources](#Resources)).

## Building the project

The project can be compiled using standard OCaml tools (`ocamlc`, `ocamllex`, `ocamlyacc`) available in the official OCaml distributions.

Run the command `./build.sh` to generate the binary.

To test that the program, try the following `echo "d20" | ./troll`

## TODO

- Port the distribution features available in the SML implementation
- Improve the command-line interface, add option flags
- Add examples of troll programs

## Resources

- [Troll language page](http://hjemmesider.diku.dk/~torbenm/Troll/)

- [The paper](http://hjemmesider.diku.dk/~torbenm/Troll/Troll-SAC.pdf)
