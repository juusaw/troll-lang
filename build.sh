#!/bin/bash 
case "$1" in
  ""|"native")
    echo "Compiling to native binary..."
    corebuild -use-menhir src/cli.native
    mv cli.native troll
    ;;
  "javascript")
    echo "Compiling to Javascript..."
    ocamlbuild -use-ocamlfind -pkgs 'base,js_of_ocaml,js_of_ocaml.ppx' -use-menhir src/javascript.byte
    js_of_ocaml +base/runtime.js javascript.byte
    mv javascript.js troll.js
    ;;
  *)
    echo "Illegal argument to build script"
    exit 1
    ;;
esac
exit 0
