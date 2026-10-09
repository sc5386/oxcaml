#!/usr/bin/env bash
set -euo pipefail
opam init --bare --disable-sandboxing --yes
if ! opam switch list --short | grep -qx guandan; then
  opam switch create guandan ocaml-base-compiler.4.14.2 --yes
fi
opam switch set guandan
eval "$(opam env --switch=guandan)"
opam install . --deps-only --with-test --yes
opam install ocaml-lsp-server ocamlformat --yes
dune runtest
dune build --profile release ui/tictactoe_ui.bc.js
cp _build/default/ui/tictactoe_ui.bc.js generated_js/tictactoe_ui.bc.js
