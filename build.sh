#!/usr/bin/env bash
# Builds the TodoMVC example into ./dist, ready to be served statically.
#
# cabal produces the wasm module; GHC's post-linker then generates the
# JavaScript FFI glue that index.js imports.
set -euo pipefail
cd "$(dirname "$0")"

# wasm32-wasi-cabal comes with ghc-wasm-meta. Set CABAL=cabal to use your own.
cabal=${CABAL:-wasm32-wasi-cabal}

"$cabal" build exe:hastea-todomvc

dist=dist
mkdir -p "$dist"
bin=$("$cabal" list-bin exe:hastea-todomvc)
# Older cabal versions leave the .wasm extension off.
[ -f "$bin" ] || bin="$bin.wasm"
cp "$bin" "$dist/hastea.wasm"
"$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" -i "$dist/hastea.wasm" -o "$dist/ghc_wasm_jsffi.js"
cp examples/todomvc/www/* "$dist/"

echo "Built $dist/ — serve it with e.g.: python3 -m http.server -d $dist 8080"
