#!/usr/bin/env bash
# Builds an example into ./dist, ready to be served statically.
#
#   ./build.sh                    # TodoMVC
#   ./build.sh react-comparison   # examples from the React documentation
#   ./build.sh benchmark          # the page that benchmark/run.cjs measures
#
# cabal produces the wasm module; GHC's post-linker then generates the
# JavaScript FFI glue that index.js imports.
set -euo pipefail
cd "$(dirname "$0")"

case "${1:-todomvc}" in
  todomvc) exe=hastea-todomvc www=examples/todomvc/www ;;
  react-comparison) exe=hastea-react-comparison www=examples/ReactComparison/www ;;
  benchmark) exe=hastea-benchmark www=benchmark/www ;;
  *) echo "Unknown example: $1 (expected todomvc, react-comparison or benchmark)" >&2; exit 1 ;;
esac

# wasm32-wasi-cabal comes with ghc-wasm-meta. Set CABAL=cabal to use your own.
cabal=${CABAL:-wasm32-wasi-cabal}

"$cabal" build "exe:$exe"

dist=dist
rm -rf "$dist"
mkdir -p "$dist"
bin=$("$cabal" list-bin "exe:$exe")
# Older cabal versions leave the .wasm extension off.
[ -f "$bin" ] || bin="$bin.wasm"
cp "$bin" "$dist/hastea.wasm"
"$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" -i "$dist/hastea.wasm" -o "$dist/ghc_wasm_jsffi.js"
cp -r "$www"/. "$dist/"
# The WASI shim that index.js loads.
cp vendor/browser_wasi_shim.js "$dist/"

echo "Built $dist/ — serve it with e.g.: python3 -m http.server -d $dist 8080"
