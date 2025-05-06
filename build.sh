# Run this file while inside of
#    nix shell 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org'

set -e

pushd $( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
#rm -rv ./dist-newstyle
wasm32-wasi-cabal build
wasm32-wasi-ghc --print-libdir
$(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i $(wasm32-wasi-cabal list-bin exe:my-miso-example) -o static/wasm.js
cp $(wasm32-wasi-cabal list-bin exe:my-miso-example) ./static/wasm.wasm
echo Done
#wasmtime $(wasm32-wasi-cabal list-bin exe:my-miso-example)
