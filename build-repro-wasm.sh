# Run this file while inside of
#    nix shell 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org'

set -e

pushd $( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
#rm -rv ./dist-newstyle
wasm32-wasi-cabal build --allow-newer
sleep 0.2
wasm32-wasi-ghc --print-libdir
$(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i $(wasm32-wasi-cabal list-bin repro --allow-newer) -o static/repro-wasm.js
cp $(wasm32-wasi-cabal list-bin repro --allow-newer) ./static/repro-wasm.wasm
echo Done
#wasmtime $(wasm32-wasi-cabal list-bin exe:my-miso-example)
