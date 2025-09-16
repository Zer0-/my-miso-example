#!/run/current-system/sw/bin/bash

set -e

pushd $( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cabal build --with-compiler=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg
sleep 0.5
binpath=$(cabal list-bin client --with-compiler=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg)
echo $binpath
d="$(dirname $binpath)"
ls "$d"/client.jsexe
cp -v "$d"/client.jsexe/all.js static/all.js
