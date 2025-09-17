#!/run/current-system/sw/bin/bash

set -e

# change directory to the location of this file
pushd $( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# compile to javascript
cabal build --with-compiler=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg

# for some reason this next part fails the first time it's run because the directory doesn't exist yet.
# if this happens just run the script again!

# figure out where the result of the compilation is on the filesystem
binpath=$(cabal list-bin client --with-compiler=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg)

echo $binpath
d="$(dirname $binpath)"
ls "$d"/client.jsexe

# copy the result to ./static
cp -v "$d"/client.jsexe/all.js static/all.js
