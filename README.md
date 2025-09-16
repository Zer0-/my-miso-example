[Demo](https://miso-example.volguine.com/)

# Build JavaScript:

nix develop .#ghcjs --experimental-features 'nix-command flakes'
./build-js.sh


# Build Wasm:

nix develop .#wasm --experimental-features 'nix-command flakes'
./build.sh
