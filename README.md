[Demo](https://miso-example.volguine.com/)

# Build JavaScript:

```bash
nix develop .#ghcjs --experimental-features 'nix-command flakes'
./build-js.sh
```


# Build Wasm:

```bash
nix develop .#wasm --experimental-features 'nix-command flakes'
./build.sh
```

# Build Server

```bash
nix-shell
cabal run
```
