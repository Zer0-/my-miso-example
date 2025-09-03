{ nixpkgs ? import <nixpkgs> {} }:

let
  servant-miso-html = import ./nix-support/servant-miso-html.nix { inherit nixpkgs; };

  drv = nixpkgs.haskellPackages.callCabal2nix "my-miso-example" ./. {
    servant-miso-html = servant-miso-html;
  };

  drv2 = drv.overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [
      nixpkgs.zlib
    ];
  });

  env = drv2.env.overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [
      nixpkgs.haskellPackages.cabal-install
    ];
  });

in

  if nixpkgs.lib.inNixShell then env else drv
