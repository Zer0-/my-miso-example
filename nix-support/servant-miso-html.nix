{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellPackages = nixpkgs.haskellPackages;

  miso = import ./miso.nix { inherit nixpkgs; };

  src = nixpkgs.fetchFromGitHub {
    owner = "haskell-miso";
    repo = "servant-miso-html";
    rev = "00781d1920795b67e0476b67ed6840c388f29810";
    sha256 = "sha256-dYPlwSbQ+QXvMeS5tonBVnT9zQGADtohmD/ZAiY/cXA=";
  };

  servant-miso-html = haskellPackages.callCabal2nix "servant-miso-html" src {
    miso = miso;
  };
in

  servant-miso-html
