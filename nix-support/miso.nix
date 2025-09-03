{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellPackages = nixpkgs.haskellPackages;

  src = nixpkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "dc2a5b18053791b4418743551d605529449734af";
    sha256 = "sha256-ek1LnuWxQugkEB2Y5Z4Qn6qWuNsyadXpmiaOwvuoplQ=";
  };

  miso = haskellPackages.callCabal2nix "miso" src {};
in

  miso
