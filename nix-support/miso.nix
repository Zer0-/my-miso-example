{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellPackages = nixpkgs.haskellPackages;

  src = nixpkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "a4fcfe8b58a07008255bf29bffb6972bfe11e1ca";
    sha256 = "sha256-2IWGzS0yLQuA23+TXUbPOM57gxSRzFqRqZVUf0M282g=";
  };

  #src = ../../miso;

  miso = haskellPackages.callCabal2nix "miso" src {};
in

  miso
