{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellPackages = nixpkgs.haskellPackages;

  #src = ../../miso;
  src = nixpkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "381d09ddba11fdbd2e9fe7ed2eb9b81c914b42da";
    sha256 = "sha256-nYykeIw3bhQALlajMNzv3SQqu3rnAukyecbw6jLbzUg=";
  };

  miso = haskellPackages.callCabal2nix "miso" src {};
in

  miso
