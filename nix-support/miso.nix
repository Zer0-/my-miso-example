{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellPackages = nixpkgs.haskellPackages;

  src = nixpkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "e6798ccb6776352e506dfb47bed6f2c1ff77a859";
    sha256 = "sha256-rXi0JBngEIpVVYd/NnFs6g7fjL2DyYjTGtpsSUY1PH0=";
  };

  miso = haskellPackages.callCabal2nix "miso" src {};
in

  miso
