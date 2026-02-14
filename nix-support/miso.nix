{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellPackages = nixpkgs.haskellPackages;

  #src = ../../miso;
  src = nixpkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "87f11b0e39576d89ca674d513a4d43bb31f1ce69";
    sha256 = "sha256-+xaq69x43NjOFxRO4YilWCPyRolXdaCLYnDcRHAiIZk=";
  };

  miso = haskellPackages.callCabal2nixWithOptions "miso" src "-fssr" {};
in

  miso
