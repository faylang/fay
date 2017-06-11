{ nixpkgs ? (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "d3b66f9b946db3c612fb6f4c28efd9e46f924e97"; # release-17.03
    sha256 = "07x3h4ld4mzsbz6znx0wbx38kywf1bqnrdv7p41rg96dw2pr6lfk"; }
}:


let
  pkgs = import nixpkgs {};
  packageSetConfig = self: super: {
    fay = self.callPackage (import ./pkg.nix) {};
  };

  ghc7103 = pkgs.haskell.packages.ghc7103.override { inherit packageSetConfig; };
  ghc802 = pkgs.haskell.packages.ghc802.override { inherit packageSetConfig; };

in rec {
  fay_ghc7103 = ghc7103.fay;
  fay_ghc802 = ghc802.fay;
  fay_ALL = pkgs.runCommand "dummy" {
    buildInputs = [ fay_ghc802 fay_ghc7103 ];
  } "";
}
