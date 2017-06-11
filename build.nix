{ nixpkgs ? (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "5bafc71b61301bac2233beee29985185ec95aa50"; # release-17.03
    sha256 = "1zh1dqm5pbfhrv6ww1dxlr9q2s5k3ixj35x1ghm4w3h7vh9alwqn"; }
}:


let
  pkgs = import nixpkgs {};
  packageSetConfig = self: super: {
    fay = self.callPackage (import ./pkg.nix) {};
  };

  ghc802 = pkgs.haskell.packages.ghc802.override { inherit packageSetConfig; };
  ghc7103 = pkgs.haskell.packages.ghc7103.override { inherit packageSetConfig; };


in pkgs.runCommand "dummy" { buildInputs = [ ghc802.fay ghc7103.fay ]; } ""
