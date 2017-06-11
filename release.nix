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

  mkFayTest = fay: pkgs.callPackage (import ./pkg-fay-tests.nix) { inherit fay; };

  jobs = {
    fay_ghc802 = ghc802.fay;
    fay-tests_ghc802 = mkFayTest ghc802.fay;

    fay_ghc7103 = ghc7103.fay;
    fay-tests_ghc7103 = mkFayTest ghc7103.fay;
  };

in (jobs // {
 everything = pkgs.releaseTools.aggregate
   { name = "everything";
     meta.description = "Every job in release.nix";
     constituents = builtins.attrValues jobs;
   };
})

