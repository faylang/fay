{ pkgs, fay }:

let fay' = pkgs.haskell.lib.doCheck fay;
in pkgs.stdenv.mkDerivation {
  name = "fay-tests";
  version = fay'.version;
  src = ./.;
  checkPhase = "${fay'}/bin/fay-tests";
  installPhase = "touch $out";
}
