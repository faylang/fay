{ mkDerivation, aeson, base, base-compat, bytestring, containers
, data-default, data-lens-light, directory, filepath, ghc-paths
, haskell-src-exts, language-ecmascript, mtl, mtl-compat
, optparse-applicative, process, safe, sourcemap, split, spoon
, stdenv, syb, text, time, transformers, transformers-compat
, traverse-with-class, type-eq, uniplate, unordered-containers
, utf8-string, vector
, tasty, tasty-hunit, tasty-th
}:
mkDerivation {
  pname = "fay";
  version = "0.23.1.16";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base-compat bytestring containers data-default
    data-lens-light directory filepath ghc-paths haskell-src-exts
    language-ecmascript mtl mtl-compat process safe sourcemap split
    spoon syb text time transformers transformers-compat
    traverse-with-class type-eq uniplate unordered-containers
    utf8-string vector
  ];
  testHaskellDepends = [
    tasty tasty-hunit tasty-th
  ];
  executableHaskellDepends = [ base mtl optparse-applicative split ];
  homepage = "https://github.com/faylang/fay/wiki";
  description = "A compiler for Fay, a Haskell subset that compiles to JavaScript";
  license = stdenv.lib.licenses.bsd3;
  configureFlags = [ "-ftest" ];
}
