{ mkDerivation, base, containers, mtl, random, stdenv }:
mkDerivation {
  pname = "x99-haskell-problems";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers mtl random ];
  license = stdenv.lib.licenses.mit;
}
