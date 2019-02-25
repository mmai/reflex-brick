{ mkDerivation, base, brick, bytestring, containers, dependent-map
, dependent-sum, lens, linear, mtl, random, reflex
, reflex-basic-host, stdenv, vty
, http-client, lens-aeson, optparse-applicative, text, wreq
}:
mkDerivation {
  pname = "reflex-brick";
  version = "0.1.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base brick bytestring dependent-map dependent-sum lens mtl reflex
    reflex-basic-host vty
  ];
  executableHaskellDepends = [
    base brick containers lens linear mtl random reflex vty
    bytestring http-client lens-aeson optparse-applicative text wreq
  ];
  license = stdenv.lib.licenses.bsd3;
}
