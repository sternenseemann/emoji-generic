{ mkDerivation, attoparsec, base, conduit, directory, file-embed
, filepath, optparse-applicative, smallcheck, stdenv, tasty
, tasty-hunit, tasty-smallcheck, text, utf8-light, xml-conduit
, xml-types
}:
mkDerivation {
  pname = "emoji-generic";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base file-embed text utf8-light
  ];
  executableHaskellDepends = [
    attoparsec base conduit directory filepath optparse-applicative
    text xml-conduit xml-types
  ];
  testHaskellDepends = [
    attoparsec base smallcheck tasty tasty-hunit tasty-smallcheck text
  ];
  description = "A generic Emoji library";
  license = stdenv.lib.licenses.lgpl3;
}
