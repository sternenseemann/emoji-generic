{ mkDerivation, attoparsec, base, file-embed, smallcheck, stdenv
, tasty, tasty-hunit, tasty-smallcheck, text, utf8-light
}:
mkDerivation {
  pname = "emoji-generic";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base file-embed text utf8-light
  ];
  testHaskellDepends = [
    base smallcheck tasty tasty-hunit tasty-smallcheck
  ];
  description = "A generic Emoji library";
  license = stdenv.lib.licenses.lgpl3;
}
