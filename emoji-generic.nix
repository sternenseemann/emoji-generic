{ mkDerivation, attoparsec, base, file-embed, stdenv, tasty
, tasty-smallcheck, text, utf8-light
}:
mkDerivation {
  pname = "emoji-generic";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base file-embed text utf8-light
  ];
  testHaskellDepends = [ tasty tasty-smallcheck ];
  description = "A generic Emoji library";
  license = stdenv.lib.licenses.lgpl3;
}
