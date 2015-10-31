{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, attoparsec, base, bytestring, parsec, split
      , stdenv, text
      }:
      mkDerivation {
        pname = "emoji";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          attoparsec base bytestring parsec split text
        ];
        description = "Emoji name expander for different string types";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
