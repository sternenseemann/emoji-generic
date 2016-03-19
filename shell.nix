{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, file-embed, parsec, split
      , stdenv, text, utf8-light
      }:
      mkDerivation {
        pname = "emoji";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring file-embed parsec split text utf8-light
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
