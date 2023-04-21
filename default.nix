{ pkgs ? import <nixpkgs> {}, compiler ? "ghc865" }:
let
  hsPkgs =  pkgs.haskellPackages; #pkgs.haskell.packages.${compiler}; # 
  pkg = hsPkgs.callCabal2nix "invertible-syntax" ./. {  };
  # was giving errors due to lorri stuff
  # shell = 
  #   (if pkgs.lib.inNixShell
  #    then pkg.env
  #    else pkg);
in
# shell
pkg
