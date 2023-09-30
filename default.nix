{ pkgs ? import <nixpkgs> {}, hsPkgs ? pkgs.haskell.packages.ghc810}:
let
  #hsPkgs =  pkgs.haskellPackages;
  pkg = hsPkgs.callCabal2nix "invertible-syntax" ./. {  };
  # was giving errors due to lorri stuff
  # shell = 
  #   (if pkgs.lib.inNixShell
  #    then pkg.env
  #    else pkg);
in
# shell
pkg
