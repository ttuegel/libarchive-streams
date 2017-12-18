let
  #nixpkgs = import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/afe9649210cace6d3ee9046684d4ea27dc4fd15d.tar.gz) {};
  nixpkgs = import ../nixpkgs {};
in
{ compiler ? "ghc822" }:

let

  inherit (nixpkgs) pkgs;

  overrides =
    if builtins.pathExists ./overrides.nix
    then import ./overrides.nix pkgs pkgs.haskell.lib
    else self: super: {};

  haskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages.override { inherit overrides; }
    else pkgs.haskell.packages.${compiler}.override { inherit overrides; };

  drv = haskellPackages.callPackage ./. { archive = pkgs.libarchive; };

in

  if pkgs.lib.inNixShell then drv.env else drv
