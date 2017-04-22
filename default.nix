{ pkgs ? import <nixpkgs> {} }:
let
  hps = pkgs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
     selda = self.callPackage ../selda/selda {};
     selda-sqlite = self.callPackage ../selda/selda-sqlite { };
     selda-postgresql = self.callPackage ../selda/selda-postgresql { };
   };
  };
in hps.callPackage ./selda-example.nix { }
