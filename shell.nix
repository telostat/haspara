{ pkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs/archive/22.05.tar.gz) { }
, ghcVersion ? "ghc902"
, ...
}:

with pkgs;

haskell.lib.buildStackProject rec {
  name = "haspara-devshell";
  src = ./.;
  ghc = haskell.packages.${ghcVersion}.ghc;
  buildInputs = [
    haskell-language-server
    haskellPackages.weeder
    hlint
    stack
    stylish-haskell
  ];
}
