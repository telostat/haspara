{ compiler ? "ghc90"
, ...
}:


let
  nix = import ./nix { compiler = compiler; };
in
nix.thisPackage
