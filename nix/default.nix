{ compiler ? "ghc90"
, ...
}:

let
  ## Import sources:
  sources = import ./sources.nix;

  ## Import telosnix:
  telosnix = import sources.telosnix { };

  ## Import nixpkgs:
  pkgs = import telosnix.pkgs-sources.unstable { };

  ## Get Haskell for package development purposes:
  haskell = telosnix.tools.haskell.getHaskell
    {
      pkgs = pkgs;
      compiler = compiler;
    };

  ## Get this package:
  thisPackage = haskell.callCabal2nixWithOptions "haspara" ../. "--no-haddock" { };

  ## Get this package's Haskell dependencies:
  thisPackageDeps = pkgs.haskell.lib.compose.getHaskellBuildInputs thisPackage;

  ## Get our GHC for development:
  thisGhc = haskell.ghcWithPackages (_: thisPackageDeps);

  ## Get Haskell development tools:
  haskell-dev-tools = with haskell;
    [
      ## Our GHC with all packages required to build and test our package:
      thisGhc

      ## Various haskell tools:
      apply-refact
      cabal-install
      cabal2nix
      fourmolu
      haskell-language-server
      hlint
      hpack
      weeder
    ];
in
{
  sources = sources;
  telosnix = telosnix;
  pkgs = pkgs;
  haskell = haskell;
  thisPackage = thisPackage;
  thisPackageDeps = thisPackageDeps;
  ghc = thisGhc;
  haskell-dev-tools = haskell-dev-tools;
}
