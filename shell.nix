{ ... }:

let
  ## Import this codebase's Nix helper set:
  nix = import ./nix { };

  ## Get packages:
  pkgs = nix.pkgs;
in
pkgs.mkShell {
  buildInputs = [
    ## Fancy stuff:
    pkgs.figlet
    pkgs.lolcat

    ## Release stuff:
    pkgs.busybox
    pkgs.gh
    pkgs.git
    pkgs.git-chglog
  ] ++ nix.haskell-dev-tools;

  shellHook = ''
    figlet -w 999 "HASPARA DEV SHELL" | lolcat -S 42

    ## Make sure that doctest finds correct GHC executable and libraries:
    export NIX_GHC=${nix.ghc}/bin/ghc
    export NIX_GHC_LIBDIR=${nix.ghc}/lib/${nix.ghc.meta.name}
  '';
}
