{ sources ? import ./nix/sources.nix
, compiler ? "default"
, system ? builtins.currentSystem
, ...
}:

let
  ##################
  ## LOAD NIXPKGS ##
  ##################

  ## Import nixpkgs pinned by niv:
  pkgs = import sources.nixpkgs { inherit system; };

  ##################
  ## LOAD HELPERS ##
  ##################

  ## Load the YAML reader:
  readYAML = pkgs.callPackage ./nix/lib/read-yaml.nix { };

  ## Load Haskell package factory:
  mkHaskell = pkgs.callPackage ./nix/lib/mk-haskell.nix { };

  ###########################
  ## ESSENTIAL INFORMATION ##
  ###########################

  ## Get the main Haskell package specification:
  packageSpec = readYAML (thisHaskellPackages.main.path + "/package.yaml");

  #############
  ## HASKELL ##
  #############

  ## Get Haskell packages in the project:
  thisHaskellPackages = {
    main = {
      name = packageSpec.name;
      path = ./.;
    };
    subs = [ ];
  };

  ## Get Haskell packages in the project as a list:
  thisHaskellPackagesAll = [ thisHaskellPackages.main ] ++ thisHaskellPackages.subs;

  ## Get base Haskell package set:
  baseHaskell = if compiler == "default" then pkgs.haskellPackages else pkgs.haskell.packages.${compiler};

  ## Get this Haskell package set:
  thisHaskell = mkHaskell {
    haskell = baseHaskell;
    packages = thisHaskellPackagesAll;
    overrides = self: super: { };
  };

  ###########
  ## SHELL ##
  ###########

  ## Prepare dev-test-build script:
  dev-test-build = pkgs.writeShellApplication {
    name = "dev-test-build";
    text = builtins.readFile ./nix/dev-test-build.sh;
    runtimeInputs = [ pkgs.bash pkgs.bc pkgs.moreutils ];
  };

  ## Prepare Nix shell:
  thisShell = thisHaskell.shellFor {
    ## Define packages for the shell:
    packages = p: builtins.map (x: p.${x.name}) thisHaskellPackagesAll;

    ## Enable Hoogle:
    withHoogle = false;

    ## Build inputs for development shell:
    buildInputs = [
      ## Haskell related build inputs:
      thisHaskell.apply-refact
      thisHaskell.cabal-fmt
      thisHaskell.cabal-install
      thisHaskell.cabal2nix
      thisHaskell.fourmolu
      thisHaskell.haskell-language-server
      thisHaskell.hlint
      thisHaskell.hpack
      thisHaskell.weeder

      ## Other build inputs for various development requirements:
      pkgs.docker-client
      pkgs.git
      pkgs.git-chglog
      pkgs.nil
      pkgs.nixpkgs-fmt
      pkgs.nodePackages.prettier
      pkgs.upx
      dev-test-build
    ];
  };
in
{
  shell = thisShell;
}
