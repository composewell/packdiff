# CAUTION! a spelling mistake in arg string is ignored silently.
#
# To use ghc-8.10.7
# nix-shell --argstr compiler "ghc8107"

{ nixpkgs ? import (builtins.fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/refs/tags/22.11.tar.gz") { }
, compiler ? "default" }:
let
  utils = let
    src = fetchGit {
      url = "git@github.com:composewell/composewell-haskell.git";
      ref = "master";
    };
  in (import "${src}/utils.nix") { inherit nixpkgs; };

  haskellPackages = let
    src = fetchGit {
      url = "git@github.com:composewell/composewell-haskell.git";
      ref = "master";
    };
  in (import "${src}/haskellPackages.nix") {
    inherit nixpkgs;
    inherit compiler;
  };

  mkHaskellPackages = inShell:
    haskellPackages.override (old: {
      overrides = nixpkgs.lib.composeExtensions (old.overrides or (_: _: { }))
        (with nixpkgs.haskell.lib;
          self: super: {
            packdiff = utils.local super "packdiff" ./. "" inShell;
            streamly =
                super.callHackageDirect
                  { pkg = "streamly";
                    ver = "0.9.0";
                    sha256 = "1ab5n253krgccc66j7ch1lx328b1d8mhkfz4szl913chr9pmbv3q";
                  } {};
            streamly-core =
                super.callHackageDirect
                  { pkg = "streamly-core";
                    ver = "0.1.0";
                    sha256 = "00lfgvicap41rxapzjcml0qxry1lx8501jarhjxrcpxy1plrb146";
                  } {};
            streamly-process = utils.composewell super "streamly-process"
              "d80b860d9d8ea98e4f7f63390442b3155c34dd08";
          });
    });

  shellDrv = mkHaskellPackages true;

  shell = utils.mkShell shellDrv (p: [ p.packdiff ]) true;

in if nixpkgs.lib.inNixShell then shell else (mkHaskellPackages false).packdiff
