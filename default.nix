{ pkgs ? import <nixpkgs> (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz))
, haskellCompiler ? "ghc865"
, lib ? pkgs.lib
, config ? pkgs.config
}:
  pkgs.haskell-nix.cabalProject {
    #src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = pkgs.nix-gitignore.gitignoreSource [] ./.; };
    ghc = pkgs.buildPackages.pkgs.haskell-nix.compiler.${haskellCompiler};
  }
