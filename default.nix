{ pkgs ? import ./haskell-pkgs.nix
, haskellCompiler ? "ghc8107"
}:
pkgs.haskell-nix.cabalProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "stylish-haskell";
    src = ./.;
  };

  compiler-nix-name = haskellCompiler;

  # need to make Cabal reinstallable, otherwise Haskell.nix uses the
  # version of Cabal that ships with the compiler even when that would
  # violate the constraint in stylish-haskell.cabal
  #
  # (eg nix-build failed because it tried to use Cabal-3.2.1.0 while
  # stylish-haskell needs Cabal >= 3.4 && < 3.7)
  #
  # See haskell-nix issue #1337 for details:
  # https://github.com/input-output-hk/haskell.nix/issues/1337
  modules = [
    ({ lib, ... }: {
      options.nonReinstallablePkgs = lib.mkOption { apply = lib.remove "Cabal"; };
    })
  ];
}
