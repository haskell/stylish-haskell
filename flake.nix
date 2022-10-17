{
  nixConfig.bash-prompt = "[nix-develop-stylish-haskell:] ";
  description = "stylish-haskell";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        deferPluginErrors = true;
        overlays = [
          haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            stylish-haskell =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc922";
                projectFileName = "stack.yaml";
                modules = [{
                  packages = { };
                }];
                shell.buildInputs = with pkgs; [
                  cabal-install
                  ghcid
                  hlint
                  nixpkgs-fmt
                ];
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.stylish-haskell.flake { };
      in
      flake // {
        defaultPackage = flake.packages."stylish-haskell:lib:stylish-haskell";
      });
}
