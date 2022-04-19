{ pkgs ? import ./haskell-pkgs.nix }:
let
  hsPkgs = import ./. { inherit pkgs; };
in
hsPkgs.shellFor {
  # Include only the *local* packages of your project.
  # packages = ps: with ps; [
  # ];

  # Builds a Hoogle documentation index of all dependencies,
  # and provides a "hoogle" command to search the index.
  # withHoogle = true;

  # You might want some extra tools in the shell (optional).
  # Some common tools can be added with the `tools` argument
  tools = {
    cabal = "3.6.2.0";
    hlint = "3.3.6";
    haskell-language-server = "1.6.1.1";
  };
  # See overlays/tools.nix for more details

  # Some you may need to get some other way.
  buildInputs = [
    pkgs.ghcid
    pkgs.nixpkgs-fmt
    pkgs.stylish-haskell
  ];

  # Prevents cabal from choosing alternate plans, so that
  # *all* dependencies are provided by Nix.
  exactDeps = true;
}
