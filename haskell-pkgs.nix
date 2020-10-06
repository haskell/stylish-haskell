let
  # Fetch the latest haskell.nix and import its default.nix
  haskellNix = import (builtins.fetchTarball{
    url = "https://github.com/input-output-hk/haskell.nix/archive/f6663a8449f5e4a7393aa24601600c8f6e352c97.tar.gz";
  }) {};

# haskell.nix provides access to the nixpkgs pins which are used by our CI,
# hence you will be more likely to get cache hits when using these.
# But you can also just use your own, e.g. '<nixpkgs>'.
  nixpkgsSrc = haskellNix.sources.nixpkgs-2003;

# haskell.nix provides some arguments to be passed to nixpkgs, including some
# patches and also the haskell.nix functionality itself as an overlay.
  nixpkgsArgs = haskellNix.nixpkgsArgs;

# import nixpkgs with overlays
in
  import nixpkgsSrc nixpkgsArgs
