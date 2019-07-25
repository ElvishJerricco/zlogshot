let
  pkgs = import (fetchGit {
    url = https://github.com/NixOS/nixpkgs-channels;
    ref = "nixos-19.03";
  }) {};

in pkgs.haskellPackages.override (old: {
  overrides = pkgs.haskell.lib.packageSourceOverrides {
    Chart = "1.9.1";
    Chart-diagrams = "1.9.2";

    zlogshot = ./.;
  };

  all-cabal-hashes = builtins.fetchurl {
    url = https://github.com/commercialhaskell/all-cabal-hashes/archive/e1c39d5161e66f11508b123503f6539d38e62900.tar.gz;
    sha256 = "1rw3lpidl0pg3ig1q882b5jaqxnhqknjbgy7prdhpa6vvxi9clph";
  };
})
