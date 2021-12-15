let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  # myReanimate =  pkgs.haskellPackages.reanimate.overrideAttrs (
  #   old: rec {
  #       buildInputs = old.buildInputs ++ [
  #         pkgs.darwin.apple_sdk.frameworks.CoreServices
  #         pkgs.darwin.apple_sdk.frameworks.CoreFoundation
  #         pkgs.darwin.apple_sdk.frameworks.Security
  #         pkgs.darwin.apple_sdk.frameworks.CoreGraphics
  #         pkgs.darwin.apple_sdk.frameworks.Carbon
  #       ];
  #   }
  # );
  myHaskellPackges = pkgs.haskellPackages.override {
    overrides = hself: hsuper: {
      "holmes" = pkgs.haskell.lib.unmarkBroken (pkgs.haskell.lib.dontCheck hsuper.holmes);
    };
  };

  haskellDeps = ps: with ps; [
    # hlint
    # brittany
    # ghcide
    # base
    relude
    megaparsec
    # conduit
    lens
    generic-lens
    matrix
    matrix-lens
    multiset
    holmes
    Stack
    memoize
    fgl
  ];
  my-ghc = myHaskellPackges.ghcWithPackages haskellDeps;
in
pkgs.mkShell {
  buildInputs = [
    my-ghc
  ];
}
