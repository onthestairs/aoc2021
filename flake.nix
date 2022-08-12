{
  description = "dias";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        myHaskellPackages = pkgs.haskellPackages.override {
          overrides = hself: hsuper: {
            "holmes" = pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.unmarkBroken (pkgs.haskell.lib.dontCheck hsuper.holmes));
          };
        };

        haskellDeps = ps: with ps; [
          relude
          megaparsec
          lens
          generic-lens
          matrix
          multiset
          holmes
          Stack
          memoize
          fgl
        ];
        my-ghc = myHaskellPackages.ghcWithPackages haskellDeps;

      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            my-ghc
            myHaskellPackages.haskell-language-server
          ];
        };
      }
    );
}
