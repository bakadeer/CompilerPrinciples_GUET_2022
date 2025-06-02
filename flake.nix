{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem
    (
      system: let
        pkgs = import nixpkgs {
          inherit system;
          config.allowBroken = true;
        };
      in {
        # flake contents here
        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              (haskellPackages.ghcWithPackages (p:
                with p; [
                  alex
                  happy
                  haskell-language-server
                  cabal-install
                  stack
                  hpack
                ]))
            ];
          };
        };
      }
    );
}
