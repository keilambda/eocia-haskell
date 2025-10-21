{
  description = "Essentials of Compilation: an Incremental Approach in Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    devshell.url = "github:numtide/devshell";
    devshell.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    { self
    , nixpkgs
    , flake-parts
    , devshell
    }@inputs: flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      perSystem = { system, pkgs, ... }:
        let
          hpkgs = pkgs.haskell.packages.ghc912;

          eocia-haskell = pkgs.haskell.lib.overrideCabal (hpkgs.callCabal2nix "eocia-haskell" ./. { }) (old: {
            doCheck = true;
            doHaddock = false;
            enableLibraryProfiling = false;
            enableExecutableProfiling = false;
          });
        in
        {
          _module.args.pkgs = import nixpkgs {
            localSystem = { inherit system; };
            overlays = [
              devshell.overlays.default
            ];
          };

          packages.default = eocia-haskell;

          devShells.default = pkgs.devshell.mkShell {
            packages = [
              hpkgs.cabal-install
              hpkgs.haskell-language-server
              hpkgs.fourmolu
              hpkgs.ghcid
              hpkgs.ghc
            ];

            commands = [
              {
                name = "test-watch";
                command = ''
                  ghcid -c "cabal repl eocia-haskell-test" -T "Main.main"
                '';
              }
            ];
          };
        };
    };
}
