{
  description = "Essentials of Compilation: An Incremental Approach in Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    git-hooks.url = "github:cachix/git-hooks.nix";
    git-hooks.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    { flake-parts
    , git-hooks
    , ...
    }@inputs: flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      perSystem = { system, pkgs, ... }:
        let
          hpkgs = pkgs.haskell.packages.ghc912;

          eocia-haskell = pkgs.haskell.lib.overrideCabal (hpkgs.callCabal2nix "eocia-haskell" ./. { }) (_: {
            doCheck = true;
            doHaddock = false;
            enableLibraryProfiling = false;
            enableExecutableProfiling = false;
          });

          githooks = git-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              statix.enable = true;
              deadnix.enable = true;
              fourmolu.enable = true;
              cabal-fmt.enable = true;
              nixpkgs-fmt.enable = true;
            };
          };
        in
        {
          packages.default = eocia-haskell;

          checks = {
            inherit githooks;
          };

          devShells.default = pkgs.mkShell {
            packages = [
              hpkgs.cabal-install
              hpkgs.haskell-language-server
              hpkgs.fourmolu
              hpkgs.ghcid
              hpkgs.ghc
              pkgs.haskellPackages.cabal-fmt
            ];

            shellHook = ''
              ${githooks.shellHook}
            '';
          };
        };
    };
}
