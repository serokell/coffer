{
  description = "Multi-backend password store with multiple frontends";

  nixConfig = {
    flake-registry = "https://github.com/serokell/flake-registry/raw/master/flake-registry.json";
  };

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    # Remove once registry-pinned haskell.nix is repinned
    haskell-nix.url = "github:input-output-hk/haskell.nix";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, haskell-nix, flake-utils, common-infra, serokell-nix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        src = pkgs.haskell-nix.haskellLib.cleanGit {
          name = "coffer";
          src = ./.;
        };

        pkgs = import nixpkgs {
          inherit system;
          overlays = [ haskell-nix.overlay serokell-nix.overlay ];
        };

        project = pkgs.haskell-nix.stackProject {
          inherit src;

          modules = [{
            packages.coffer = {
              # strip executable to reduce closure size
              dontStrip = false;
            };
          }];
        };

        inherit (project) coffer;
      in
      {
        packages = {
          coffer = coffer.components.exes.coffer;
        };

        checks = {
          reuse = pkgs.build.reuseLint src;
          trailingWhitespace = pkgs.build.checkTrailingWhitespace src;

          hlint = pkgs.build.haskell.hlint src;

          tests = coffer.components.tests.test;
          doctests = coffer.components.tests.doctests;
          lib = coffer.components.library;
          haddock = coffer.components.library.haddock;
        };

        devShell = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            ghc
            cabal-install
            haskell-language-server
            haskellPackages.implicit-hie
          ];
          buildInputs = with pkgs; [
            zlib
          ];
        };
      }) // {
        pipelineFile = common-infra.mkPipelineFile (self // {
          # Remove once https://github.com/serokell/common-infra/issues/4 is fixed
          deployFromPipeline = [];
        });
      };
}
