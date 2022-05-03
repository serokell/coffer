{
  description = "Multi-backend password store with multiple frontends";

  nixConfig = {
    flake-registry = "https://github.com/serokell/flake-registry/raw/master/flake-registry.json";
  };

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    # Remove once registry-pinned haskell.nix is repinned
    haskell-nix.url = "github:input-output-hk/haskell.nix";

    haskell-nix-weeder.flake = false;

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, haskell-nix, flake-utils, common-infra, serokell-nix, haskell-nix-weeder, ... }@inputs:
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

        weeder-hacks = import haskell-nix-weeder { inherit pkgs; };
        weeder-legacy = pkgs.haskellPackages.callHackageDirect {
          pkg = "weeder";
          ver = "1.0.9";
          sha256 = "0gfvhw7n8g2274k74g8gnv1y19alr1yig618capiyaix6i9wnmpa";
        } {};

        project = pkgs.haskell-nix.stackProject {
          inherit src;

          modules = [{
            packages.coffer = {
              # strip executable to reduce closure size
              dontStrip = false;
              package.ghcOptions = "-ddump-to-file -ddump-hi";
              postInstall = weeder-hacks.collect-dump-hi-files;
            };
          }];
        };

        inherit (project) coffer;
      in
      {
        packages = {
          coffer = coffer.components.exes.coffer;
          nix = pkgs.nixUnstable;
        };

        checks = {
          reuse = pkgs.build.reuseLint src;
          trailingWhitespace = pkgs.build.checkTrailingWhitespace src;

          hlint = pkgs.build.haskell.hlint src;

          weeder = let
            script = weeder-hacks.weeder-script {
              weeder = weeder-legacy;
              hs-pkgs = project;
              local-packages = [{
                  name = "coffer";
                  subdirectory = ".";
              }];
            };
          in pkgs.build.runCheck script;

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
            reuse
          ];
        };
      }) // {
        pipelineFile = (import ./pipe.nix inputs).mkPipelineFile (self // {
          # Remove once https://github.com/serokell/common-infra/issues/4 is fixed
          deployFromPipeline = [];
        });
      };
}
