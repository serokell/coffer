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

        inherit (pkgs) lib;

        weeder-hacks = import haskell-nix-weeder { inherit pkgs; };
        weeder-legacy = pkgs.haskellPackages.callHackageDirect {
          pkg = "weeder";
          ver = "1.0.9";
          sha256 = "0gfvhw7n8g2274k74g8gnv1y19alr1yig618capiyaix6i9wnmpa";
        } {};

        mkProject = release: pkgs.haskell-nix.stackProject {
          inherit src;

          modules = [{
            packages.coffer = {
              # strip executable to reduce closure size
              dontStrip = !release;
              doHaddock = !release;
              ghcOptions = lib.optionals (!release) ["-ddump-to-file" "-ddump-hi"];
              postInstall = if release then null else weeder-hacks.collect-dump-hi-files;
            };
          }];
        };
      in
      {
        defaultPackage = self.packages."${system}".coffer;
        packages = {
          coffer = (mkProject true).coffer.components.exes.coffer;
          nix = pkgs.nixUnstable;
        };

        defaultApp = self.apps."${system}".coffer;
        apps.coffer = {
          type = "app";
          program = "${self.defaultPackage."${system}"}/bin/coffer";
        };

        checks = let
          project = mkProject false;
        in {
          reuse = pkgs.build.reuseLint src;
          trailingWhitespace = pkgs.build.checkTrailingWhitespace src;

          hlint = pkgs.build.runCheck "cd ${src} && ${pkgs.haskellPackages.hlint}/bin/hlint";
          shellcheck = pkgs.build.runCheck "find ${src} -name '*.sh' -exec ${pkgs.shellcheck}/bin/shellcheck {} +";

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

          test = pkgs.build.runCheck "cd ${src} && ${project.coffer.components.tests.test}/bin/test";
          doctests = pkgs.build.runCheck "cd ${src} && ${project.coffer.components.tests.doctests}/bin/doctests";
          lib = project.coffer.components.library;
          haddock = project.coffer.components.library.haddock;
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
            haskellPackages.hlint
            shellcheck
          ];
        };
      }) // {
        pipelineFile = (import ./pipe.nix inputs).mkPipelineFile (self // {
          # Remove once https://github.com/serokell/common-infra/issues/4 is fixed
          deployFromPipeline = [];
        });
      };
}
