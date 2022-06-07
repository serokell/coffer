{
  description = "Multi-backend password store with multiple frontends";

  nixConfig = {
    flake-registry = "https://github.com/serokell/flake-registry/raw/master/flake-registry.json";
  };

  inputs = {
    serokell-nix.url = "github:serokell/serokell.nix/notgne2/impure-checks";

    flake-utils.url = "github:numtide/flake-utils";

    # Remove once registry-pinned haskell.nix is repinned
    haskell-nix.url = "github:input-output-hk/haskell.nix";

    haskell-nix-weeder.flake = false;

    xrefcheck.url = "github:serokell/xrefcheck";
    xrefcheck.flake = false;

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, haskell-nix, flake-utils, common-infra, serokell-nix, haskell-nix-weeder, xrefcheck, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        inherit (nixpkgs) lib;

        pkgs' = pkgs;
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ haskell-nix.overlay serokell-nix.overlay ];
        };

        src = pkgs.haskell-nix.haskellLib.cleanGit {
          name = "coffer";
          src = pkgs.haskell-nix.haskellLib.cleanSourceWith {
            filter = path: type: !(pkgs.lib.hasInfix "tests/golden/helpers" path);
            src = ./.;
          };
        };

        weeder-hacks = import haskell-nix-weeder { inherit pkgs; };
        weeder-legacy = pkgs.haskellPackages.callHackageDirect {
          pkg = "weeder";
          ver = "1.0.9";
          sha256 = "0gfvhw7n8g2274k74g8gnv1y19alr1yig618capiyaix6i9wnmpa";
        } {};

        mkProject = { release ? false, pkgs ? pkgs' }: pkgs.haskell-nix.stackProject {
          inherit src;

          modules = [{
            packages.coffer = {
              # strip executable to reduce closure size
              dontStrip = !release;
              doHaddock = !release;
              ghcOptions = lib.optionals (!release) [ "-O0" "-ddump-to-file" "-ddump-hi" ];
              postInstall = if release then null else weeder-hacks.collect-dump-hi-files;
            };
          }];
        };

        checkProject = mkProject { release = false; };
        releaseProject = mkProject { release = true; };
        staticProject = mkProject { release = true; pkgs = pkgs.pkgsCross.musl64; };
      in
      {
        defaultPackage = self.packages."${system}".coffer;
        packages = ({
          coffer = releaseProject.coffer.components.exes.coffer;
          coffer-server = releaseProject.coffer.components.exes.coffer-server;
          nix = pkgs.nixUnstable;
        } // (if system == "x86_64-linux" then {
          coffer-static = staticProject.coffer.components.exes.coffer // { meta.artifacts = [ "/bin/coffer" ]; };
        } else {}));

        defaultApp = self.apps."${system}".coffer;
        apps.coffer = {
          type = "app";
          program = "${self.defaultPackage."${system}"}/bin/coffer";
        };

        checks = {
          reuse = pkgs.build.reuseLint src;
          trailingWhitespace = pkgs.build.checkTrailingWhitespace src;

          golden-tests = pkgs.runCommand "golden-tests" {
            buildInputs = with pkgs; [ vault bats ];
          } ''
            cd ${./.}
            bash ./scripts/run-bats-tests.sh
            touch $out
          '';

          stylish = pkgs.runCommand "stylish-check" {
            buildInputs = with pkgs; [ git gnumake stylish-haskell ];
          } ''
            mkdir src && cd src
            cp -a --no-preserve=mode,ownership ${src}/. .

            make stylish

            set +e
            diff=$(diff -q ${src} .)
            exitCode=$?
            set -e

            if [ "$exitCode" != 0 ]; then
                echo "Found files that do not adhere to stylish-haskell."
                echo "Run 'make stylish' on the repository to fix this."
                echo ""
                echo "Offending files:"
                echo "$diff"
                exit 1
            fi

            touch $out
          '';
          hlint = pkgs.build.runCheck "cd ${src} && ${pkgs.haskellPackages.hlint}/bin/hlint";
          shellcheck = pkgs.build.runCheck "find ${src} -name '*.sh' -exec ${pkgs.shellcheck}/bin/shellcheck {} +";

          weeder = let
            script = weeder-hacks.weeder-script {
              weeder = weeder-legacy;
              hs-pkgs = checkProject;
              local-packages = [{
                  name = "coffer";
                  subdirectory = ".";
              }];
            };
          in pkgs.build.runCheck "cp -a --no-preserve=mode,ownership ${src}/. . && ${script}";

          test = pkgs.build.runCheck "cd ${src} && ${checkProject.coffer.components.tests.test}/bin/test";
          # doctests = pkgs.build.runCheck "cd ${src} && ${checkProject.coffer.components.tests.doctests}/bin/doctests";
          lib = checkProject.coffer.components.library;
          haddock = checkProject.coffer.components.library.haddock;
        };

        impureChecks = {
          validate-cabal = let
            runtimeInputs = with pkgs; [
              (haskell.lib.overrideCabal haskellPackages.stack2cabal (drv: {
                jailbreak = true;
                broken = false;
              }))
              diffutils
            ];
          in pkgs.writeShellScript "validate-cabal" ''
            export PATH="${lib.makeBinPath runtimeInputs}:$PATH"
            ./scripts/validate-cabal-files.sh
          '';

          xrefcheck = pkgs.writeShellScript "xrefcheck-check" ''
            export PATH="${import xrefcheck {}}/bin:$PATH"
            xrefcheck --no-progress -m full --ignored tests/golden/helpers
          '';

          server-integration = pkgs.writeShellScript "server-integration" ''
            export PATH="${pkgs.vault}/bin:$PATH"
            ${checkProject.coffer.components.tests.server-integration}/bin/server-integration
          '';
        };

        devShell = self.devShells.${system}.default;
        devShells.default = pkgs.mkShell {
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
        pipelineFile = serokell-nix.lib.pipeline.mkPipelineFile self;
        validatesWithoutBuild = false;
        nixArgs = [ "--impure" ];
      };
}
