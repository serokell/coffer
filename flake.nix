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
      in
      {
        defaultPackage = self.packages."${system}".coffer;
        packages = ({
          coffer = (mkProject { release = true; }).coffer.components.exes.coffer;
          nix = pkgs.nixUnstable;
        } // (if system == "x86_64-linux" then {
          coffer-static = (mkProject { release = true; pkgs = pkgs.pkgsCross.musl64; }).coffer.components.exes.coffer // { meta.artifacts = [ "/bin/coffer" ]; };
        } else {}));

        defaultApp = self.apps."${system}".coffer;
        apps.coffer = {
          type = "app";
          program = "${self.defaultPackage."${system}"}/bin/coffer";
        };

        checks = let
          project = mkProject { release = false; };
        in {
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
              hs-pkgs = project;
              local-packages = [{
                  name = "coffer";
                  subdirectory = ".";
              }];
            };
          in pkgs.build.runCheck "cp -a --no-preserve=mode,ownership ${src}/. . && ${script}";

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
        pipelineFile = serokell-nix.lib.pipeline.mkPipelineFile self;
      };
}
