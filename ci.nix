# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

rec {
  sources = import ./nix/sources.nix;
  xrefcheck = import sources.xrefcheck;
  haskell-nix = import sources."haskell.nix" {
    sourcesOverride = { hackage = sources."hackage.nix"; stackage = sources."stackage.nix"; };
  };
  serokell-nix = import sources."serokell.nix";
  pkgs = import sources.nixpkgs (
    haskell-nix.nixpkgsArgs // {
      overlays =
        haskell-nix.nixpkgsArgs.overlays
        ++ [ serokell-nix.overlay ]; # contains trailing whitespace check
    }
  );

  local-packages = [{
      name = "coffer";
      subdirectory = ".";
  }];

  project-src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "coffer";
    src = pkgs.haskell-nix.haskellLib.cleanSourceWith {
      filter = path: type: !(pkgs.lib.hasInfix "tests/golden/helpers" path);
      src = ./.;
    };
  };

  project = (import ./coffer.nix { linux = true; });

  lib = project.coffer.components.library;
  cli = project.coffer.components.exes.coffer;
  tests = project.coffer.components.tests.test;
  doctests = project.coffer.components.tests.doctests;

  coffer-static = (import ./coffer.nix { linux-static = true; }).coffer.components.exes.coffer;
  coffer-windows = (import ./coffer.nix { windows = true; }).coffer.components.exes.coffer;

  trailing-whitespace-check = pkgs.build.checkTrailingWhitespace project-src;

  weeder-hacks = import sources.haskell-nix-weeder { inherit pkgs; };

  # nixpkgs has weeder 2, but we use weeder 1
  weeder-legacy = pkgs.haskellPackages.callHackageDirect {
    pkg = "weeder";
    ver = "1.0.9";
    sha256 = "0gfvhw7n8g2274k74g8gnv1y19alr1yig618capiyaix6i9wnmpa";
  } {};

  weeder-script = weeder-hacks.weeder-script {
    weeder = weeder-legacy;
    hs-pkgs = project;
    local-packages = local-packages;
  };

  # stack2cabal is broken because of strict constraints, set 'jailbreak' to ignore them
  stack2cabal = pkgs.haskell.lib.overrideCabal pkgs.haskellPackages.stack2cabal (drv: {
    jailbreak = true;
    broken = false;
  });
}
