inputs:
rec {
  mkPipeline = { deploy ? { nodes = { }; }, packages ? { }, checks ? { }, deployFromPipeline ? { }, agents ? [ ]
    , systems ? [ "x86_64-linux" ], ciSystem ? "x86_64-linux", ... }@args:
    let
      pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
      inherit (pkgs) lib;
      inherit (lib)
        getAttrFromPath collect concatStringsSep mapAttrsRecursiveCond
        optionalAttrs optionalString concatMapStringsSep splitString last head
        optional optionals escapeShellArg;
      inherit (builtins)
        concatMap length filter elemAt listToAttrs unsafeDiscardStringContext;

      escapeAttrPath = path: escapeShellArg ''"${concatStringsSep ''"."'' path}"'';

      nixBinPath = optionalString (packages ? ${ciSystem}.nix) "${packages.${ciSystem}.nix}/bin/";

      namesTree =
        mapAttrsRecursiveCond (x: !(lib.isDerivation x)) (path: _: path);
      names = attrs: collect lib.isList (namesTree attrs);

      filterNative = what:
        listToAttrs (concatMap (system:
          optional (args.${what} ? ${system}) {
            name = system;
            value = args.${what}.${system};
          }) systems);

      buildable = {
        inherit deploy;
        packages = filterNative "packages";
      };

      packageNames = filter (x: last x == "path" || head x == "packages")
        (names buildable);

      pathByValue = listToAttrs (map (path: {
        name =
          unsafeDiscardStringContext (getAttrFromPath path buildable).drvPath;
        value = path;
      }) packageNames);

      drvFromPath = path: getAttrFromPath path buildable;

      build = comp:
        let
          drv = drvFromPath comp;
          hasArtifacts = drv ? meta && drv.meta ? artifacts;
          displayName = if head comp == "packages" then
            elemAt comp 2
          else
            "${elemAt comp 2}.${elemAt comp 4}";
        in {
          label = "Build ${displayName}";
          command = "${nixBinPath}nix build .#${escapeAttrPath comp}";
          inherit agents;
        } // optionalAttrs hasArtifacts {
          artifact_paths = map (art: "result${art}") drv.meta.artifacts;
        };

      buildSteps = map build (builtins.attrValues pathByValue);

      checkNames = names { checks = filterNative "checks"; };

      check = name: {
        label = elemAt name 2;
          command = "${nixBinPath}nix build --no-link .#${escapeAttrPath name}";
        inherit agents;
      };

      checkSteps = map check checkNames;

      doDeploy = { branch, node ? branch, profile, user ? "deploy", ... }: {
        label = "Deploy ${branch} ${profile}";
        branches = [ branch ];
        command =
          "${
            inputs.deploy.defaultApp.${head systems}.program
          } ${lib.escapeShellArg ''.#"${node}"."${profile}"''} --ssh-user ${lib.escapeShellArg user} --fast-connection true";
        inherit agents;
      };

      deploySteps = [ "wait" ] ++ map doDeploy deployFromPipeline;

      doRelease = {
        label = "Release";
        branches = args.releaseBranches or [ "master" ];
        command = pkgs.writeShellScript "release" ''
          set -euo pipefail
          export PATH='${inputs.nixpkgs.legacyPackages.${ciSystem}.github-cli}/bin':"$PATH"
          nix build .#'release.${ciSystem}'
          timestamp=$(git show -s --format=%ci)
          date=$(cut -d\  -f1 <<< $timestamp)
          time=$(cut -d\  -f2 <<< $timestamp | sed s/:/-/g)
          gh release create "$date"T"$time" -d -t "Automatic release on $date" -F result/notes.md ./result/*
          sleep 1
          # Clean up old draft releases
          for draft_tag in $(gh release list -L 1000 | grep Draft | tail +${toString ((args.keepReleaseDrafts or 1) + 1)} | cut -f3); do
            gh release delete -y "$draft_tag"
          done
        '';
      };

      releaseSteps = optionals (args ? release) [ "wait" doRelease ];

      steps = buildSteps ++ checkSteps ++ releaseSteps ++ deploySteps;
    in { inherit steps; };

  mkPipelineFile = { systems ? [ "x86_64-linux" ], ... }@flake:
    inputs.nixpkgs.legacyPackages.${builtins.head systems}.writeText "pipeline.yml"
    (builtins.toJSON (mkPipeline flake));
}
