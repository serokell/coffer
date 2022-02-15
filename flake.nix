{
  inputs =
    { nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-21.11";
    };

  outputs = { self, nixpkgs }:
    with nixpkgs.lib;
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems' = genAttrs;
      forAllSystems = forAllSystems' supportedSystems;
      pkgsForSystem = system:
        import nixpkgs { inherit system; overlays = singleton self.overlay; };
    in
      {
        overlay = final: prev:
          {};

        devShell = forAllSystems
          (system:
            let
              pkgs = pkgsForSystem system;
            in
              pkgs.mkShell {
                nativeBuildInputs = with pkgs;
                  [ ghc
                    cabal-install
                    haskell-language-server
                    haskellPackages.implicit-hie
                  ];
                buildInputs = with pkgs;
                  [ zlib
                  ];
              }
          );
      };
}
