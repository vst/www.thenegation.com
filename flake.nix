{
  description = "The Negation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
  };

  outputs =
    { self, nixpkgs }:
    let
      lib = nixpkgs.lib;
      systems = lib.systems.flakeExposed;
      forAllSystems = lib.genAttrs systems;
    in
    {
      devShells = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          dev-md-format = pkgs.callPackage ./var/tools/dev-md-format { };
        in
        {
          default = pkgs.mkShell {
            packages = [
              pkgs.curl
              pkgs.git
              pkgs.hugo
              pkgs.jq
              pkgs.nodejs
              pkgs.pnpm
              pkgs.unzip
              pkgs.vscode-langservers-extracted
              pkgs.wrangler

              dev-md-format
              (pkgs.callPackage ./var/tools/dev-cross-post-devto { inherit dev-md-format; })
              (pkgs.callPackage ./var/tools/dev-cross-post-hashnode { inherit dev-md-format; })
            ];
          };

          ci = pkgs.mkShell {
            packages = [
              pkgs.curl
              pkgs.git
              pkgs.hugo
              pkgs.jq
              pkgs.nodejs
              pkgs.pnpm
              pkgs.unzip
            ];
          };
        }
      );
    };
}
