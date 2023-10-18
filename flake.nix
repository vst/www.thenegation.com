{
  description = "Nix Development Shell";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixos-unstable;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgs = import inputs.nixpkgs { inherit system; };
      in
      {
        devShell = nixpkgs.mkShell {
          buildInputs = [
            nixpkgs.nodejs_18
            nixpkgs.tailwindcss
            nixpkgs.taplo
            nixpkgs.zola
          ];
        };
      }
    );
}
