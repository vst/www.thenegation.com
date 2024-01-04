{ sources ? import ./nix/sources.nix
, system ? builtins.currentSystem
, ...
}:

let
  ##################
  ## LOAD NIXPKGS ##
  ##################

  ## Import nixpkgs pinned by niv:
  pkgs = import sources.nixpkgs { inherit system; };

  ###########
  ## SHELL ##
  ###########

  ## Prepare Nix shell:
  thisShell = pkgs.mkShell {
    ## Build inputs for development shell:
    buildInputs = [
      ## Build dependencies:
      pkgs.nodejs_18
      pkgs.tailwindcss
      pkgs.zola

      ## Development dependencies:
      pkgs.git
      pkgs.marksman
      pkgs.nil
      pkgs.nixpkgs-fmt
      pkgs.taplo
    ];
  };
in
{
  shell = thisShell;
}
