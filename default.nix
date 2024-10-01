{ sources ? import ./nix/sources.nix
, ...
}:

let
  ## Import nixpkgs pinned by niv:
  pkgs = import sources.nixpkgs { };

  ## Prepare the check script:
  dev-check = pkgs.writeShellScriptBin "dev-check" ''
    #!/usr/bin/env bash

    ## Fail on errors:
    set -e

    ## Run `taplo` to check toml files:
    taplo check
    taplo fmt --check

    ## Run prettier to check json, yaml and Markdown files (and maybe more):
    prettier --check .

    ## Run `nil` to check nix files:
    find . -iname "*.nix" -not -path "*/nix/sources.nix" -exec nil diagnostics {} \;

    ## Run `nixpkgs-fmt` to check nix formatting:
    find . -iname "*.nix" -not -path "*/nix/sources.nix" -print0 | xargs --null nixpkgs-fmt --check

    ## Compile the CSS:
    tailwindcss --minify --input styles/main.css --output static/styles/main.css

    ## Build the site:
    zola build
  '';

  ## Prepare the format script:
  dev-format = pkgs.writeShellScriptBin "dev-format" ''
    #!/usr/bin/env bash

    ## Fail on errors:
    set -e

    ## Run `taplo` to format toml files:
    taplo fmt

    ## Run prettier to format json, yaml and Markdown files (and maybe more):
    prettier --write .

    ## Run `nixpkgs-fmt` to format nix files:
    find . -iname "*.nix" -not -path "*/nix/sources.nix" -print0 | xargs --null nixpkgs-fmt

    ## Compile the CSS:
    tailwindcss --minify --input styles/main.css --output static/styles/main.css

    ## Build the site:
    zola build
  '';

  ## Prepare the serve script:
  dev-serve = pkgs.writeShellScriptBin "dev-serve" ''
    #!/usr/bin/env bash

    ## Fail on errors:
    set -e

    ## Compile the CSS:
    tailwindcss --minify --input styles/main.css --output static/styles/main.css

    ## Build the site:
    zola serve
  '';

  ## Prepare the build script:
  dev-build = pkgs.writeShellScriptBin "dev-build" ''
    #!/usr/bin/env bash

    ## Fail on errors:
    set -e

    ## Compile the CSS:
    tailwindcss --minify --input styles/main.css --output static/styles/main.css

    ## Temporarily copy assets to preserve external links to static assets:
    find content/posts/ -mindepth 1 -type d | while read -r _path; do
      _dir="$(basename "''${_path}" | cut -f 2 -d "_")"
      mkdir -p "static/assets/media/posts/''${_dir}"
      rsync -avP --exclude="*.md" --exclude="*.lhs" "''${_path}/" "static/assets/media/posts/''${_dir}"
    done
    cp content/posts/2012-01-15_redefining-the-ontology-of-accounting/REA_entity_diagram.png static/assets/media/posts

    ## Build the site:
    zola build
  '';

  ## Prepare the MD-reformat script:
  dev-md-format = pkgs.writeShellScriptBin "dev-md-format" ''
    #!/usr/bin/env bash

    _path="$(realpath "''${1}")"

    cd content/posts/2024-08-11_executable-blog-post-pandoc-filters/ && runhaskell -pgmLmarkdown-unlit index.lhs "''${_path}" && cd -
  '';

  ## Prepare the cross-post script (dev.to):
  dev-cross-post-devto = pkgs.writeShellScriptBin "dev-cross-post-devto" (builtins.readFile ./nix/src/cross-post-devto.sh);

  ## Prepare the cross-post script (hashnode):
  dev-cross-post-hashnode = pkgs.writeShellScriptBin "dev-cross-post-hashnode" (builtins.readFile ./nix/src/cross-post-hashnode.sh);

  #########
  ## GHC ##
  #########

  ghc = pkgs.haskellPackages.ghcWithPackages (hpkgs: [
    hpkgs.SVGFonts
    hpkgs.aeson
    hpkgs.diagrams
    hpkgs.diagrams-cairo
    hpkgs.markdown-unlit
    hpkgs.pandoc
    hpkgs.pandoc-lua-engine
    hpkgs.yaml
    hpkgs.haskell-language-server
  ]);

  ###########
  ## SHELL ##
  ###########

  ## Prepare Nix shell:
  thisShell = pkgs.mkShell {
    ## Build inputs for development shell:
    buildInputs = [
      ## Build dependencies:
      pkgs.mustache-go
      pkgs.nodejs_20
      pkgs.puppeteer-cli
      pkgs.tailwindcss
      pkgs.zola

      ## Development dependencies:
      ghc
      pkgs.git
      pkgs.jq
      pkgs.marksman
      pkgs.nil
      pkgs.nixpkgs-fmt
      pkgs.nodePackages.prettier
      pkgs.tailwindcss-language-server
      pkgs.taplo
      pkgs.yq-go

      ## Our custom scripts:
      dev-check
      dev-format
      dev-serve
      dev-build
      dev-md-format
      dev-cross-post-devto
      dev-cross-post-hashnode
    ];

    NIX_GHC = "${ghc}/bin/ghc";
    NIX_GHCPKG = "${ghc}/bin/ghc-pkg";
    NIX_GHC_DOCDIR = "${ghc}/share/doc/ghc/html";
    NIX_GHC_LIBDIR = "${ghc}/lib/ghc-9.6.5/lib";
  };
in
{
  shell = thisShell;
}
