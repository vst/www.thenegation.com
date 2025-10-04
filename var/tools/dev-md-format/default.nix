{ stdenv
, haskellPackages
, ...
}:

let
  ghc = haskellPackages.ghcWithPackages (hpkgs: [
    hpkgs.markdown-unlit
    hpkgs.pandoc
  ]);
in
stdenv.mkDerivation {
  name = "dev-md-format";
  version = "0.0.1";
  src = ../../../content/posts/2024-08-04_abuse-haskell;

  buildInputs = [
    ghc
  ];

  installPhase = ''
    mkdir -p $out/bin
    cp $src/index.md $out/bin/dev-md-format.lhs
    ghc -pgmLmarkdown-unlit $out/bin/dev-md-format.lhs
    rm $out/bin/dev-md-format.{o,hi,lhs}
  '';
}
