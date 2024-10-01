---
title: "More Haskell Diagrams: Images"
date: 2024-08-10 11:03:50
description: Embedding external images in Haskell diagrams.
taxonomies:
  tags:
    - Technical Note
    - Haskell
    - Hacking
    - Literate Programming
    - Computing
---

Let's continue exploring the Haskell [diagrams] library. In this post, we will
embed external images in our diagrams.

<!-- more -->

## Motivation

I would like to create images from Haskell diagrams with embedded images. At the
same time, I want to use [diagrams]' [cairo] backend ([diagrams-cairo]) instead
of the `svg` backend.

## Program

I am going to need 3 Haskell packages for this Literate Haskell program:
[diagrams], [diagrams-cairo] and [markdown-unlit]. It is noted in the [diagrams]
documentation page that the `cairo` backend is a little tricky to install in
some environments. Luckily, `nixpkgs` has a well-packaged version of
`diagrams-cairo`:

```nix
{
  ##...

  ghc = pkgs.haskellPackages.ghcWithPackages (hpkgs: [
    ## ...
    hpkgs.diagrams
    hpkgs.diagrams-cairo
    hpkgs.markdown-unlit
    ## ...
  ]);

  thisShell = pkgs.mkShell {
    buildInputs = [
      ## ...

      ghc

      ## ...
    ];

    NIX_GHC = "${ghc}/bin/ghc";
    NIX_GHCPKG = "${ghc}/bin/ghc-pkg";
    NIX_GHC_DOCDIR = "${ghc}/share/doc/ghc/html";
    NIX_GHC_LIBDIR = "${ghc}/lib/ghc-9.6.5/lib";
  };

  # ...
}
```

We will need to import a few modules.

```haskell
import Diagrams.Backend.Cairo
import Diagrams.Prelude
import System.Environment (getArgs)
```

Unlike the previous post, we are importing `Diagrams.Backend.Cairo` instead of
`Diagrams.Backend.SVG`. The API interface is pretty much the same, though.

Why `cairo`? Firstly, image clipping does not work with the `diagrams-svg`
backend for some reason. I guess why, but I will not speculate here. Secondly, I
want to see how `diagrams-cairo` backend works for me.

We will generate a few diagrams in this tutorial. Let's implement our entry
point:

```haskell
main :: IO ()
main = do
  dir <- head <$> getArgs
  logo <- loadLogo
  render dir "diagram-logo.png" logo
  render dir "diagram-logo-clipped.png" (mkAvatar logo)
  render dir "diagram-logo-clipped-with-text.png" (mkAvatarWithText "The Negation" logo)
  render dir "diagram1.svg" (mkAvatarWithText "SVG (~20KB)" logo)
  render dir "diagram1.png" (mkAvatarWithText "PNG (~12KB)" logo)
  render dir "diagram1.jpg" (mkAvatarWithText "JPG (~12KB)" logo)
  where
    render dpath fname = renderCairo (dpath <> "/" <> fname) (mkSizeSpec2D (Just 240) Nothing) . frame 0.2
```

This is how we will run our blog post:

```sh
runhaskell -pgmLmarkdown-unlit index.lhs .
```

Our objective is to create an avatar image for my blog. It will be a circle with
my Website's logo in it. We will also add some text under the avatar image.

This is how my Website's logo looks like as a raw PNG:

![](/android-chrome-512x512.png)

Let's load the image inside a `Diagram B` value. We are scaling the diagram to
`10`:

```haskell
loadLogo :: IO (Diagram B)
loadLogo = do
  (Right img) <- loadImageEmb "../../../static/android-chrome-512x512.png"
  pure $ scaleUToX 10 $ image img
```

![diagram-logo.png](diagram-logo.png)

Let's create a function to clip a given image in a circle. We will use the
`clipBy` function to achieve this. Since our square image is scaled to `10`, We
are using a circle with a radius of `5`:

```haskell
mkAvatar :: Diagram B -> Diagram B
mkAvatar =
  clipBy (circle 5)
```

![diagram-logo-clipped.png](diagram-logo-clipped.png)

I also want to add some text under the image:

```haskell
mkAvatarWithText :: String -> Diagram B -> Diagram B
mkAvatarWithText txt img =
  vsep 0.3 $
    [ mkAvatar img
    , text txt # fontSizeL 1 # fc black # frame 1
    ]
```

![diagram-logo-clipped-with-text.png](diagram-logo-clipped-with-text.png)

That's all! Let's see how SVG, PNG and JPG images look like:

<div class="flex flex-col md:flex-row justify-between">
  <img src="diagram1.svg" />
  <img src="diagram1.png" />
  <img src="diagram1.jpg" />
</div>

## Wrap-Up

I am getting used to the [diagrams] library. This onboarding process will take
some time until I get the fundamentals right, but I am happy with the progress
so far. It is also reassuring to see that this library is out for quite some
time, it is actively maintained, and there is a decent community around it.

I will continue to explore the library in my future posts.

<!-- REFERENCES -->

[diagrams]: https://diagrams.github.io
[cairo]: https://cairographics.org
[diagrams-cairo]: https://hackage.haskell.org/package/diagrams-cairo
[markdown-unlit]: https://hackage.haskell.org/package/markdown-unlit
