---
title: "Executable Blog Posts: Second Take"
date: 2024-08-11 22:35:12
taxonomies:
  tags:
    - Technical Note
    - Haskell
    - Hacking
    - Pandoc
    - Literate Programming
    - Computing
---

This is a second take for my previous blog post [Abusing Haskell: Executable
Blog Posts]. This time, I am going to improve the solution with a [Lua] filter
for [pandoc].

<!-- more -->

## Background

One week ago, I published a blog post, [Abusing Haskell: Executable Blog Posts],
where I used a literate Haskell program to convert my raw blog posts in Markdown
format to a different Markdown format for cross-posting to different blogging
platforms. I used [pandoc] library.

## Falling Short

The problem was absurd. The solution was stupid. The execution was terrible.

At the end, my blog post program turned out to be good enough for only a week.
Then, I realised that I need one more feature: processing Markdown links and
image sources to include the base URL of my blog, so that the target platform
can render links and images correctly.

## Pandoc Filters to the Rescue

[pandoc] has a feature called [filters]. These filters are small programs that
can manipulate the AST of the document. They can be written in any language, but
the most common language is Lua as its interpreter is embedded in pandoc, and it
is faster compared to the JSON filter interface which is also used by other
languages (_You may wish to listen to the [Episode 37] of [The Haskell
Interlude] podcast, where [Joachim Breitner] and [David Christiansen] interview
[John MacFarlane], the creator of [pandoc], where he mentions Lua vs JSON
filters_).

Since I am abusing Haskell, why not abuse Lua as well? Let's write a Lua filter
to process the links and images in the Markdown document.

## Getting Ready

Let's review our dependencies which inside a [Nix shell] environment in my case.
Our only additional dependency is `pandoc-lua-engine`:

```nix
{
  ##...

  ghc = pkgs.haskellPackages.ghcWithPackages (hpkgs: [
    hpkgs.markdown-unlit
    hpkgs.pandoc
    hpkgs.pandoc-lua-engine
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

## The Implementation

For sanity, we need some language extensions:

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

For added pleasure, we need some imports:

```haskell
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Lua as P.Lua
```

Let's get the entrypoint function ready:

```haskell
main :: IO ()
main = do
  path <- head <$> getArgs
  iTxt <- TIO.readFile path
  oTxt <- convert "./static/assets/media/posts/executable-blog-post-pandoc-filters/filter.lua" iTxt
  TIO.putStrLn oTxt
```

Our workhorse function is finally getting more complicated, thank God:

1. Mark the input as [CommonMark] with YAML metadata block extension.
2. Mark the output as plain [CommonMark].
3. Apply a Lua filter to the AST to process the links and images.

Now, let's re-implement our workhorse function:

```haskell
convert :: FilePath -> T.Text -> IO T.Text
convert lua txt = P.runIOorExplode $ do
  md <- P.readCommonMark readerOptions txt
  fmd <- P.Lua.applyFilter P.def ["commonmark"] lua md
  P.writeCommonMark writerOptions fmd
  where
    readerOptions =
      P.def
        { P.readerExtensions = P.enableExtension P.Ext_yaml_metadata_block $ P.getDefaultExtensions "commonmark"
        , P.readerStripComments = True
        }
    writerOptions =
      P.def
        { P.writerExtensions = P.getDefaultExtensions "commonmark"
        , P.writerWrapText = P.WrapNone
        }
```

That's it as far as Haskell goes. Now, we need to write our Lua filter. I
[found] this gem:

```lua
local base_url = "https://thenegation.com"

function fix_link(url)
    return url:sub(1, 1) == "/" and base_url .. url or url
end

function Link(link)
    link.target = fix_link(link.target)
    return link
end

function Image(img)
    img.src = fix_link(img.src)
    return img
end

return {
    {
        Link = Link,
        Image = Image
    }
}
```

I am saving this in a [file] where my program can find it.

We are done with the program. We can run our blog post via `runhaskell` on our
blog post. But first, like last time, we need to symlink our Markdown file
(`.md`) with a literate Haskell file extension (`.lhs`) so that GHC is not
upset:

```sh
ln -sr \
  content/posts/2024-08-11_executable-blog-post-pandoc-filters.md \
  content/posts/2024-08-11_executable-blog-post-pandoc-filters.lhs
```

Then, we can run the blog post on the blog post itself:

```sh
runhaskell \
  -pgmLmarkdown-unlit \
  content/posts/2024-08-11_executable-blog-post-pandoc-filters.lhs \
  content/posts/2024-08-11_executable-blog-post-pandoc-filters.md
```

It worked on my computer!

## Wrap-Up

I used Lua for years to configure my [awesomewm] desktop environment. Then, I
started using it to configure my [Wezterm]. Since I bumped into an Emacs bug
([lsp-mode] bug to be fair), I switched quickly to [Neovim] after 20 years of
Emacs, and I am using Lua to configure my Neovim. Last but not least,
[OpenResty] gives my Nginx superpowers with Lua.

Lua is silently taking over my life. Now, I am using Lua to configure my blog
posts. I am not sure if I am getting better or worse.

As for my solution: Once stupid, always stupid. I promise it will get worse.

<!-- REFERENCES -->

[Abusing Haskell: Executable Blog Posts]: /posts/abuse-haskell/
[CommonMark]: https://commonmark.org
[David Christiansen]: https://davidchristiansen.dk
[Episode 37]: https://haskell.foundation/podcast/37/
[Joachim Breitner]: https://www.joachim-breitner.de
[John MacFarlane]: https://johnmacfarlane.net
[Lua]: https://www.lua.org
[Neovim]: https://neovim.io
[Nix shell]: https://wiki.nixos.org/wiki/Development_environment_with_nix-shell
[OpenResty]: https://openresty.org
[The Haskell Interlude]: https://haskell.foundation/podcast/
[Wezterm]: https://wezfurlong.org/wezterm/
[awesomewm]: https://awesomewm.org
[file]: /assets/media/posts/executable-blog-post-pandoc-filters/filter.lua
[filters]: https://pandoc.org/filters.html
[found]: https://github.com/jgm/pandoc/issues/4894
[lsp-mode]: https://emacs-lsp.github.io/lsp-mode/
[pandoc]: https://pandoc.org
