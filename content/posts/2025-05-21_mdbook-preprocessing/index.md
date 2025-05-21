---
title: "Hacking with mdBook"
date: 2025-05-21 20:35:08
description: >
  Hacking an mdBook project with a few scripts for fun and profit.
taxonomies:
  tags:
    - Technical Notes
    - Technical Writing
    - Hacking
---

This post explores how to hack an [mdBook] project with scripts.

<!-- more -->

A few days back, I [wrote a blog post] about [static site generators], in particular
how I decided to migrate my blog from [Zola] to [Hugo]. One of my points was to be
able to _hack_ my own content before generating the final HTML.

## mdBook

[mdBook] is a Rust-based tool to create Web-based books from vanilla Markdown
files. Although it is quite minimalistic, you will bump into it quite often in
the wild. Most notably, the [Rust Book] uses it. I see it quite often in the [Nix]
ecosystem, too.

As I said, it is minimalistic. If you have a tree of Markdown files, you just
need a `SUMMARY.md` file to render them into a navigable book. The Markdown
flavor is [CommonMark], without any extensions and front-matter. This makes it
readable and navigable even on GitHub or GitLab without compiling the book.

I used `mdBook` in the past, and I reach for it every now and then.
Occasionally, my needs quickly grow bigger than what `mdBook` offers out of the
box. The good news is that I can help myself with a few scripts that pre-process
my Markdown files before building the book.

Today, I did it again, and I thought it would be a good idea to share my
approach with you.

## The Setup

Let us create a simple `mdBook` project:

```sh
nix run nixpkgs#mdbook -- init --title="mdBook Demo" --ignore=git
```

The directory structure looks like this along with a quick overview of the
files:

```console
$ tree
.
├── book               <- The output directory
├── book.toml          <- mdBook configuration
├── .gitignore         <- Files to ignore in Git tree
└── src                <- Content directory for Markdown files
    ├── chapter_1.md   <- Sample Markdown file
    └── SUMMARY.md     <- Sidebar navigation file

3 directories, 4 files
```

We are interested in the files under the `src` directory. In particular,
`src/SUMMARY.md` file must exist and refer to individual Markdown files for them
to be included in the book:

```md
# Summary

- [Chapter 1](./chapter_1.md)
```

## The Problem

I faced two problems.

The first problem was to manage metadata in my Markdown files. `mdBook` does not
allow any front-matter in the Markdown files. While there is a pre-processor
that removes front-matter, I want to utilize the metadata before it is stripped.

My second problem was me being too lazy to manage the `SUMMARY.md` file
manually. If I can put `weight` in the front-matter of the Markdown files, I can
use it to define the order of each chapter in the book. This is a common pattern
in many static site generators. Indeed, I wanted to add some further metadata to
Markdown files, such as _last review date_, _tags_, _todos_ and so on, to
generate more Markdown files in addition to `SUMMARY.md`.

## The Solution

Let us say we have two Markdown content files with front-matter:

1. `introduction.md`

   ```md
   ---
   title: "Introduction"
   weight: 1
   ---

   Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
   ```

2. `chapter_1.md`:

   ```md
   ---
   title: "Chapter 1"
   weight: 100
   ---

   Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
   ```

### The `SUMMARY.md` File

How about parsing the front-matter, sorting the records by `weight`, and
preparing the `SUMMARY.md` file with [mustache]?

First, let us pull the dependencies using Nix:

```sh
nix-shell -p jq -p yq-go -p mustache-go
```

We need a Mustache template, `SUMMARY.mustache`:

```mustache
# Summary

{{#.}}
- [{{title}}]({{path}})
{{/.}}
```

The following command will generate the `SUMMARY.md` file, running under the
`./src` directory for simplicity:

```sh
find . \
  -iname "*.md" -and -not -name "SUMMARY.md" \
  -exec yq --front-matter=extract --output-format=json '. += {"path": "{}"}' {} \; |
  jq --slurp "sort_by(.weight)" |
  mustache ../SUMMARY.mustache > SUMMARY.md
```

We `find` all Markdown files of interest, extract their front-matter as JSON and
inject the file `path` (relative to `src` directory) to the JSON object, slurp
individual JSON objects into an array and sort it by the `weight` field.
Finally, we use the JSON data to be rendered by the Mustache template into the
`SUMMARY.md` file.

Done! One task is complete.

### Individual Chapters

Each Markdown file is now rendered with the front-matter included. Instead, we
want to extract the `title` from the front-matter, add it as an `#` heading and
remove the front-matter.

This is done using the `mdBook` [pre-processor]. Our pre-processor is a simple
Python script, added to the `book.toml` file:

```toml
[preprocessor.my-preprocessor]
command = "python script.py"
```

What should the `script.py` file look like?

```python
import json
import sys

import frontmatter


def main():
    if len(sys.argv) > 1:
        if sys.argv[1] == "supports":
            sys.exit(0)

    context, book = json.load(sys.stdin)

    for section in book["sections"]:
        process_section(section)

    json.dump(book, sys.stdout)


def process_section(section):
    if "Chapter" in section:
        process_chapter(section["Chapter"])


def process_chapter(chapter):
    data = frontmatter.loads(chapter["content"])

    if data.keys():
        chapter["content"] = f"# {data['title']}\n\n{data.content}"


if __name__ == "__main__":
    main()
```

What does it do? `mdBook` will collect all Markdown files of interest, pack them
into an internal data definition (as a tuple of `[context]` and `[book]`), pass
it to the pre-processor as a JSON value from `stdin`, and expect the processed
book data to be returned as a JSON value from `stdout`.

Particularly in the above case:

1. `mdBook` will call our script with the `supports` argument to check if it
   supports the pre-processor. We do not care about it here, so the script just
   returns `0` to indicate success.
2. Otherwise, the script reads the JSON data from `stdin` into `context` and
   `book` symbols.
3. We are interested in the `book` value which has one or more `sections`.
4. We iterate over each section and process it.
5. For each section, we check if it is a chapter, and if so, process it.
6. For each chapter, we use the Python [frontmatter] library to extract the
   front-matter and the content. If there is any front-matter, we remove it by
   replacing the content with the `title` as an `h1` heading and the rest of the
   content.

Now, let us serve the book:

```sh
nix-shell \
  -p mdbook \
  -p python3Packages.python-frontmatter \
  --run "mdbook serve"
```

Done! You can now open your browser and navigate to
[http://localhost:3000](http://localhost:3000).

## Conclusion

I like to use `mdBook` for quick documentation. Sometimes, I end up managing
data in my Markdown files and I need to pre-process them before building the
book. Likewise, I may need to generate additional Markdown files for the book,
such as `SUMMARY.md` or other files.

The power of `mdBook` is due to its simplicity and clear design for
pre-processing.

I strongly recommend keeping it in your toolbox, along with other command-line
power tools, such as [jq], [yq], and [mustache]. No need to mention Nix, right?

<!-- REFERENCE -->

[CommonMark]: https://commonmark.org/
[Hugo]: https://gohugo.io/
[Nix]: https://nixos.org/
[Rust Book]: https://doc.rust-lang.org/book/
[Zola]: https://www.getzola.org/
[wrote a blog post]: https://thenegation.com/posts/migrate-from-zola-to-hugo/
[frontmatter]: https://pypi.org/project/python-frontmatter/
[jq]: https://jqlang.org/
[mdBook]: https://rust-lang.github.io/mdBook/
[mustache]: https://mustache.github.io/
[static site generators]: https://en.wikipedia.org/wiki/Static_site_generator
[yq]: https://mikefarah.gitbook.io/yq
[pre-processor]:
  https://rust-lang.github.io/mdBook/for_developers/preprocessors.html
[context]:
  https://docs.rs/mdbook/latest/mdbook/preprocess/struct.PreprocessorContext.html
[book]: https://docs.rs/mdbook/latest/mdbook/book/struct.Book.html
