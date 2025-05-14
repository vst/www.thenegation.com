---
title: "Why I am Migrating From Zola Back to Hugo"
date: 2025-05-14 23:45:57
description: >
  Why I decided to settle back to Hugo after Zola -- a pragmatic comparison and
  lessons learned.
taxonomies:
  tags:
    - Technical Notes
    - Decision Making
    - Tools
    - Web
---

This post is a summary of my recent decision to go back to [Hugo] after using
[Zola]. I also report on how LLM assistants with Web access can aid in such
decisions, not as an authority but as a research assistant.

<!-- more -->

As a programmer, I made my first bucks back in 2002 by writing PHP code for a
Web site. Throughout the years, I have used various programming languages,
frameworks and tools to build Web sites, from marketing pages to complex, high
traffic e-Commerce platforms. Many things have changed since then, except for
two things:

1. The core of the Web was and still is HTTP, HTML and CSS.
2. It has always been a pain to build and a hell to maintain a Web site.

It is funny how every new generation of Web developers reinvents the wheel, only
to cry later -- just like their forefathers when they realize the wheel does not
work. The recent trend I find particularly amusing is the idea of "let the
frontend developers do the backend work." We tried the opposite in the early
2000s with ASP.NET Web Forms and JavaServer Faces, and it did not work out well.

Anyway, not my problem...

I am rather questioning the need for on-demand rendering of pages with
database-backed content. Most marketing sites, knowledge bases and documentation
can be static just fine. How many WordPress sites are truly dynamic?

I have been using static site generators for a very long time. Excluding the
experiments and short-term ones, I started with Jekyll, then moved to Hugo, and
finally to Zola. I still maintain several Hugo sites and am consistently
impressed by how low-maintenance they are.

I have a local site I use as a knowledge base, journal and scratchpad. I
recently migrated it from [mdBook] to Hugo, using the [Hextra] theme. The result
was so good that I started questioning my use of Zola as my primary static site
generator.

Staying true to my strategy of consolidating around fewer but quality tools, I
decided to reassess available static site generators. Since I had strong
opinions but needed a fresh lens, I asked my LLM assistant, `elelem`, to help me
assess the options using the most up-to-date information from the Web.

`elelem` is a helpful assistant, but not much of a decision-maker. So I helped
him define an assessment framework based on criteria under following headings:

1. Purpose of the Site
2. Developer Experience
3. Hosting and Deployment
4. Community and Ecosystem
5. Maintenance and Longevity
6. Content Format and Sourcing
7. Extensibility and Plugins
8. Build Performance
9. Styling and Theming
10. Internationalization (i18n)
11. Accessibility (a11y)
12. Analytics and Tracking
13. Security
14. Licensing
15. Extra, Specific Criteria

Under each heading, I defined must-haves, nice-to-haves and deal-breakers. The
last one, "Extra, Specific Criteria", is a catch-all for anything peculiar to
me. For example, I strongly prefer using Nix for development, testing, building
and deployment -- and I want to avoid anything JavaScript as much as possible.

`elelem` concluded that I am looking for a static site generator that is:

> 1. Purpose-flexible (suitable for many site types)
> 2. Tech-agnostic and standards-driven
> 3. Lightweight but extensible
> 4. Not reliant on JavaScript-heavy ecosystems
> 5. Markdown-first, with rich content support
> 6. Open-source with a permissive license
> 7. Well-suited to developer tooling (e.g., Nix, CI/CD)
>
> You have essentially specified the "ideal" general-purpose static site
> generator for a power user with strong preferences for transparency,
> portability and maintainability.

Then, he suggested that we compare the most popular static site generators:

1. [Hugo]
2. [Zola]
3. [Eleventy]
4. [Astro]
5. [mdBook]
6. [Jekyll]
7. [MkDocs]
8. [Pelican]

Hugo and Zola were by far the most fitting for my needs. Surprise!

It took me another hour of digging through Hugo's and Zola's documentation and
community forums -- and I made up my mind: I am going back to Hugo.

I am not a fan of the Go programming language, and Hugo's use of Go templates
has always been one of the most frustrating parts of it.

On the other hand, I like Rust and prefer Zola's simpler templating system.

However, these points led me to choose Hugo over Zola:

1. Zola is simple and fast, but for building more complex Web sites, Hugo fits
   better with its many features and extensibility via modules, shortcodes and
   its internal asset pipeline (not plugins in the traditional sense, but close
   enough). Zola has no plugin system or hooks into the build process -- only
   pre-/post-processing scripts, which introduce a new class of problems to the
   mix.
2. If I need to collaborate or hand over a project, I want something widely
   adopted with a large community. Hugo wins here by a great margin. As a test,
   I checked the GitHub star history of both projects. Hugo is still rising
   steadily, and Zola has no dramatic growth.
3. Certain Hugo features are just not available in Zola. For example:
   - Hugo includes a built-in asset pipeline to process and optimize images, CSS
     and JavaScript. Zola does not.
   - Hugo supports Subresource Integrity ([SRI]) for static assets. Zola does
     not, which previously bit me with cache issues.
   - Even though Zola's templating is easier, Hugo's is more powerful. Hugo
     shortcodes and templates can render Markdown directly or allow nesting
     them. Zola cannot -- which becomes annoying when I try to write reusable
     layouts or components.

Ultimately, I could still use Zola for smaller projects. But if Hugo is _the_
tool I will reach for on complex ones, I would rather standardize on it across
the board. Also, from a business standpoint: Hugo just makes sense. It is
mature, widely used and not going anywhere. It is probably easier to hire for,
too.

Let us see how many days it takes me to move my Web site from Zola to Hugo --
and how long before I regret the decision.

<!-- REFERENCES -->

[mdBook]: https://rust-lang.github.io/mdBook/
[Hugo]: https://gohugo.io/
[Zola]: https://www.getzola.org/
[Hextra]: https://github.com/imfing/hextra
[Eleventy]: https://www.11ty.dev/
[Astro]: https://astro.build/
[Jekyll]: https://jekyllrb.com/
[mkdocs]: https://www.mkdocs.org/
[Pelican]: https://blog.getpelican.com/
[SRI]:
  https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Subresource-Integrity
