---
title: "Boring on Purpose: Bold Moves in Internal Tooling"
date: 2025-05-26 23:56:04
description: >-
  How boring work can lead to effective internal tooling with bold
  experimentation.
taxonomies:
  tags:
    - Technical Notes
    - Productivity
    - Hacking
---

Boring work is often the most effective work. It is the kind of work that might
lead us to understand patterns and solve problems that we did not even know we
had.

<!-- more -->

Tolerance to boredom and tedium is a superpower, especially for programmers.
Once we begin to recognize the patterns in our work, we can factor out the
repetitive parts, automate them, and focus on what actually requires our
attention. Tolerance is the key to such recognition in the first place.

Over the past few years, I have been reflecting on how I deal with work chores
-- the kind of tasks nobody volunteers for. Oddly enough, I have become more
effective and efficient at doing these. One unexpected benefit has been the
tools I ended up building for myself.

Sometimes, these tools are not just helpful for me -- they also turn out to be
useful for my colleagues.

## Basecamp

A few years back, I decided to replace our spreadsheet-based host and service
registries with a proper, Web-based asset registry. It took us a few days to
hack together a simple system that we could use to track our infrastructure
elements. It was not a big project -- just a simple app backed by a database,
[Hasura], and a React frontend. Since we were already using OpenID for
authentication, it was easy for our team to start using it.

The next immediate step was to build a CLI tool that would allow us to interact
with the registry from the command line. We used Haskell for this. One of the
most useful features was the ability to generate an entire SSH configuration
file from the registry data:

```sh
basecamp ssh config --output-file ~/.ssh/conf.d/work
```

This enabled us to register even less significant services in the registry --
ones we would not even bother to add to a spreadsheet. I could see quickly which
services were running on which hosts, and how they related to each other.

## Giving a Place to Ideas

The name _Basecamp_ was not a coincidence. Like a real basecamp, it served as a
starting point for further exploration -- not just of infrastructure, but of
ideas.

Since then, we have added an incident registry, a GitHub repository registry
with quick stats such as the number of open issues, PRs, etc., a knowledge base,
and most effectively, a simple expense tracker.

## Understanding Patterns and Relations

After effectively using Basecamp for a while, I started to notice new patterns I
could make use of. For example, I launched [HOSTPATROL], a simple tool for
collecting and consolidating information about hosts over SSH, and shipped
[clompse], the companion to HOSTPATROL for collecting and consolidating cloud
information.

As a result, I was able to run our asset registry review process in less than an
hour. Even better, I changed the frequency of certain quarterly reviews to
monthly, because I could do them so quickly.

The best part was that we managed to reduce our monthly cloud expenses by at
least 25 percent over the last year, simply by having a better overview of our
infrastructure and its costs.

## Boring and Bold

This is not a life-changing, motivational story. On the contrary, it is a boring
story that just happens to turn out well.

It did not start with a grand vision I was invested in. It started with the
reflex to replace a few spreadsheets with a simple but enabling tool.

It is boring, but also bold. It is bold because spending time on internal
systems goes against the grain in many organizations. And it is bold because it
meant investing in boring work that nobody wants to do.

The best part is that the total amount of time my team and I spent on this was
already amortized within a few months -- let alone the benefits we continue to
reap today.

## Conclusion

You might think of other alternatives to my approach of building internal tools
from scratch, such as using existing SaaS solutions or tools like Notion,
Airtable, headless CMS, etc.

In my concept, they are the same. One tolerates boring and tedious work,
recognizes patterns and relations, and builds structures that help to understand
and manage work better.

This kind of work is not about chasing the next big thing. It is not your next
startup idea. It is about building a system that works for you and your team,
makes your life easier, and gives you a foundation for new experiments. The
trick is to start small -- think weekend-sized.

Scratching your own itch can be risky: your use case may not generalize. But who
knows, maybe someday those experiments turn into something others want to use as
well. What you need is a good [Mom Test] to validate your problem and its solution.

<!-- References -->

[Hasura]: https://hasura.io
[React]: https://reactjs.org
[HOSTPATROL]: https://hostpatrol.io
[clompse]: https://github.com/vst/clompse
[Mom Test]: https://momtestbook.com
