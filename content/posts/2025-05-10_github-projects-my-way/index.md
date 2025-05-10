---
title: "GitHub Projects My Way"
date: 2025-05-10 22:33:27
description: >
  How I use GitHub Projects, Actions, and custom tooling to run all aspects of
  my personal and work projects.
taxonomies:
  tags:
    - Lifehack
    - Hacking
    - GitHub
---

This post explains why and how I use GitHub for nearly everything. I will also
explain how I use GitHub Actions to automate the creation of issues in my GitHub
Projects.

<!-- more -->

I am using [GitHub] for both personal and work projects. In the past, I used
[BitBucket], and at some point I considered using [GitLab], too. However, the
popularity of GitHub and its ecosystem made it hard to ignore. I even use GitHub
to follow trends in my profession.

## Full-on GitHub

So, I doubled down on GitHub and started using it for everything. Probably the
most important feature my team and I use is GitHub Projects. I dropped all other
project management tools and even my lifelong quest for the perfect project
management tool in favor of adopting GitHub Projects.

I am not a fan of the GitHub UI. The GitHub Projects UI is no improvement at
all. The functionality is just good enough to get the job done. But four things
made me forgo all the fancy features of other project management tools:

1. **Simplicity:** Fanciness is my archenemy when it comes to productivity.
   GitHub offers just the right amount of features to get the job done.
2. **Unit of Work:** I like that the unit of work on GitHub Projects is an issue
   or a pull request, which is my daily bread and butter anyway.
3. **Integration:** I use the GitHub CLI to work on issues and pull requests
   from my terminal, and GitHub Projects is becoming more and more integrated
   with the GitHub CLI. The same goes for the GitHub API.
4. **Automation:** I automate the creation of issues and pull requests on a
   schedule.

Here is one example of how our team uses GitHub Projects, Issues, and Actions
together.

## Forcing the Boring

Let me start by saying that boring things can be inevitable. Nobody wants to
spend time on boring things. But worst of all, nobody wants to remember boring
tasks, let alone do them.

My team usually works on fintech projects. We are a small company, but in the
financial services sector, there is no "small" when it comes to regulations and
compliance. Despite our automation efforts, we still have to manually attend
certain tasks. Furthermore, we have to perform these tasks on a regular basis,
which we must be able to demonstrate to ourselves and our customers.

Therefore, we schedule GitHub Actions to create issues for each member of our
team on a daily, weekly, monthly, and quarterly basis. These issues are nicely
formatted and contain all the information or links to relevant information,
usually with a checklist.

GitHub Projects has its own workflow automation, but I find it lacking.

## The Script

I chose to centralize the creation of these issues in a single GitHub
repository. Originally, the idea was to pick a suitable GitHub Action and use it
to create issues in the target repositories using a set of predefined templates.

Unfortunately, all the GitHub Actions I found on the [GitHub Marketplace] were limited
in various ways. Therefore, I decided to write my own script to create GitHub issues
from templates with appropriate users, assign them, and add them to the appropriate
GitHub Projects with the correct project fields filled out.

Since all my personal and work GitHub Projects share the same structure and
settings, the script applies to all of them.

Here is how the script works in a GitHub Action:

```yaml
on:
  schedule:
    - cron: "15 6 1 * *"

  workflow_dispatch:

name: "Create Monthly Issues"

permissions:
  contents: "read"
  issues: "write"

jobs:
  create:
    runs-on: "ubuntu-latest"
    steps:
      - name: "Checkout Codebase"
        uses: "actions/checkout@v4"

      - name: "Install Nix"
        uses: "DeterminateSystems/nix-installer-action@v16"

      - name: "Install gh-cpi"
        run: "nix profile install github:vst/gh-cpi"

      - name: "Create Issue: Monthly Security Checks"
        run: "gh-cpi"
        env:
          GH_TOKEN: "${{ secrets.CPI_GH_TOKEN }}"
          GH_CPI_ISSUE_FILE: "./issues/monthly-security-checks.md"

      - name: "Create Issue: Monthly Cloud Costs Review"
        run: "gh-cpi"
        env:
          GH_TOKEN: "${{ secrets.CPI_GH_TOKEN }}"
          GH_CPI_ISSUE_FILE: "./issues/monthly-cloud-costs-review.md"
```

In the above example, first, we are checking out the codebase, then we install
Nix and our script. Finally, we run the script to create two issues with the
templates available under the `./issues` directory in the repository.

Here is the first issue template:

```md
---
title: "Check Security Tests ({this_week})"
owner: "my-organization"
repository: "my-repository"
project: 11
assignees:
  - "my-github-user"
labels: []
status: "Todo"
iteration: "@next"
priority: "3"
size: "S"
difficulty: "E"
inception: "2022-06-06"
---

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec velit nunc.

- [ ] In nec purus efficitur, sodales ligula ut, lobortis turpis.
- [ ] Praesent congue nulla posuere iaculis ultricies.
- [ ] In quis arcu malesuada, convallis sapien id, mollis purus.
- [ ] Donec vestibulum tortor sed sapien rutrum laoreet.

> [!NOTE]
>
> Sed at dapibus ante. Vestibulum ante ipsum primis in faucibus orc ultrices
> posuere cubilia curae; Proin in diam at lacus ultricies.
>
> Nulla id ipsum eget lectus dignissim aliquet. Sed et laoreet justo, non
> vulputate.
```

This template is using YAML front matter to specify the issue title, repository
owner, repository name, GitHub Project number, assignees, labels, status,
iteration, priority, size, difficulty, and project inception date (to compute
the actual iteration).

I called the script [gh-cpi] and put it on GitHub as a public repository.
Currently, it works only with my peculiar GitHub Project structure, but I plan
to make it more generic and publish it on the [GitHub Marketplace] as a GitHub Action
once [GitHub Issue Types] are more mature in the GitHub API and GitHub CLI.

<!-- REFERENCES -->

[GitHub]: https://github.com
[BitBucket]: https://bitbucket.org
[GitLab]: https://gitlab.com
[GitHub Marketplace]: https://github.com/marketplace?type=actions
[GitHub Issue Types]:
  https://docs.github.com/en/issues/tracking-your-work-with-issues/configuring-issues/managing-issue-types-in-an-organization
[GitHub CLI]: https://cli.github.com
[gh-cpi]: https://github.com/vst/gh-cpi
[Nix]: https://nixos.org
[GitHub Actions]: https://github.com/features/actions
[GitHub Projects]:
  https://docs.github.com/en/issues/planning-and-tracking-with-projects/learning-about-projects/about-projects
