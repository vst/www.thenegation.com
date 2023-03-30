---
title: R Startup Profiles
date: 2012-02-07 08:03:01
tags: computing R
layout: post
---

It is a useful practice to customize the initialization of <a
href="http://www.r-project.org">R</a>. This is achieved through
<code>R_PROFILE</code> (system wide) and <code>R_PROFILE_USER</code>
(user specific) files which are both simple R scripts.

<!-- more -->

`R_PROFILE` is an environment variable, and by default, set to
`${R_HOME}/etc/Rprofile.site`. If such a file exists, it is evaluated
in the global environment of your R session.

Per-user customization is achieved through the `R_PROFILE_USER`
environment variable. After `R_PROFILE` has been evaluated, the file
which is pointed by `R_PROFILE_USER` is evaluated. If `R_PROFILE_USER`
is not set, a file named `.Rprofile` is searched first in the current
directory, then in the user home directory. If found, it is evaluated
in the global environment of your R session.

R documentation gives quite a good overview of the entire
initialization process of an R session. It is recommended to read this
(through `?.Rprofile`) since it gives some useful hints and warnings
as well.

I am keeping my own `.Rprofile` in my home directory. Here is the gist
of it:

<script src="https://gist.github.com/1765907.js?file=Rprofile.R"></script>
<link rel="stylesheet" href="https://gist.github.com/assets/embed-d35f263f218e837c50bd6571355ed383.css">
