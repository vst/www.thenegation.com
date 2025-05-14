---
title: "Pomodoro Timer: Waybar and uair"
date: 2024-08-08 21:02:30
description: A brief note on my Pomodoro Technique setup with Waybar and uair.
taxonomies:
  tags:
    - Life Hack
    - Technical Note
    - Computing
---

Most life hackers know about the _[Pomodoro Technique]_. I tried it, and saw its
merits in the past, but I never adopted it as a habit. I decided to fully invest
in it this time, starting with my _Pomodoro Timer_ on my desktop.

<!--more-->

```txt
 Work 52:00 100%
```

## What is Pomodoro Technique?

_Pomodoro Technique_ is a time management method. In a nutshell, the technique
uses a timer to break down your time on a task into intervals:

1. Pick a task.
2. Set a timer, traditionally for 25 minutes.
3. Work on the task until the timer rings without distractions.
4. Take a short break, traditionally 5 minutes.
5. Repeat from 2 until you complete four intervals.
6. Take a longer break, traditionally 15-30 minutes.

It is not a _vague_ technique. Indeed, it is quite _strict_. For example: If
your work finishes before the timer rings, you are not _allowed_ to break the
interval: Go review your work, take notes about what you did, and plan your next
steps.

If you want more information, you can visit the Website and books of
[Francesco Cirillo](https://www.francescocirillo.com), the original author of
the technique.

## What is in it for me?

I take this technique seriously. It is not just about time management. It is
about awareness, focus and flow. I can even say that the first ballpoint of the
above technique is probably the most important one to me.

I tried pomodoro technique in the past, more than once. Unfortunately, I gave up
before I picked it up as a habit. The last time I gave it a shot was 2 months
ago when I saw even bigger positive impact on my productivity and mood. Yet
again, I abondoned it.

I do not always drop the ball. I pick up habits if I am into it. For example, I
am an early riser, as early as 4:30am. For the last 6-7 years, I set an alarm
clock, maybe twice?!

Similarly, I am recording my time on every task since November 2022 using a
command-line tool called [watson]:

```sh
$ watson log --all | head -n1
Monday 31 October 2022 (38m 43s)
```

I even integrated it into my [gh] setup as an extension:

```sh
$ cd ~/repos/vst/vst.github.io
$ gh watson
Starting project vst [gh:vst/vst.github.io] at 21:01
$ watson stop
...
```

It took a while to get used to it. But after 1 or 2 months, I was doing it
habitually. Soon after, I started realising how much time I spend on a project
of a specific type or interest. I started to see the patterns and the flow, as
well as the distractions. Nowadays, I can tell how much time I can or should
dedicate to work on a task: 3 hours, 5 hours, 2 days?

But purely because of laziness, I sometimes avoid making a fair assessment of a
task in the pipeline, and set a time budget from my gut feeling. In my last
attempt to use Pomodoro Technique, I told myself that I will never **Pick a
task** (remember _Step 1_?) without knowing the depth of it. Therefore, usually
a few days before, I give a single _pomodoro_ time to assess the task: Read
Keycloak documentation, see a couple of tutorials and read through fellow
hackers' NixOS configuration. That helped me immensely, but only for 2 weeks.

Also, I can get distracted easily. I can start working on a new Servant API
endpoint, and find myself refactoring the project's Nix setup. Pomodoro
Technique does not allow that!

As of this writing, I am trying to make it a habit again. I am not sure if I
will be successful this time, but I am determined to give it a shot until I can
call it a habit.

## Tools

Well, the world can run out of tomatoes, but will never be short of _Pomodoro
Timers_. I will not spoil it for you: Just Google it or go to kitchen.

I am currently using [uair] integrated in to my [Waybar] running on my [sway]
window manager. I use it similar to the way I use `watson` (both as a CLI tool
with [Waybar] integrations), so it is a good fit for me.

At any time, I see if my timer is running or not by checking the color of
`uairctl fetch` output in my [Waybar]: no color means no timer, red means timer
is running.

I can toggle the timer by clicking on it on my [Waybar] or issuing
`uairctl toggle` command in my terminal.

I can choose a different interval tied to a session ID by middle or right
clicking on my mouse, or simply issuing `uairctl jump <session-id>` or
`uairctl prev` or `uairctl next` in my terminal.

My [Waybar] shows now:

```txt
 Work 18:12 36%
```

## My Configuration

So, my configuration is as follows. First, my `uair` configuration:

```toml
## ~/.config/uair/uair.toml
[defaults]
format = "{time}\n"
paused_state_text = "paused"
resumed_state_text = "resumed"

[[sessions]]
id = "quick"
name = "Quick"
duration = "15m"
time_format = "%M:%S"
command = "notify-send 'Quick Done!'"

[[sessions]]
id = "work"
name = "Work"
duration = "52m"
time_format = "%M:%S"
command = "notify-send 'Work Done!'"

[[sessions]]
id = "rest"
name = "Rest"
duration = "17m"
time_format = "%M:%S"
command = "notify-send 'Rest Done!'"
```

My [Waybar] configuration has the following custom module for `uair`
(`~/.config/waybar/config` with `custom/uair` key):

```json
{
  "format": "{icon} {}",
  "format-icons": [
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    ""
  ],
  "tooltip": false,
  "return-type": "json",
  "interval": 5,
  "on-click": "uairctl toggle",
  "on-click-middle": "uairctl prev",
  "on-click-right": "uairctl next",
  "exec-if": "which uairctl",
  "exec": "uairctl fetch '{\"text\":\"{name} {time} {percent}%\",\"class\":\"{state}\",\"percentage\":{percent}}'"
}
```

... accompanied by its style:

```css
/* ~/.config/waybar/style.css */
#custom-uair.resumed {
  color: #f87171;
}
```

That's about it!

## Wrap-Up

I used a 52-minute pomodoro to write this post and I am about to finish it. Will
get back to you after my 17-minute break (See [52/17 Rule]).

<!-- REFERENCES -->

[Pomodoro Technique]: https://en.wikipedia.org/wiki/Pomodoro_Technique
[watson]: https://tailordev.github.io/Watson/
[gh]: https://cli.github.com
[uair]: https://github.com/metent/uair
[Waybar]: https://github.com/Alexays/Waybar
[sway]: https://swaywm.org
[52/17 Rule]:
  https://www.themuse.com/advice/the-rule-of-52-and-17-its-random-but-it-ups-your-productivity
