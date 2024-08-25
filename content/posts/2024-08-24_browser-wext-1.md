---
title: "Web Browser Extension Workshop - Part 1"
date: 2024-08-24 22:31:40
taxonomies:
  tags:
    - Technical Note
    - Web Development
    - Hacking
    - Computing
---

I am starting a short series of blog posts about creating a Web browser
extension using [WXT]. In this first part, we will set up the development
environment and try to read the content of the navigated Webpage or Webpage on
the active tab.

<!-- more -->

## Motivation

Not that I know what I am doing, but it sounds exciting...

Since I have started using computers _effectively_, a lot of things have
changed. It is quite normal if you consider the speed of technical advancements.

One of the things we used to do then was to use [Greasemonkey] scripts to modify
the behavior of websites. At some point, things stopped working, mostly because
browser vendors started to limit the capabilities of what such scripts can do
and not.

I gave up on Greasemonkey and started relying on browser extensions. Every once
in a while, I felt the need to reach out Greasemonkey again. But I never did.

Recently, I came to know that various Web browser vendors have started to
support a common set of APIs for creating browser extensions. This is a good
thing. Also, I noticed a few frameworks that make it easier to create browser
extensions. This is even better!

[WXT] is one of these frameworks. It looked simple enough to make me decide to
waste my time on it.

## Problem

Since my motivation is not rooted in a real-world, useful problem, I needed to
find one that can keep me interested.

I am not an SEO guy, but I am exposed to this OpenGraph thing. I understand what
it is and why that matters, but I do not know how others do it.

So, I asked my self a question: "How can I see the OpenGraph tags of a Webpage
that I am visiting?"

## Solution

Just right click on the Webpage and select the context menu item that says "View
Page Source". Done, but so dull!

## Overkill Solution

Let's write a browser extension that reads the OpenGraph tags of the Webpage
that is rendered on the active tab and show it in a popup when the extension
icon is clicked.

I do not know if it is even doable. But I even have a name for it: `ogpatrol`.

Let's see...

In this very post, we will create a GitHub repository for this project,
initialize the codebase and listen to tab events to read the content of the
Webpage. In the upcoming posts, we will try to read the OpenGraph tags and show
them in a popup.

## Getting Started

You do not need this post to get started as [WXT - Getting Started
Documentation] is good enough, but let me tell you what I did for good orders
sake.

I used `npx` and `wxt` to create the project:

```sh
npx wxt@latest init ogpatrol
```

Then entered the project directory and installed the dependencies:

```sh
cd ogpatrol && npm install
```

Finally, I ran the project in the development mode targeting [Firefox]:

```sh
npm run dev:firefox
```

This compiled the project and opened a new Firefox window with the extension
loaded. I could locate the extension icon in the toolbar, click on it and see
the Welcome popup. Great!

I also ran the project in the development mode targeting [Chrome]:

```sh
npm run dev
```

Same! The extension was loaded in a new Chrome (actually Chromium in my case)
window and I could see the Welcome popup.

Finally, I created a GitHub repository for the project and pushed the codebase:

```sh
gh repo create "ogpatrol" --public --push --source .
```

You can see it yourself [ogpatrol].

## Indulging in the Code

So far, nothing much has been achieved. No thrill, no joy.

How about we listen to tab events and read the content of the Webpage loaded or
activated?

Let's see the diff to the `background.ts` file:

```diff
diff --git a/entrypoints/background.ts b/entrypoints/background.ts
index f96fa48..856e65c 100644
--- a/entrypoints/background.ts
+++ b/entrypoints/background.ts
@@ -1,3 +1,44 @@
 export default defineBackground(() => {
-  console.log('Hello background!', { id: browser.runtime.id });
+  browser.tabs.onActivated.addListener(({ tabId }) => {
+    process(tabId);
+  });
+
+  browser.tabs.onUpdated.addListener((tabId, changeInfo) => {
+    if (changeInfo?.status === "complete") {
+      process(tabId);
+    }
+  });
 });
+
+async function process(tabId: number) {
+  // Get the tab:
+  const tab = await browser.tabs.get(tabId);
+
+  // Attempt to get the tab URL;
+  const url = tab.url ?? tab.pendingUrl;
+
+  // Return if we do not have a URL:
+  if (!url) {
+    return;
+  }
+
+  // Attempt to get the hostname:
+  const hostname = new URL(url).hostname;
+
+  // Return if we do not have a hostname:
+  if (!hostname) {
+    return;
+  }
+
+  // Cool, we have a nice tab!
+  console.log("Tab activated:", url, hostname);
+
+  const [{ result }] = await browser.scripting.executeScript({
+    target: { tabId },
+    func: () => {
+      return document.documentElement.outerHTML;
+    },
+  });
+
+  console.log(result);
+}
```

Let me explain what is going on here...

`background.ts` is the entrypoint for the background script. This script runs in
the background (surprise!) and listens to various events. We are listening to
two events: `tabs.onActivated` and `tabs.onUpdated`.

When a tab is activated or updated, we call our `process` function with the tab
identifier. The `process` function gets the tab, attempts to extract the URL and
hostname, and if successfull, it executes a script in the tab to get the content
of the Webpage and log it.

In my first attempt, it did not work, and instead it showed me an error in the
Browser Console:

```txt
Error: Missing host permission for the tab
```

Note that the Browser Console is different from the Web Console. You can open it
by pressing `Ctrl+Shift+J` in Firefox, and select the "Multiprocess" option to
see the console message from the background script.

It turned out to be a permission issue. We need to add make the following change
in the `wxt.config.ts` file:

```diff
diff --git a/wxt.config.ts b/wxt.config.ts
index 1e2f53d..2c6fc40 100644
--- a/wxt.config.ts
+++ b/wxt.config.ts
@@ -1,4 +1,9 @@
 import { defineConfig } from 'wxt';

 // See https://wxt.dev/api/config.html
-export default defineConfig({});
+export default defineConfig({
+  manifest: {
+    permissions: ["scripting"],
+    host_permissions: ["<all_urls>"],
+  },
+});
```

We added the `scripting` permission to access the `browser.scripting` API and
`<all_urls>` host permission to access the content of the Webpage.

The latter is why we all love browser extensions, right? Such a yummy
permission. We hate it when others do it, we love it when we do it. That's the
rule.

Anyway, by now, we can see the content of the Webpage in the Browser Console.

## Wrap Up

We started our journey to create a Web browser extension using [WXT]. We set up
the development environment and listened to tab events to read the content of
the Webpage by stripping our users' privacy rights. Lovely...

We will continue this journey in next posts by trying to read the OpenGraph tags
of the Webpage and show them in a popup.

<!-- REFERENCES -->

[Chrome]: https://www.google.com/chrome
[Firefox]: https://www.mozilla.org/en-US/firefox
[Greasemonkey]: https://www.greasespot.net
[WXT - Getting Started Documentation]:
  https://wxt.dev/get-started/introduction.html
[WXT]: https://wxt.dev
[ogpatrol]: https://github.com/vst/ogpatrol
