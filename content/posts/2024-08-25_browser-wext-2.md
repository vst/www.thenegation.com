---
title: "Web Browser Extension Workshop - Part 2"
date: 2024-08-25 15:42:40
taxonomies:
  tags:
    - Technical Note
    - Web Development
    - Hacking
    - Computing
---

This is the second part of the series of blog posts about creating a Web browser
extension using [WXT]. In this part, we will try to read the OpenGraph tags of
the Webpage that is rendered on the active tab.

<!-- more -->

## Overview

In the previous post, we started working on a simple extension, [ogpatrol], that
reads the OpenGraph tags of the Webpage on the active tab and display them in
the popup.

So far, the extension is able to load and process the content of the Webpage on
the active tab. The next step is to extract the OpenGraph tags from the content
of the Webpage.

## OpenGraph Tags

A Webpage that supports OpenGraph protocol will have a set of meta tags in the
`<head>` section of the HTML document. These tags provide information about the
Webpage itself such as the title, description, and the image to be displayed
when the Webpage is shared on social media.

The OpenGraph tags are `<meta>` tags with a `property` attribute prefixed with
`og:`:

- `og:title` (required) - The title of the Webpage.
- `og:type` (required) - The type of the Webpage.
- `og:image` (required) - The URL of the image to be displayed when the Webpage
  is shared.
- `og:url` (required) - The URL of the Webpage.
- `og:description` - A brief description of the Webpage.
- `og:site_name` - The name of the site.
- `og:locale` - The locale of the Webpage.
- `og:determiner` - The word that appears before the title of the Webpage.
- `og:audio` - The URL of an audio file to be displayed with the Webpage.
- `og:video` - The URL of a video file to be displayed with the Webpage.
- etc...

## Implementation

So far, `ogpatrol` background script is able to load the content of the Webpage.
The next step is to extract the OpenGraph tags from the content of the Webpage.

We can do it manually, or use a library to parse the HTML content and extract
the OpenGraph tags. In this case, we will use
[`open-graph-scraper-lite`][open-graph-scraper-lite] library just because I am
too lazy to do it manually.

This library is a lightweight version of the original `open-graph-scraper` which
is a Node.js library and probably will not work in the browser environment.

First, we need to install the library:

```sh
npm install open-graph-scraper-lite
```

Then, we can use the library to extract the OpenGraph tags from the content of
the Webpage. Here is the diff of the `background.js` file:

```diff
diff --git a/entrypoints/background.ts b/entrypoints/background.ts
index 856e65c..bc8edf9 100644
--- a/entrypoints/background.ts
+++ b/entrypoints/background.ts
@@ -1,3 +1,5 @@
+import ogs from "open-graph-scraper-lite";
+
 export default defineBackground(() => {
   browser.tabs.onActivated.addListener(({ tabId }) => {
     process(tabId);
@@ -33,12 +35,26 @@ async function process(tabId: number) {
   // Cool, we have a nice tab!
   console.log("Tab activated:", url, hostname);

+  // Get the HTML content:
   const [{ result }] = await browser.scripting.executeScript({
     target: { tabId },
     func: () => {
-      return document.documentElement.outerHTML;
+      return document.head.innerHTML;
     },
   });

-  console.log(result);
+  // Attempt to parse the OpenGraph data:
+  ogs({ html: result })
+    .then(({ error, result }) => {
+      if (error) {
+        console.error("Error while trying to extract OpenGraph data.");
+        return;
+      }
+
+      // We should have the OpenGraph data now:
+      console.log("OpenGraph data is extracted successfully.", result);
+    })
+    .catch((error) => {
+      console.error("Error while trying to extract OpenGraph data.", error);
+    });
 }
```

As you see, the change is quite simple:

1. We import the `open-graph-scraper-lite` library.
2. We extract the content of the `<head>` section of the HTML document instead
   of using the entire document.
3. We use the `ogs` function to parse the OpenGraph tags from the content of the
   Webpage.

For now, we just log the extracted OpenGraph data to the console.

## Wrap Up

There are quite a few OpenGraph parser/extractor libraries available. I chose
`open-graph-scraper-lite` because it is lightweight, easy to use and developed
by the same authors of the original `open-graph-scraper` library. It fits our
use case quite well here.

Now that we have access to the OpenGraph data of the Webpage, we can store it in
the storage and offer users a rudimentary glimpse of it in the popup. This is
going to be the next step that we will cover in the next post.

<!-- REFERENCES -->

[WXT]: https://wxt.dev
[ogpatrol]: https://github.com/vst/ogpatrol
[open-graph-scraper-lite]: https://www.npmjs.com/package/open-graph-scraper-lite
