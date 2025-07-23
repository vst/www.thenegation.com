---
title: "Web Browser Extension Workshop - Part 3"
date: 2024-08-26 21:53:14
description:
  Changing the extension icon based on the OpenGraph information parsed from the
  Webpage.
slug: browser-wext-3
tags:
  - Technical Note
  - Web Development
  - Hacking
  - Computing
---

This is the third part of the series of blog posts about creating a Web browser.
In this part, we will change the extension icon based on the OpenGraph
information parsed from the Webpage on the active tab. We will also refactor the
code by encoding parse results in a better type.

<!--more-->

## Overview

So far, we managed to read the OpenGraph tags of the Webpage on the active tab.
However, users still do not see any visual feedback from the extension. In this
part, we will change the extension icon based on the OpenGraph information
parsed from the Webpage.

Before doing so, we will refactor the code by encoding the parse results in a
better type first.

You can check the code on the [GitHub repository][ogpatrol].

Let's start.

## Refactoring

We have 3 possible results from the parsing process:

1. **Not Applicable:** If the browser tab is not a Webpage, we should not even
   bother parsing the content. This is a special case that we should handle
   separately.Our heuristic is if the tab does not open a URL with a proper
   hostname.
2. **Error:** If there is an error during the parsing process, we should handle
   it gracefully.
3. **Success:** If the parsing process is successful, we should have the
   OpenGraph information. However, the library we are using does not guarantee
   that all or even any of the OpenGraph tags are present. Instead, it returns a
   dictionary with the tags that are present.

We will encode these results in a new type called `ParseResult`:

```typescript
export type ParseResult =
  | ParseResultSuccess
  | ParseResultError
  | ParseResultNotApplicable;

export type ParseResultSuccess = {
  status: "success";
  ogdata: OgObject;
};

export type ParseResultError = {
  status: "error";
  ogdata?: OgObject;
};

export type ParseResultNotApplicable = {
  status: "not-applicable";
};
```

You may have noticed the `OgObject` type. This is the type that represents the
OpenGraph tags. It is not exported from the `open-graph-scraper-lite` library,
so we need to do some _type mining_ to make it work:

```typescript
import type { SuccessResult } from "open-graph-scraper-lite";

export type OgObject = SuccessResult["result"];
```

Once we refactor the `background.ts` module to use the new types, the diff looks
like this:

```diff
diff --git a/entrypoints/background.ts b/entrypoints/background.ts
index bc8edf9..3cb3422 100644
--- a/entrypoints/background.ts
+++ b/entrypoints/background.ts
@@ -1,4 +1,5 @@
 import ogs from "open-graph-scraper-lite";
+import type { SuccessResult } from "open-graph-scraper-lite";

 export default defineBackground(() => {
   browser.tabs.onActivated.addListener(({ tabId }) => {
@@ -12,7 +13,15 @@ export default defineBackground(() => {
   });
 });

-async function process(tabId: number) {
+export async function process(tabId: number): Promise<void> {
+  // Attempt to parse OpenGraph data from the tab content:
+  const result = await parse(tabId);
+
+  // Handle the result:
+  console.log(result);
+}
+
+export async function parse(tabId: number): Promise<ParseResult> {
   // Get the tab:
   const tab = await browser.tabs.get(tabId);

@@ -21,7 +30,7 @@ async function process(tabId: number) {

   // Return if we do not have a URL:
   if (!url) {
-    return;
+    return { status: "not-applicable" };
   }

   // Attempt to get the hostname:
@@ -29,14 +38,14 @@ async function process(tabId: number) {

   // Return if we do not have a hostname:
   if (!hostname) {
-    return;
+    return { status: "not-applicable" };
   }

   // Cool, we have a nice tab!
   console.log("Tab activated:", url, hostname);

   // Get the HTML content:
-  const [{ result }] = await browser.scripting.executeScript({
+  const [{ result: html }] = await browser.scripting.executeScript({
     target: { tabId },
     func: () => {
       return document.head.innerHTML;
@@ -44,17 +53,42 @@ async function process(tabId: number) {
   });

   // Attempt to parse the OpenGraph data:
-  ogs({ html: result })
+  const result = await ogs({ html })
     .then(({ error, result }) => {
       if (error) {
         console.error("Error while trying to extract OpenGraph data.");
-        return;
+        return { status: "error", ogdata: result } as ParseResultError;
       }

-      // We should have the OpenGraph data now:
-      console.log("OpenGraph data is extracted successfully.", result);
+      // Return with success:
+      return { status: "success", ogdata: result } as ParseResultSuccess;
     })
     .catch((error) => {
       console.error("Error while trying to extract OpenGraph data.", error);
+      return { status: "error" } as ParseResultError;
     });
+
+  // Return the result:
+  return result;
 }
+
+export type OgObject = SuccessResult["result"];
+
+export type ParseResult =
+  | ParseResultSuccess
+  | ParseResultError
+  | ParseResultNotApplicable;
+
+export type ParseResultSuccess = {
+  status: "success";
+  ogdata: OgObject;
+};
+
+export type ParseResultError = {
+  status: "error";
+  ogdata?: OgObject;
+};
+
+export type ParseResultNotApplicable = {
+  status: "not-applicable";
+};
```

## Changing the Extension Icon

Now that we have the OpenGraph information parsed from the Webpage, we can use
it to change the extension icon. We will use the `setIcon` browser action method
to change the icon based on the OpenGraph information.

As for icons, we will use the following icon from [unwing]:

![](icon.svg)

We will generate variations of this icon based on the OpenGraph information:

- **Default:** If the OpenGraph information is not available, we will use the
  default icon in blue.
- **Success:** If the OpenGraph information is available, we will use the icon
  in green.
- **Error:** If there is an error during the parsing process, we will use the
  icon in red.

Let's generate the icons under `public/icon` directory. First, the default icon:

```sh
for size in "16" "32" "48" "96" "128"; do
  convert \
    -fill "#2563eb" \
    -colorize 100 \
    -background transparent \
    -resize "${size}x${size}" \
    https://uxwing.com/wp-content/themes/uxwing/download/arts-graphic-shapes/circle-center-icon.svg \
    "${size}.png"
done
```

Now, the success icon:

```sh
for size in "16" "32" "48" "96" "128"; do
  convert \
    -fill "#22c55e" \
    -colorize 100 \
    -background transparent \
    -resize "${size}x${size}" \
    https://uxwing.com/wp-content/themes/uxwing/download/arts-graphic-shapes/circle-center-icon.svg \
    "${size}_success.png"
done
```

Finally, the error icon:

```sh
for size in "16" "32" "48" "96" "128"; do
  convert \
    -fill "#dc2626" \
    -colorize 100 \
    -background transparent \
    -resize "${size}x${size}" \
    https://uxwing.com/wp-content/themes/uxwing/download/arts-graphic-shapes/circle-center-icon.svg \
    "${size}_error.png"
done
```

We are ready to change the icon based on the OpenGraph information. The diff of
our small touch to the `background.ts` module is as follows:

```diff
diff --git a/entrypoints/background.ts b/entrypoints/background.ts
index 3cb3422..5c17785 100644
--- a/entrypoints/background.ts
+++ b/entrypoints/background.ts
@@ -14,11 +14,15 @@ export default defineBackground(() => {
 });

 export async function process(tabId: number): Promise<void> {
+  // Reset the icon:
+  setIcon();
+
   // Attempt to parse OpenGraph data from the tab content:
   const result = await parse(tabId);

   // Handle the result:
   console.log(result);
+  setIcon(result);
 }

 export async function parse(tabId: number): Promise<ParseResult> {
@@ -92,3 +96,39 @@ export type ParseResultError = {
 export type ParseResultNotApplicable = {
   status: "not-applicable";
 };
+
+export const ICONS = {
+  default: {
+    16: "icon/16.png",
+    32: "icon/32.png",
+    48: "icon/48.png",
+    96: "icon/96.png",
+    128: "icon/128.png",
+  },
+  success: {
+    16: "icon/16_success.png",
+    32: "icon/32_success.png",
+    48: "icon/48_success.png",
+    96: "icon/96_success.png",
+    128: "icon/128_success.png",
+  },
+  error: {
+    16: "icon/16_error.png",
+    32: "icon/32_error.png",
+    48: "icon/48_error.png",
+    96: "icon/96_error.png",
+    128: "icon/128_error.png",
+  },
+  "not-applicable": {
+    16: "icon/16.png",
+    32: "icon/32.png",
+    48: "icon/48.png",
+    96: "icon/96.png",
+    128: "icon/128.png",
+  },
+};
+
+export async function setIcon(result?: ParseResult) {
+  const path = ICONS[result?.status ?? "default"];
+  (browser.action ?? browser.browserAction).setIcon({ path });
+}
```

What did we do here?

1. We reset the icon before parsing the OpenGraph information.
2. We set the icon based on the OpenGraph information after parsing it.

How do we set the icon?

1. We declared a constant `ICONS` that maps the OpenGraph information status to
   the icon paths with a default icon set.
2. We declared a function `setIcon` that sets the icon based on the OpenGraph
   information with a default icon set if the information is not provided.

The icon set means a table of icon paths for different sizes.

Note the use of `browser.action` and `browser.browserAction`. It seems that Web
extensions may have different APIs between browsers and even manifest versions.
This way, we can use the `browserAction` API if the `action` API is not
available.

## Wrap Up

In this part, we refactored the code by encoding the parse results in a better
type. We also changed the extension icon based on the OpenGraph information
parsed from the Webpage on the active tab.

In the next part, we will attempt to show the OpenGraph information in the
popup.

<!-- REFERENCES -->

[unwing]: https://uxwing.com
[ogpatrol]: https://github.com/vst/ogpatrol
