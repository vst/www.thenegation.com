---
title: "Web Browser Extension Workshop - Part 4"
date: 2024-08-27 21:53:14
description:
  Populating a database with the OpenGraph information parsed from the Webpage
  on active tabs.
taxonomies:
  tags:
    - Technical Note
    - Web Development
    - Hacking
    - Computing
---

This is the fourth part of the Web Browser Extension Workshop series. In this
part, we will populate a database with the OpenGraph information parsed from the
Webpage on active tabs. For this, we will implement a service that we can use
later to query the OpenGraph information as well.

<!--more-->

## Overview

In the previous blog post, I concluded by saying that we would show the
OpenGraph information to the user in the popup. However, I realized that we need
to refactor the code a bit before we can do that, and I do not want to rush
things up.

So in this part, we will implement a service that will populate a database with
the OpenGraph information parsed from the Webpage on the active tab. This
service will also provide a method to query the OpenGraph information and list
all the OpenGraph information in the database.

First, let me clarify one thing: Why do we need such a service? Why do we need a
database at all?

The information parsed from the Webpage is not persistent. It is in the
background process and there are not many ways to access it from the popup. We
could have used a messaging mechanism over content scripts, but it would be an
overkill for the job.

Using a database-backed service, we can store the information in the database
and query it whenever we need it and from wherever we need it, most importantly
from the popup.

## Refactoring

We will use the `@/utils` directory to store the utility definitions, in
particular the database and service implementations. Both the database and the
service need to know our data types.

Let's create a new file `@/utils/types.ts` and move our data types there:

```diff
diff --git a/entrypoints/background.ts b/entrypoints/background.ts
index 0fbfeb9..5c17785 100644
--- a/entrypoints/background.ts
+++ b/entrypoints/background.ts
@@ -1,9 +1,5 @@
-import {
-  ParseResult,
-  ParseResultError,
-  ParseResultSuccess,
-} from "@/utils/types";
 import ogs from "open-graph-scraper-lite";
+import type { SuccessResult } from "open-graph-scraper-lite";

 export default defineBackground(() => {
   browser.tabs.onActivated.addListener(({ tabId }) => {
@@ -80,6 +76,27 @@ export async function parse(tabId: number): Promise<ParseResult> {
   return result;
 }

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
+
 export const ICONS = {
   default: {
     16: "icon/16.png",
diff --git a/utils/types.ts b/utils/types.ts
deleted file mode 100644
index 2f6ffb7..0000000
--- a/utils/types.ts
+++ /dev/null
@@ -1,23 +0,0 @@
-import type { SuccessResult } from "open-graph-scraper-lite";
-
-export type OgObject = SuccessResult["result"];
-
-export type ParseResult =
-  | ParseResultSuccess
-  | ParseResultError
-  | ParseResultNotApplicable;
-
-export type ParseResultSuccess = {
-  status: "success";
-  ogdata: OgObject;
-};
-
-export type ParseResultError = {
-  status: "error";
-  ogdata?: OgObject;
-};
-
-export type ParseResultNotApplicable = {
-  status: "not-applicable";
-};
-
```

## Database and Service

The next step will be the implementation of the database under
`@/utils/database.ts`:

```typescript
import { DBSchema, IDBPDatabase, openDB } from "idb";

interface ExtensionDatabaseSchema extends DBSchema {
  ogs: {
    key: string;
    value: ParseResultSuccess;
  };
}

export type ExtensionDatabase = IDBPDatabase<ExtensionDatabaseSchema>;

export function openExtensionDatabase(): Promise<ExtensionDatabase> {
  return openDB<ExtensionDatabaseSchema>("ogpatrol-service", 1, {
    upgrade(database) {
      database.createObjectStore("ogs", { keyPath: "source" });
    },
  });
}
```

As you may have noticed, we are using the `idb` library to interact with the
`IndexedDB` database. The database schema is simple: we have a single object
store `ogs` that stores the successfully parsed OpenGraph information
(`ParseResultSuccess`) with the URL (`source`) as the key.

Our record, however, does not have a `source` field yet. We will add it to the
record type:

```diff
diff --git a/utils/types.ts b/utils/types.ts
index 2f6ffb7..a0eda50 100644
--- a/utils/types.ts
+++ b/utils/types.ts
@@ -9,6 +9,7 @@ export type ParseResult =

 export type ParseResultSuccess = {
   status: "success";
+  source: string;
   ogdata: OgObject;
 };
```

Now we can implement the service that will populate the database with the
OpenGraph information:

```typescript
import { defineProxyService } from "@webext-core/proxy-service";
import type { ExtensionDatabase } from "./database";

export interface Service {
  getAll(): Promise<ParseResultSuccess[]>;
  find(key: string): Promise<ParseResultSuccess | undefined>;
  upsert(record: ParseResultSuccess): Promise<void>;
}

function createService(_db: Promise<ExtensionDatabase>): Service {
  return {
    async getAll() {
      const db = await _db;
      return await db.getAll("ogs");
    },

    async find(key) {
      const db = await _db;
      return await db.get("ogs", key);
    },

    async upsert(record) {
      const db = await _db;
      await db.put("ogs", record);
    },
  };
}

export const [registerService, getService] = defineProxyService(
  "ogpatrol-service",
  createService,
);
```

The service is simple, too: it provides three methods to get all the records, to
find a record by key, and to upsert a record. Note that we are wrapping our
service with `@webext-core/proxy-service` that allows us to call service methods
anywhere but execute them in the background process.

## Using the Service

Finally, we need to register and use the service in the background process:

```diff
diff --git a/entrypoints/background.ts b/entrypoints/background.ts
index 0fbfeb9..5e4ace4 100644
--- a/entrypoints/background.ts
+++ b/entrypoints/background.ts
@@ -1,3 +1,4 @@
+import { registerService } from "@/utils/service";
 import {
   ParseResult,
   ParseResultError,
@@ -6,18 +7,24 @@ import {
 import ogs from "open-graph-scraper-lite";

 export default defineBackground(() => {
+  // Open extension database:
+  const db = openExtensionDatabase();
+
+  // Register our service:
+  const service = registerService(db);
+
   browser.tabs.onActivated.addListener(({ tabId }) => {
-    process(tabId);
+    process(service, tabId);
   });

   browser.tabs.onUpdated.addListener((tabId, changeInfo) => {
     if (changeInfo?.status === "complete") {
-      process(tabId);
+      process(service, tabId);
     }
   });
 });

-export async function process(tabId: number): Promise<void> {
+export async function process(service: Service, tabId: number): Promise<void> {
   // Reset the icon:
   setIcon();

@@ -25,8 +32,12 @@ export async function process(tabId: number): Promise<void> {
   const result = await parse(tabId);

   // Handle the result:
-  console.log(result);
   setIcon(result);
+
+  // Store the result in the database:
+  if (result.status === "success") {
+    service.upsert(result);
+  }
 }

 export async function parse(tabId: number): Promise<ParseResult> {
@@ -69,7 +80,11 @@ export async function parse(tabId: number): Promise<ParseResult> {
       }

       // Return with success:
-      return { status: "success", ogdata: result } as ParseResultSuccess;
+      return {
+        status: "success",
+        source: url,
+        ogdata: result,
+      } as ParseResultSuccess;
     })
     .catch((error) => {
       console.error("Error while trying to extract OpenGraph data.", error);
```

Changes are straightforward: we open the database and register the service in
the background process. We also pass the service to the `process` function and
store the parse result in the database if it is successful. Note how we inject
the URL in to the record for successful parse results.

## Wrap Up

In this part, we implemented a service that populates a database with the
successfully parsed OpenGraph information for each URL we visit. We also
provided a method to query the OpenGraph information and list all the OpenGraph
information in the database.

With these changes, we have completed the refactoring of the codebase. This
time, we are really really really ready to show the OpenGraph information to the
user in the popup. In the next part, we will see how.

<!-- REFERENCES -->
