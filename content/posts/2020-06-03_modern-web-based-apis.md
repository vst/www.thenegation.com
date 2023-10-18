---
title: "Modern Web-based APIs: What to Expect?"
date: 2020-06-03
updated: 2023-10-18 14:42:13
taxonomies:
  tags:
    - computing
---

Here are some quick notes for mysels on what to expect from a modern, Web-based
API that facilitates data query and manipulation (and possibly remote procedure
calls).

<!-- more -->

1. **High-Level and Accessible:** Data should be made available via an API that
   (1) does not require any proprietary or idiosyncratic technology, custom
   query language and run-time environment, and (2) imposes a particular
   language at the call-site. In other words, users should be able to issue
   queries using a standard query language over an open communication protocol
   on a free run-time environment, all with which they are already familiar
   with.
2. **Liberal:** Consumers should be able to issue read/write queries which may
   involve filtering, searching, sorting, composing and lensing without
   limitations, even in the existence of deeply relational data models, as long
   as they are authorised to read/write data of interest.
3. **Performant:** The evaluation performance of a query should be
   proportionally related to the essential complexity of the data model.
   Furthermore, the base performance should be good enough to allow clients to
   issue queries without worrying much about the performance imposed by the
   underlying technology, notwithstanding the need for query optimisations at
   the query-side in some cases.
4. **Authorised:** All data should be authorised at the row and column levels.
   Such authorisation machinery should be a built-in feature of the API to avoid
   a separate realm of encoding such authorisation rules. Once a query is issued
   and it starts being interpreted, it should be authorised without the need for
   a procedural intervention. Authorisation should be embedded in the query
   interpreter (ideally backed by relational algebra capabilities of the RDBMS)
   as part of the evaluation process.
