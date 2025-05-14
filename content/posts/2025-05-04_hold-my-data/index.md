---
title: "Hold My Data"
date: 2025-05-04 20:20:29
description: |
  How do we manage data? Why is it important?
taxonomies:
  tags:
    - Technical Notes
    - Data
    - Infrastructure
---

This post is about data snapshots, backups and archives, why we create and keep
them, and how this entire exercise can be seen from top-down and bottom-up in a
business setting.

<!--more-->

What will follow is not a practical guide. It will start as philosophical and
psychological musings. Then, I will try to establish a glossary for reference.
Finally, I will offer a conceptual framework to assess the value in a business
setting.

## Personal Motivation

I like to hoard and organize.

Sometimes, I joke that my calling is to fight entropy. You will not find anyone
around me who appreciates the profession of a librarian or an accountant more
than I do.

Librarians enumerate knowledge, and accountants sort numbers. Their methods are
results of centuries of hard work and experience. In essence, they create and
maintain information structures, which in return enable their users to navigate
to create further knowledge and value.

On the other hand, unlike what librarians and accountants have, our methods are
in their infancy. Industrial revolution is a toddler compared to the history of
human knowledge. Information revolution is just a newborn. Figuring the right
methods and creating the right structures when programming or architecting a
software system are still big challenges. Failing to do so leads to tears as our
projects grow, if they survive at all.

### Psychological Aspect

I keep lots of data. All kinds of data! When not enough, my programs generate
even more data for me to keep, such as logs, metrics, ephemeral files, and so
on. I worry about losing them. Deleting data is not in my vocabulary. It makes
me very nervous.

But I know that hoarding has limits and comes with a price tag. Organizing kept
data is one challenge, forgetting it rightfully is another.

I do not know if my obsession is due to a neuro-divergent condition, a trauma or
purely cultural reasons which I am not aware of. I just need peace.

### Philosophical Aspect

Occasionally, I amuse myself asking GPT seemingly incoherent questions. I asked
it recently, along the lines of:

> Why would data backups matter from a philosophical perspective?

Here are some aphorisms I got back:

> 1. 🧠 Memory and Identity
>
>    -- We are our stories—and in the digital age, our stories are stored in
>    bits.
>
> 2. ⏳ Time, Impermanence, and the Fight Against Entropy
>
>    -- Snapshots capture a moment; backups restore the past; archives defy the
>    forgetting of time.
>
> 3. 🧭 Responsibility and Stewardship
>
>    -- If you are a custodian of data, you are a custodian of trust.
>
> 4. ⚖️ Control and Agency
>
>    -- Backups are forgiveness systems. They let us fail without finality.
>
> 5. 🧍‍♂️ Mortality and Legacy
>
>    -- Archives are digital tombs or time capsules, holding pieces of us in
>    trust for the future.

It even suggested an introduction to my possible philosophical book (not that I
had such an idea :smiley:):

> In a world built on data, to lose it is not just a technical failure -- it’s
> an existential one.

However silly it may sound, I have real, practical reasons to obsess over these
topics. Indeed, I am practically working with data snapshots, backups, archives,
encryption, storage, and so on.

I named my demons, so I should name what I am dealing with.

## Concepts

Backup, snapshot, archive, storage, versioning, retention, history, audit trail,
replication, redundancy, recovery...

I wrote these terms at least once in the last few years into a business policy
or a technical document. I am sure readers of this post are familiar with them
as well, probably more than I am.

Below is a glossary that I compiled some time ago for both business context and
homelab situations. It helps me as a map to address problems and solutions.

First, I will list the terms which address the discrete nature of data retention
and recovery. Then, I will list the terms which address the continuous nature of
the subject matter.

### Discrete Terms

- **Snapshot**
  - _Definition:_ A point-in-time capture of data without a specific
    destination.
  - _Use Case:_ Fast rollback, short-term recovery, consistency check.
- **Backup**
  - _Definition:_ A copy of data stored independently to enable recovery.
  - _Use Case:_ Recover from data loss, corruption, or disasters.
- **Archive**
  - _Definition:_ Long-term, infrequently accessed, immutable and compressed
    copy of data, often encrypted.
  - _Use Case:_ Regulatory compliance, cold data retention.
- **Cold Storage**
  - _Definition:_ Infrequently accessed data storage, often cheaper and slower.
  - _Use Case:_ Long-term data retention, compliance.
- **Hot Storage**
  - _Definition:_ Frequently accessed data storage, typically faster and more
    expensive.
  - _Use Case:_ Active data, real-time applications.
- **Versioning**
  - _Definition:_ Keeping multiple historical versions of data or files.
  - _Use Case:_ Rollback, auditing, recovering from user errors.
- **Retention Policy**
  - _Definition:_ Rules defining how long data (backups, snapshots, etc.) is
    kept and when it's purged.
  - _Use Case:_ Compliance, cost control, automation.
- **Point-in-Time Recovery (PITR)**
  - _Definition:_ Recovering data or databases to a specific timestamp.
  - _Use Case:_ Granular database restoration.
- **Immutable Storage**
  - _Definition:_ Data that cannot be modified or deleted for a defined period.
  - _Use Case:_ Legal compliance, ransomware protection.

The relation between these terms is not always clear. I like to think of it as
follows:

Run a snapshot of my data on a schedule. Archive each of them in a _cold
storage_, and keep the last few as _backups_ in a _hot storage_ for a shorter
period of time for system recovery.

_Version_ each snapshot, and keep them for as long as my _retention policy_
requires.

Also, keep _timestamp_ along with the _version_ information so that a
_point-in-time_ recovery is possible upon request.

Finally, make sure that the both _cold storage_ and _hot storage_ are immutable
as per compliance requirements and retention policy.

### Continuous Terms

- **Replication**
  - _Definition:_ Real-time, lossless copy to another location/system.
  - _Use Case:_ High availability, seamless recovery.

Replication is possibly the most desired solution to the problem of data loss or
systems failure. Unlike the core idea in the previous section, this is not a
static, scheduled operation, but rather a dynamic, continuous one.

However, replication is usually costly and complex. It requires a lot of
resources, and it is not always practical to implement in a way that guarantees
data consistency.

Nevertheless, we may still want to, or even need to, run discrete data snapshots
and keep them, even if we have a replication solution in place.

## Value Framework

As a business owner, I am interested in both risk management and value
proposition. In other words, I see my business a synthesis of _top-down_ forces
and _bottom-up_ levers.

Admittedly, this statement sounds too abstract. What do I mean by that?

### Top-Down Forces

Here are some terms that compliance officers want to hear about:

- **Service Level Agreement (SLA):** A formal agreement between a service
  provider and service client that defines offered and expected service levels
  including uptime, RTO, and RPO.
- **Business Continuity Plan (BCP):** A playbook to ensure operation during
  disruptions to usual business processes.
- **Disaster Recovery Plan (DRP):** A plan for recovering IT infrastructure,
  services and data after a significant disruption or outage.

How are these operationalized in practice? Consider the following terms often
used in the context of SLAs, BCPs and DRPs:

- **Recovery Time Objective (RTO):** The maximum acceptable time to restore a
  service after a disruption.
- **Recovery Point Objective (RPO):** The maximum acceptable amount of data loss
  measured in time, such as "1 hour of data loss."
- **Time to Recovery (TTR):** The total actual time taken to recover from a
  disruption.

These three metrics often drive the design of backup and recovery solutions.
They are not options, but rather imperatives.

### Bottom-Up Levers

This section will be a bit informal, but I will try to give you an idea of what
I mean by _bottom-up_ levers.

First of all, I do not like to think of data retention and recovery as exogenous
concerns. I prefer thinking of them as a part of actual development and
operations processes.

For example, I use NixOS both at home and at work. If I launch a new service on
a NixOS server, I am defining a custom NixOS module for it. If the service is
stateful, such as depending on a PostgreSQL database or a directory of data and
state artifacts (like `/var/lib/abc`), the same module will have a section for
deploying the snapshot and encrypted archive solution. The two example scenarios
which commonly come up in my work are:

1. Dump PostgreSQL database, compress (`gzip`) and encrypt (`gpg`) it, and
   upload to one or more remote S3-compatible storage ([rclone]).
2. Run [restic] to backup an entire filesystem or a subset of it, and upload to
   a remote S3-compatible storage.

Secondly, the particular solution architecture and implementation also influence
the data retention and recovery process.

For example, answering audit trail queries and compliance inquiries becomes much
easier if versioning is built into the system. However, a typical CRUD
application with a relational database cannot be easily versioned, but CQRS and
Event Sourcing can be considered as a potential solution.

Similarly, I argue that higher normal form database designs can help with
eliminating data redundancy and improving data integrity. But this property
makes it harder to use ORMs as it favours _relational algebraic approach_ over
_object oriented approach_ which are fundamentally incompatible. On the other
hand, statically typed functional programming languages such as Haskell are
better suited to normalized data modeling.

Finally, I see avoiding "feature-happy" approach to software development also as
a bottom-up lever. I am not saying that we should not deliver new functions and
features, but if adding a small feature requires deploying a completely new
stateful service, then we should think twice about it.

When bottom-up levers are in place and in good shape, the top-down forces become
easier to satisfy. For example, if the system is designed to be stateless, the
RTO and TTR are much smaller, and the RPO is just zero, because there is no
state, hence no data loss! Similarly, if the system is backed by event sourcing,
the RTO and TTR might be a little higher, but again, the RPO might be minimal.
These all depend on the particular problem and solution, but a team of skilled
engineers should be able to reason about such trade-offs.

## Conclusion

I keep refining these ideas, but my core point is that data and its management
should be part of initial problem analysis, and then, the system design.
Treating them as separate concerns is a recipe for suboptimal or even
ineffective solutions.

<!-- REFERENCES -->

[restic]: https://restic.net
[rclone]: https://rclone.org
