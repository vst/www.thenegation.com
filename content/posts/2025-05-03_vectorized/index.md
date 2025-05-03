---
title: "Vectorized Data Pipelines"
date: 2025-05-03 23:04:29
description: |
  Learn how to build a simple, declarative data pipeline using Vector to ingest, transform, and store webhook events—without writing a single line of custom code.
taxonomies:
  tags:
    - Technical Notes
    - Infrastructure
---

This post shows how to use Vector to capture and persist webhook events -- like
those from SendGrid -- into a PostgreSQL database with minimal setup.

<!-- more -->

Due to my profession, I work with large amounts of data, logs and metrics.
Collecting them is one thing, processing and analyzing them is another. My
approach is usually to employ an existing tool to collect and perform initial
processing for such data. Luckily, there are more tools available now than when
I started my career.

I prefer to use tools which emphasize simplicity, composability, and most
importantly, a declarative approach to configuration and deployment. These
become even more important when working in a team setting.

## Criteria to Choose a Data Tool

Data analysis is a broad field where tools are usually chosen based on the
problem at hand and the methodology applied. In my personal opinion, however,
how data is collected and staged can be done based on the data frequency, volume
and shape, instead of the meaning of the data. Therefore, I prefer to avoid
idiosyncratic, special purpose tools when possible. In this regard, my criteria
for choosing a data tool are as follows:

1. **Simplicity:** The tool should be simple to understand, deploy and
   configure. Obviously, no tool is advertised as being complicated. My
   definition of simplicity is to be able to understand its operational model.
   My quick checklist is:

   - Is it complicated, simple or simplistic?
   - Do I understand how it works?
   - What is its dependency footprint?
   - Can I use it both at work and at home?

2. **Composability:** Being used to functional programming and Unix philosophy,
   I expect tools to be able to compose together to solve a problem. This is
   especially fruitful when refactoring certain parts of the pipeline: You can
   replace one tool with another without having to change the whole pipeline.
   Following questions help me to evaluate composability:

   - Is it stateful or stateless?
   - Does it offer clear interfaces?
   - Is it a monolith or a collection of small, independent tools?
   - Does it use ubiquitous data formats and protocols?

3. **Declarative Approach:** Imperative or procedural approach to deployment and
   configuration is a recipe for disaster in long run. I prefer to use
   declarative tools which allow introspection and validation. Such approach
   pretty much facilitates composability, too. Some things to consider:

   - Does it have a declarative configuration?
   - Does the language have a type system?
   - Is it an expression or statement based language?
   - Can I validate the configuration?
   - Can I introspect the configuration?
   - Is the configuration human-readable?
   - Does it have a good documentation?

## A Minimal-Maintenance Tool for Data Pipelines

For a long time, I was looking for a tool that would fit my criteria above. I
understand and appreciate that I can not use the same tool for everything. What
I was looking for was something that could solve a relatively large class of
practical problems.

I kept an eye on both [Fluent Bit] and [Vector] for a while. I decided to pick one
to test in production, in particular to:

1. Capture and land data from various sources, and
1. Notify me (and my team) when something goes wrong or seems fishy.

The takeoff with Fluent Bit was a bit bumpy. Without spending too much time on
it, I quickly pivoted to Vector. Once I figured it out and convinced myself that
it passes my criteria, I decided to use it in production.

Let's see how to use it in practice.

## Getting Started with Vector

Vector advertises itself as _a lightweight, ultra-fast tool for building
observability pipelines_. It is developed by [Datadog] who are known for their
cloud-based observability platform. It is open-source, developed in Rust, and
has quite a good community and documentation.

What made me finally stick to Vector in production is as follows:

1. It has a very clear execution model.
1. It uses a declarative approach to configuration and processing:
   - One or more configuration files of JSON, YAML or TOML format, and
   - Vector Remap Language (VRL) for data transformation.
1. It has both a Nix package `nixpkgs.vector` and a NixOS module: So
   *double*down on declarative configuration!
1. It does not need any other services to run, such as a configuration or
   database service.

Let's prepare a Vector configuration file first. We need a source and a sink:

1. **Source:** Let Vector listen for incoming HTTP requests and consume some
   JSON data.
2. **Sink:** Let Vector print the data to the console.

```sh
cat <<EOF > $PWD/vector_simple.yaml
sources:
  webhook:
    type: http_server
    address: 127.0.0.1:40002
    method: POST
    path: /
    strict_path: true
    encoding: json
    response_code: 200
    log_namespace: true

sinks:
  console:
    inputs:
      - webhook
    target: stdout
    type: console
    encoding:
      codec: json
EOF
```

In plain English, this means that Vector will listen for incoming HTTP POST
requests on port `40002` strictly on the root path `/`. The incoming data will
be parsed as JSON and keep the payload clean without injecting any extra data
(`log_namespace: false`). And then, the sink part will print the data from this
`input` to the console in JSON format.

Now, let's run Vector:

```sh
docker run --rm \
  --network host \
  --volume $PWD/vector_simple.yaml:/etc/vector/vector.yaml:ro \
  --name my-vector \
  timberio/vector:0.46.1-debian
```

And this means that we want a temporary container that will run Vector in the
host network with our configuration file mounted as read-only.

Let's test it:

```sh
curl http://localhost:40002 \
  -d '{"title": "Hello World!", "message": "Nice to see you here!"}'
```

You should see the following output on the console of the Vector container:

```json
{ "title": "Hello World!", "message": "Nice to see you here!" }
```

You can now close the Vector container. We will launch it again later with
another configuration file.

## Real-World Example: Capturing and Persisting SendGrid Webhook Events

I am using [SendGrid] to send transactional emails. It is a little pricey, but
the deliverability is great. It is important for me and my team to maintain good
_reputation_ on both our domain and IP address. This means that we need to
monitor the email delivery and engagement metrics.

However, there are two drawbacks with SendGrid:

1. Logs are not retained for a period of time longer than a month.
2. The Web interface is very slow, feels clunky and is not very responsive.

Luckily, SendGrid offers Webhooks to send events to a URL of our choice. So, we
will help ourselves to capture and persist these events in a database for longer
retention and better querying (and visualize in Grafana, for example).

Now, we have lots of plausible options: Implementing a small service to capture
these events and persist them, using our favourite programming language, and
deploying it on a server we manage, or using a serverless function.

But, I do not want to write any code for this. I do not want any of my team
members do it either. And more importantly, I do not want my team to maintain
it!

Let's see how we can use Vector to capture these events and persist them in a
PostgreSQL database. But we will do it in a way that does not require us to know
anything about the actual SendGrid Webhook event structure! So, this is how our
database table will look like:

```sql
CREATE TABLE "sendgrid_raw_event" (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    timestamp TIMESTAMPTZ NOT NULL,
    account TEXT NOT NULL,
    payload JSONB NOT NULL,
    metadata JSONB NOT NULL
);

CREATE INDEX ON "sendgrid_raw_event" ("timestamp");
CREATE INDEX ON "sendgrid_raw_event" ("account");
CREATE INDEX ON "sendgrid_raw_event" USING GIN ("payload" jsonb_ops);
CREATE INDEX ON "sendgrid_raw_event" USING GIN ("metadata" jsonb_ops);
```

Here is what each column means:

1. `id`: A unique identifier for the event.
2. `timestamp`: The time when the event was received by us.
3. `account`: The SendGrid account that sent the event. If we have multiple
   SendGrid accounts, we can still use the same table to store the events.
4. `payload`: The actual event data.
5. `metadata`: The metadata about the event, in particular, Vector metadata.

We index the `timestamp` and `account` columns to query the data, along with the
`payload` and `metadata` columns. The latter two are indexed with `GIN` because
they are JSONB columns. This means that we can query the data using the JSONB
operators and functions.

What does the Vector source configuration look like? It is similar to the
previous example, an HTTP server that listens for incoming requests:

```yaml
sources:
  src_sendgrid:
    type: http_server
    address: 127.0.0.1:40002
    method: POST
    path: /
    strict_path: false
    encoding: json
    response_code: 200
    log_namespace: true
```

Note that the only difference is that we set `strict_path: false` to allow
SendGrid to send events to any path. This is important because we will use
different paths for different SendGrid accounts.

Let's add the transformation part:

```yaml
sources:
  src_sendgrid:
    type: http_server
    address: 127.0.0.1:40002
    method: POST
    path: /
    strict_path: false
    encoding: json
    response_code: 200
    log_namespace: true

transforms:
  trn_sendgrid:
    type: remap
    inputs: ["src_sendgrid"]
    source: |
      payload = .
      . = {
            "id": uuid_v4(),
            "timestamp": %vector.ingest_timestamp,
            "account": %http_server.path,
            "payload": payload,
            "metadata": %
          }
```

We used the `remap` transform to transform the incoming data. The source is in
[Vector Remap Language (VRL)](VRL) which is a simple and powerful language for
transforming data. There is a [playground][VRL-playground] to test your VRL
expressions.

In essence, we get the payload from `.` (the incoming data) and then we assign
it to the symbol `payload`. Then, we assign the new data structure to `.`. This
also means that this is the result of our transformation.

Like `.`, `%` is a special symbol. It refers to the Vector event metadata:

1. `%vector` refers to the common Vector metadata regardless of the source.
2. `%http_server` refers to the HTTP server source specific Vector metadata,
   such as the request headers, path, method, etc. This is source specific, and
   the name will change to the source type if you use a different source.

The rest is quite similar to JSON object creation:

1. We create a new UUID with the built-in VRL function `uuid_v4()`, and set it
   to the `id` field.
2. We assign the Vector's ingestion timestamp to the `timestamp` field. This is
   important because it is the time when the event was received by Vector, and
   not when it was created/sent by SendGrid.
3. We assign the HTTP request path to the `account` field.
4. We assign the incoming event payload to the `payload` field, and
5. We assign the Vector event metadata (both common and source specific) to the
   `metadata` field.

Honestly, despite my long explanation, this is quite simple and straightforward.
There is no magic here.

Next, we need to add the sink part to persist the data in PostgreSQL:

```yaml
sources:
  src_sendgrid:
    type: http_server
    address: 127.0.0.1:40002
    method: POST
    path: /
    strict_path: false
    encoding: json
    response_code: 200
    log_namespace: true

transforms:
  trn_sendgrid:
    type: remap
    inputs: ["src_sendgrid"]
    source: |
      payload = .
      . = {
            "id": uuid_v4(),
            "timestamp": %vector.ingest_timestamp,
            "account": %http_server.path,
            "payload": payload,
            "metadata": %
          }

sinks:
  snk_sendgrid:
    type: postgres
    inputs: ["trn_sendgrid"]
    endpoint: "postgresql://SECRET[vector_postgresql.username]:SECRET[vector_postgresql.password]@localhost:5432/vector"
    table: "sendgrid_raw_event"

  console:
    inputs:
      - src_sendgrid
      - trn_sendgrid
    target: stdout
    type: console
    encoding:
      codec: json
```

It reads quite well, right? We use the `postgres` sink to persist the data in
PostgreSQL. We use the `trn_sendgrid` transform as the input, and we specify the
PostgreSQL connection string and the table name. The connection string is
actually a typical PostgreSQL connection URI, but we use the `SECRET[]` syntax
to inject the username and password from the secret store.

Note that we added a second sink to print the data to the console. This is
useful for debugging and testing. We can remove it later when we are done. Also
note that we use both the source and the transform as inputs to the console
sink. This means that we will print the incoming data and the transformed data
to the console. This is useful to see what is going on.

Finally, we add the secret store as another section in the configuration file,
which you may want to keep at the top of the file:

```yaml
secret:
  vector_postgresql:
    type: file
    path: /run/secrets/vector_postgresql.json

sources:
  src_sendgrid:
    type: http_server
    address: 127.0.0.1:40002
    method: POST
    path: /
    strict_path: false
    encoding: json
    response_code: 200
    log_namespace: true

transforms:
  trn_sendgrid:
    type: remap
    inputs: ["src_sendgrid"]
    source: |
      payload = .
      . = {
            "id": uuid_v4(),
            "timestamp": %vector.ingest_timestamp,
            "account": %http_server.path,
            "payload": payload,
            "metadata": %
          }

sinks:
  snk_sendgrid:
    type: postgres
    inputs: ["trn_sendgrid"]
    endpoint: "postgresql://SECRET[vector_postgresql.username]:SECRET[vector_postgresql.password]@localhost:5432/vector"
    table: "sendgrid_raw_event"

  console:
    inputs:
      - src_sendgrid
      - trn_sendgrid
    target: stdout
    type: console
    encoding:
      codec: json
```

The contents of the `/run/secrets/vector_postgresql.json` file should look like:

```json
{
  "username": "myusername",
  "password": "mypassword"
}
```

Now, we can run Vector with this configuration file, namely
`vector_sendgrid.yaml`:

```sh
docker run --rm \
  --network host \
  --volume $PWD/vector_sendgrid.yaml:/etc/vector/vector.yaml:ro \
  --name my-vector \
  timberio/vector:0.46.1-debian
```

And we can test it with the following command, sending a sample SendGrid event
payload to the Vector HTTP server:

```sh
curl http://localhost:40002 \
  -H 'Content-Type: application/json' \
  -d '{
    "email": "hebele@example.com",
    "timestamp": 1683033600,
    "event": "processed",
    "category": "my-category",
    "sg_event_id": "SG_EVENT_ID",
    "sg_message_id": "SG_MESSAGE_ID",
    "sg_template_id": "SG_TEMPLATE_ID",
    "smtp-id": "SMTP_ID"
  }'
```

To go to production:

1. Make sure that the PostgreSQL database is up and running.
2. Put a reverse proxy in front of Vector to handle HTTPS requests (configuring
   Vector HTTP Server TLS is not worth the effort).
3. Configure SendGrid to send events to the reverse proxy URL.

## Conclusion

In this post, we have seen how to use Vector to capture and persist SendGrid
events in a PostgreSQL database.

The way I am deploying Vector is over NixOS, using a wrapper around the provided
NixOS module. This means that I gain more declarative power, such as NixOS
`nginx` module for reverse proxy configuration, and [opsops] for managing Vector
secret stores.

<!-- REFERENCES -->

[Vector]: https://vector.dev
[Fluent Bit]: https://fluentbit.io
[Datadog]: https://www.datadoghq.com
[SendGrid]: https://sendgrid.com
[VRL]: https://vector.dev/docs/reference/vrl/
[VRL-playground]: https://playground.vrl.dev/
[opsops]: https://github.com/vst/opsops
