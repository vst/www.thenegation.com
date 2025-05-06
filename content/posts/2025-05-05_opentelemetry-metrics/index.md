---
title: "Working with OpenTelemetry Metrics"
date: 2025-05-05 21:17:22
description: |
  Why I switched to OpenTelemetry for metrics ingestion and how to get started.
taxonomies:
  tags:
    - Technical Notes
    - Infrastructure
    - Observability
---

I have started adopting [OpenTelemetry] in my workshop to unify metrics, logs,
and traces. This post explains why --and how-- I took the first step.

<!-- more -->

My main job definition is to design and implement computer programs. However, a
significant part of my work involves system administration and DevOps
activities.

Over the years, I have developed a few strategies to help myself stay focused on
my primary job while also attending to the deployment, monitoring and
maintenance of the systems I work with.

One of these strategies is to avoid idiosyncratic solutions. I ask myself the
following two questions:

1. Is this solution ubiquitous?
2. Can I use this solution both at work and at home?

The first question is important because I expect the solution to have multiple
uses in my workshop. The second question is just a heuristic to help me
determine if the solution has wider applicability.

This is why I switched to [Grafana] for visualisation and alerting after
deciding to drop [InfluxDB] from my stack. I have been seeing Grafana everywhere
for a very long time, and I could imagine myself using it in different settings,
such as at home, at work, or in my clients' own workshops.

Yet I struggled to find a good replacement for InfluxDB's data ingestion,
persistence and querying facilities. Its line protocol is very simple and easy
to use. Data is persisted in a time series database that never failed me. Once
in the database, data is available for querying right away.

On the other hand, Grafana’s way of doing things is to not worry about how data
is ingested and where it is stored. So I decided to pick the best tools for the
type of data I am working with.

For example, [Prometheus] --like Grafana-- is ubiquitous, making it a good
candidate for ingesting and storing the same kind of data I was using InfluxDB
for. There is [Loki] for logs, [Tempo] for traces and so on.

One thing still bothered me: the lack of a grand picture. Being used to working
with specifications, protocols and standards, I was not happy with this void.

Then I stumbled upon [OpenTelemetry]. It is a project that aims to provide a
unified way to collect, process and export telemetry data. From a hundred
thousand feet, it looks like they know what they are doing: standardized data
definitions and protocols, semantic conventions, etc. To me, it is like a
rulebook for telemetry data --something I find reassuring to rely on.

The most attractive feature of OpenTelemetry to me is that it is
vendor-agnostic. There are quite a few decent, free and open-source software
solutions which can consume and produce OpenTelemetry data. Even if I cannot
pick a vendor for both home and work use, I can still stick to OpenTelemetry and
use different vendors.

My OpenTelemetry adoption roadmap is to:

1. collect metrics as OpenTelemetry data and export them to Prometheus,
2. replace classic logging functions in my applications with OpenTelemetry
   traces and spans, and capture and store them as such, and
3. aggregate logs from different sources, map them to OpenTelemetry logs and
   export them to Loki.

The latter two are still in the works, but I have already figured out the first
one. Although my stack is slightly different, here is how you can test
collecting metrics as OpenTelemetry data and exporting them to Prometheus.

We will use Docker Compose:

```yaml
services:
  otel-collector:
    image: ghcr.io/open-telemetry/opentelemetry-collector-releases/opentelemetry-collector-contrib:0.125.0
    ports:
      - 127.0.0.1:4317:4317 # OTLP gRPC receiver
      - 127.0.0.1:4318:4318 # OTLP http receiver
    volumes:
      - ./otel-collector-config.yaml:/etc/otelcol-contrib/config.yaml:ro

  prometheus:
    image: prom/prometheus:v3.3.1
    volumes:
      - ./prometheus.yaml:/etc/prometheus/prometheus.yml:ro
    ports:
      - "127.0.0.1:9090:9090" # Prometheus UI
```

In this setup, we are launching the [OpenTelemetry Collector] with the configuration
file `otel-collector-config.yaml` and exposing its gRPC and HTTP receiver ports on
localhost. Also, we are launching [Prometheus] with the configuration file `prometheus.yaml`
and exposing its UI on localhost.

The configuration file `otel-collector-config.yaml` for the OpenTelemetry
Collector is as follows:

```yaml
receivers:
  otlp:
    protocols:
      grpc:
        endpoint: 0.0.0.0:4317
      http:
        endpoint: 0.0.0.0:4318

exporters:
  prometheus:
    endpoint: 0.0.0.0:8889
  debug:
    verbosity: detailed

service:
  pipelines:
    metrics:
      receivers: [otlp]
      exporters: [prometheus, debug]
```

This configuration tells the OpenTelemetry Collector to listen for incoming
metrics data on the gRPC and HTTP ports (`4317` and `4318`, respectively) and to
export the data to Prometheus on port `8889`. The `debug` exporter is used to
log the received data to the console for debugging purposes. The `service`
section is where we connect the receivers and exporters to the _metrics_
pipeline.

The configuration file `prometheus.yaml` for Prometheus is as follows:

```yaml
global:
  scrape_interval: 10s

scrape_configs:
  - job_name: "otel-collector"
    static_configs:
      - targets:
          - otel-collector:8889
```

This simple configuration tells Prometheus to scrape metrics from the
OpenTelemetry Collector on port `8889` every 10 seconds.

Once the Docker Compose stack is up and running, you can send metrics to the
OpenTelemetry Collector using gRPC or HTTP. This is probably the trickiest part
to start with: even a simple metric is represented by a mouthful of JSON. There
is no CLI client to send metrics to the OpenTelemetry Collector, either.
Therefore, I suggest trying [Telegraf] to send actual metrics to the
OpenTelemetry Collector. You can use the following sample configuration file
`telegraf.toml`:

```toml
[global_tags]

[agent]
  interval = "10s"
  round_interval = true
  metric_batch_size = 1000
  metric_buffer_limit = 10000
  collection_jitter = "0s"
  flush_interval = "10s"
  flush_jitter = "0s"
  precision = "0s"

[[inputs.cpu]]
  percpu = true
  totalcpu = true
  collect_cpu_time = false
  report_active = false
  core_tags = false

[[inputs.disk]]
  ignore_fs = ["tmpfs", "devtmpfs", "devfs", "iso9660", "overlay", "aufs", "squashfs"]

[[inputs.mem]]

[[outputs.opentelemetry]]
```

... and run it with:

```sh
telegraf --config telegraf.conf
```

If you are using Nix, you can use the following command to run Telegraf:

```sh
nix-shell --packages telegraf --command "telegraf --config telegraf.toml"
```

Telegraf will collect CPU, memory and disk usage metrics and send them to the
OpenTelemetry Collector using the OpenTelemetry protocol. Then, you can check
your Prometheus UI at [http://localhost:9090](http://localhost:9090) and see the
metrics being scraped from the OpenTelemetry Collector. For example, you can see
the [memory usage] metrics or the [disk usage] metrics.

---

Adopting OpenTelemetry has helped me take a more portable approach to
observability in my workshop. Starting with metrics gave me a practical entry
point, and tools like Telegraf, OpenTelemetry Collector and Prometheus made
integration straightforward. While I am excited to tinker with traces and logs,
there’s still more work to be done on the metrics side.

<!-- REFERENCES -->

[Grafana]: https://grafana.com/oss/grafana/
[InfluxDB]: https://www.influxdata.com/products/influxdb/
[Loki]: https://grafana.com/oss/loki/
[OpenTelemetry Collector]: https://opentelemetry.io/docs/collector/
[OpenTelemetry]: https://opentelemetry.io/
[Prometheus]: https://prometheus.io/
[Telegraf]: https://www.influxdata.com/time-series-platform/telegraf/
[Tempo]: https://grafana.com/oss/tempo/
[memory usage]:
  http://localhost:9090/query?g0.expr=mem_used&g0.show_tree=0&g0.tab=graph&g0.range_input=1h&g0.res_type=auto&g0.res_density=medium&g0.display_mode=lines&g0.show_exemplars=0
[disk usage]:
  http://localhost:9090/query?g0.expr=disk_used_percent&g0.show_tree=0&g0.tab=graph&g0.range_input=1h&g0.res_type=auto&g0.res_density=medium&g0.display_mode=lines&g0.show_exemplars=0
